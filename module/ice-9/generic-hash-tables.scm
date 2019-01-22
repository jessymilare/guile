;;; generic-hash-tables.scm --- Intermediate hash tables

;;    Copyright (C) 2007,2018 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


;;;; Commentary by Jessica Milare 2018

;; This implementation was created on top of SRFI-69 (old) Guile code,
;; extending it to support SRFI-125 specifications and intended to be
;; used by SRFI-69, SRFI-125, SRFI-126 and R6RS. One single hash tables
;; implementation could be exported to all of these libraries, avoiding
;; duplication of code, missing features and incompatibilities.
;;
;; Hash tables here have 2 hash functions, one internal (that is used by
;; Guile) and one external (that is returned by hash-table-hash-function).
;; Internal hash functions accepts two arguments, while external functions
;; accept one argument (and possibly more optional arguments).

;;; Code:

;;;; Module definition & exports

(define-module (ice-9 generic-hash-tables)
  #:use-module (srfi srfi-1)	;alist-cons,second&c,assoc
  #:use-module (srfi srfi-8)    ;receive
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 format)
  #:use-module ((guile)
                #:select ((hash . guile:hash)
                          (make-hash-table . guile:make-hash-table)
                          (assoc . guile:assoc)))
  #:export (;; Type constructors & predicate
	    make-hash-table
            hash-table? hash-table hash-table-unfold alist->hash-table
            ;; Reflective queries
            hash-table-equivalence-function hash-table-hash-function
            hash-table-weakness hash-table-key-weakness hash-table-value-weakness
            ;; Predicates
            hash-table-mutable? hash-table-contains?
            hash-table-empty? hash-table=?
            ;; Accessors
            hash-table-ref hash-table-ref/default
            ;; Mutators
            hash-table-set! hash-table-set-single!
            hash-table-delete! hash-table-delete-single!
            hash-table-intern! hash-table-intern!/default
            hash-table-update! hash-table-update!/default
            hash-table-pop! hash-table-clear!
            ;; The whole hash table
            hash-table-size hash-table-find hash-table-count
            hash-table-keys hash-table-values hash-table-entries
            hash-table-key-vector hash-table-value-vector hash-table-entry-vectors
            ;; Mapping and folding
            hash-table-map hash-table-for-each hash-table-map! hash-table-map->list
            hash-table-fold hash-table-prune!
            ;; Copying
            hash-table-copy hash-table-empty-copy
            ;; Conversion
            hash-table->alist
            ;; Hash tables as sets
            hash-table-union! hash-table-intersection! hash-table-difference!
            hash-table-xor!
            ;; Hashing
            string-ci-hash hash-by-identity hash-by-value hash)
  #:re-export (string-hash)
  #:replace (hash make-hash-table hash-table?))


;;;; Internal helper macros

;; Define these first, so the compiler will pick them up.

;; I am a macro only for efficiency, to avoid varargs/apply.
(define-macro (hashx-invoke hashx-proc hash-table-var . args)
  "Invoke HASHX-PROC, a `hashx-*' procedure taking a hash-function,
assoc-function, and the hash-table as first args."
  `(,hashx-proc (hash-table-internal-hash-function ,hash-table-var)
		(hash-table-associator ,hash-table-var)
		(hash-table-real-table ,hash-table-var)
		. ,args))

(define-macro (with-hashx-values bindings hash-table-var . body-forms)
  "Bind BINDINGS to the hash-function, associator, and real-table of
HASH-TABLE-VAR, while evaluating BODY-FORMS."
  `(let ((,(third bindings) (hash-table-real-table ,hash-table-var))
         (,(first bindings) (hash-table-internal-hash-function ,hash-table-var))
	 (,(second bindings) (hash-table-associator ,hash-table-var)))
     . ,body-forms))

(define-syntax assert-mutable
  (syntax-rules ()
    ((assert-mutable hash-table)
     (or (hash-table-mutable? hash-table)
         (error "Hash table is not mutable" hash-table)))))


;;;; Hashing

;;; SRFI-125 and R6RS hash functions are supposed to accept only one
;;; argument, but Guile standard hash tables needs two args.
;;; Therefore, the hash functions inside the hash table always accepts
;;; one (required) argument and at least one possible argument, which
;;; must be a fixnum.

;;; The largest fixnum is in `most-positive-fixnum' in module (guile),
;;; though not documented anywhere but libguile/numbers.c.

(define (caller-with-default-size hash-function)
  "Answer a function that makes `most-positive-fixnum' the default
second argument to HASH-FUNCTION, a 2-arg procedure."
  (lambda* (obj #:optional (size most-positive-fixnum))
    (hash-function obj size)))

(define hash (caller-with-default-size guile:hash))

(define string-ci-hash string-hash-ci)

(define hash-by-identity (caller-with-default-size hashq))
(define hash-by-value    (caller-with-default-size hashv))

(define (wrap-hash-function hash-function)
  (lambda* (obj size)
    (modulo (hash-function obj) size)))

(define (maybe-wrap-hash-function hash-function)
  (cond
   ;; SRFI-69 should give HASH as default hash-function.
   ((or (eq? guile:hash hash-function)
        (eq? hash hash-function))
    guile:hash)
   ;; These procedures don't need to be wrapped.
   ((or (eq? string-hash hash-function)
        (eq? string-ci-hash hash-function))
    hash-function)
   ((or (eq? hashq hash-function)
        (eq? hash-by-identity hash-function))
    hashq)
   ((or (eq? hashv hash-function)
        (eq? hash-by-value hash-function))
    hashv)
   ;; Otherwise, wrap the given function.
   (else (wrap-hash-function hash-function))))


;;;; Reflective queries, construction, predicate

(define (get-hash-functions equiv-function hash-function)
  "Returns an internal and an external hash function."
  (cond
   (hash-function (values (maybe-wrap-hash-function hash-function)
                          hash-function))
   ((eq? equal? equiv-function) (values guile:hash hash))
   ((eq? eq? equiv-function) (values hashq hash-by-identity))
   ((eq? eqv? equiv-function) (values hashv hash-by-value))
   ((eq? string=? equiv-function) (values string-hash string-hash))
   ((eq? string-ci=? equiv-function) (values string-ci-hash string-ci-hash))
   (else (error "A suitable hash function could not be determined" equiv-function))))

(define (guile-hash-table-constructor weakness)
  "Answer the Guile HASH-TABLE constructor for the given WEAKNESS, where
WEAKNESS is as specified by SRFI-126."
  (case weakness
    ((#f) guile:make-hash-table)
    ((weak-key) make-weak-key-hash-table)
    ((weak-value) make-weak-value-hash-table)
    ((weak-key-and-value) make-doubly-weak-hash-table)
    ((ephemeral-key ephemeral-value ephemeral-key-and-value)
     (error "Unsupported hash table weakness" weakness))
    (else (error "Invalid hash table weakness" weakness))))

(define (equivalence-proc->associator equiv-function)
  "Answer an `assoc'-like procedure that compares the argument key to
alist keys with EQUIV-FUNCTION."
  (cond ((or (eq? equal? equiv-function)
	     (eq? string=? equiv-function)) guile:assoc)
	((eq? eq? equiv-function) assq)
	((eq? eqv? equiv-function) assv)
	(else (lambda (item alist)
		(assoc item alist equiv-function)))))

(define-record-type generic-hash-table
  (make-generic-hash-table real-table internal-hash-function associator weakness
                           mutable? equivalence-function hash-function)
  hash-table?
  ;; These three are the most accessed fields.
  (real-table hash-table-real-table hash-table-real-table!)
  (internal-hash-function hash-table-internal-hash-function)
  (associator hash-table-associator)
  ;; Weak hash tables don't use handles.
  (weakness hash-table-weakness)
  ;; Supports immutability.
  (mutable? hash-table-mutable?)
  ;; These are mostly needed for reflective queries
  (equivalence-function hash-table-equivalence-function)
  (hash-function hash-table-hash-function))

;; Show some informations.
(define (print-hash-table hash-table port)
  (let ((equiv-name (procedure-name (hash-table-equivalence-function hash-table))))
    (format port "#<generic-hash-table ~@[~a ~]~@[~a ~]size: ~a mutable? ~a>"
            equiv-name (hash-table-weakness hash-table)
            (hash-table-size hash-table) (hash-table-mutable? hash-table))))

(set-record-type-printer! generic-hash-table print-hash-table)

(define (hash-table-key-weakness hash-table)
  "Returns weak-keys if HASH-TABLE has weak keys, or #f otherwise."
  ;; If Guile ever supports ephemeral keys, this procedure should
  ;; return 'ephemeral-keys if the HASH-TABLE keys are ephemeral.
  (case (hash-table-weakness hash-table)
    ((#f weak-value) #f)
    ((weak-key weak-key-and-value) 'weak-keys)))

(define (hash-table-value-weakness hash-table)
  "Returns weak-values if HASH-TABLE has weak values, or #f otherwise."
  ;; If Guile ever supports ephemeral values, this procedure should
  ;; return 'ephemeral-values if the HASH-TABLE values are ephemeral.
  (case (hash-table-weakness hash-table)
    ((#f weak-key) #f)
    ((weak-value weak-key-and-value) 'weak-values)))

;; This internal function allows us to create immutable hash tables
(define (%make-hash-table equiv-function hash-function mutable capacity weakness)
  (receive (internal-hash-function hash-function)
      (get-hash-functions equiv-function hash-function)
    (let ((real-table ((guile-hash-table-constructor weakness) capacity)))
      ;; Arguments: real-table internal-hash-function associator
      ;;            weakness mutable? equivalence-function hash-function
      (make-generic-hash-table real-table internal-hash-function
                               (equivalence-proc->associator equiv-function)
                               weakness (and mutable #t)
                               equiv-function hash-function))))

;; If the list of arguments is updated, HASH-TABLE, ALIST->HASH-TABLE,
;; HASH-TABLE-UNFOLD and HASH-TABLE-MAP should be updated as well.
(define* (make-hash-table equiv-function hash-function
                          #:key (capacity 1) (weakness #f) #:rest args)
  "Creates a new hash table. EQUIV-FUNCTION is used as the comparison
function and HASH-FUNCTION, if not false, is used as the hash function,
otherwise a suitable hash-function for the hash table is guessed or, if
one can't be guessed, an error is signaled. #:WEAKNESS should be either
#f, WEAK-KEY, WEAK-VALUE or WEAK-KEY-AND-VALUE, as specified by
SRFI-126. #:CAPACITY is the minimal number of buckets of the hash table.

ARGS is ignored and reserved for future extensions."
  (%make-hash-table equiv-function hash-function #t capacity weakness))

;; We use this to denote missing or unspecified values to avoid
;; possible collision with *unspecified*.

(define hash-table-unspecified (cons *unspecified* "hash-table-value"))

(define (%make-hash-table-with-args equiv-function-or-mht-args capacity)
  (if (pair? equiv-function-or-mht-args)
      (cond ((null? equiv-function-or-mht-args)
             ;; SRFI-125 Spec says this should return an immutable hash table
             (%make-hash-table equal? #f #f capacity #f))
            ((null? (cdr equiv-function-or-mht-args))
             (%make-hash-table (car equiv-function-or-mht-args) #f
                               #f capacity #f))
            ((null? (cddr equiv-function-or-mht-args))
             (%make-hash-table (car equiv-function-or-mht-args)
                               (cadr equiv-function-or-mht-args)
                               #f capacity #f))
            (else
             (apply (lambda* (equiv-function
                              hash-function
                              #:key (capacity capacity) (mutable #f) (weakness #f)
                              #:rest args)
                      (%make-hash-table equiv-function hash-function
                                        mutable capacity weakness))
                    (car equiv-function-or-mht-args)
                    (cadr equiv-function-or-mht-args)
                    #:capacity capacity
                    (cddr equiv-function-or-mht-args))))
      (%make-hash-table equiv-function-or-mht-args #f #f capacity #f)))

(define* (hash-table equiv-function-or-mht-args . args)
  "Creates a new immutable hash table with the associations given by
ARGS. If EQUIV-FUNCTION-OR-MHT-ARGS is a list, the new hash table is
created by (APPLY MAKE-HASH-TABLE EQUIV-FUNCTION-OR-MHT-ARGS), otherwise
it is created by (MAKE-HASH-TABLE EQUIV-FUNCTION-OR-MHT-ARGS) with the
initial capacity set to the number of associations in args.

The ARGS alternate between keys and values. If the same key (in the
sense of the equality procedure) is specified more then once, an error
is signaled."
  (let ((len (length args)))
    (unless (even? len)
      (error "Odd number of key-value pairs" args))
    (let* ((capacity (quotient len 2))
           (hash-table (%make-hash-table-with-args equiv-function-or-mht-args capacity)))
      (with-hashx-values (h a real-table) hash-table
        (if (hash-table-weakness hash-table)
            (let loop ((kvs args))
              (cond
               ((null? kvs) #f)
               ((not (eq? hash-table-unspecified
                          (hashx-ref h a real-table (car kvs) hash-table-unspecified)))
                (error "Two equivalent keys were provided"
                       (car (member (car kvs) (hash-table-keys hash-table)
                                    (hash-table-equivalence-function hash-table)))
                       (car kvs)))
               (else (hashx-set! h a real-table (car kvs) (cadr kvs))
                     (loop (cddr kvs)))))
            (let loop ((kvs args))
              (cond
               ((null? kvs) #f)
               (else (let ((handle (hashx-create-handle! h a real-table
                                                         (car kvs) hash-table-unspecified)))
                       (unless (eq? hash-table-unspecified (cdr handle))
                         (error "Two equivalent keys were provided"
                                (car handle) (car kvs)))
                       (set-cdr! handle (cadr kvs)))
                     (loop (cddr kvs)))))))
      hash-table)))

(define* (hash-table-unfold stop? mapper successor seed
                            equiv-function hash-function
                            #:key (mutable #t) (weakness #f) (capacity 1) #:rest args)
  "Returns a new hash table created by MAKE-HASH-TABLE with the given
arguments. If the result of applying the predicate STOP? to SEED is
true, returns the hash table. Otherwise, apply the procedure MAPPER to
SEED. MAPPER returns two values, which are inserted into the hash table
as the key and the value respectively. Then get a new seed by applying
the procedure SUCCESSOR to SEED, and repeat this algorithm."
  (let ((result (%make-hash-table equiv-function hash-function
                                  mutable capacity weakness)))
    (with-hashx-values (h a real-table) result
      (let loop ((seed seed))
        (if (stop? seed)
            result
            (receive (key val) (mapper seed)
              (hashx-set! h a real-table key val)
              (loop (successor seed))))))
    result))

(define* (alist->hash-table alist equiv-function hash-function
                            #:key (mutable #t) (capacity (length alist)) (weakness #f)
                            #:rest args)
  "Returns a new hash-table created by MAKE-HASH-TABLE with the given
arguments. It is initialized from the associations of alist.
Associations earlier in the list take precedence over those that
come later."
  (let ((result (%make-hash-table equiv-function hash-function
                                  mutable capacity weakness)))
    (with-hashx-values (h a real-table) result
      (for-each (lambda (pair)
                  (hashx-set! h a real-table (car pair) (cdr pair)))
                (reverse alist)))
    result))


;;;; Accessing table items

(define* (hash-table-ref hash-table key #:optional
                         (failure (lambda () (error "Key not in table" key hash-table)))
                         success)
  "Extracts the value associated to KEY in HASH-TABLE, invokes the
procedure SUCCESS on it, and returns its result; if SUCCESS is not
provided, then the value itself is returned. If key is not contained in
hash-table and FAILURE is supplied, then FAILURE is invoked on no
arguments and its result is returned. Otherwise, an error is signaled."
  (let ((result (hashx-invoke hashx-ref hash-table key hash-table-unspecified)))
    (if (eq? hash-table-unspecified result)
        (failure)
        (if success (success result) result))))

(define (hash-table-ref/default hash-table key default)
  "Lookups KEY in HASH-TABLE and returns the associated value. Returns DEFAULT if
KEY isn't present."
  (hashx-invoke hashx-ref hash-table key default))


;;; Predicates.

;; (define (hash-table? obj))

(define (hash-table-empty? hash-table)
  "Returns whether KEY is empty."
  (zero? (hash-n-items (hash-table-real-table hash-table))))

(define (hash-table-contains? hash-table key)
  "Return whether KEY is a key in HASH-TABLE."
  (not (eq? hash-table-unspecified (hashx-invoke hashx-ref hash-table key hash-table-unspecified))))

(define (hash-table=? val=? hash-table1 hash-table2)
  "Returns #t if the hash tables HASH-TABLE1 and HASH-TABLE2 have the
same keys (in the sense of their common equality predicate) and each key
has the same value (in the sense of VAL=?), and #f otherwise."
  (let ((n1 (hash-table-size hash-table1))
        (n2 (hash-table-size hash-table2)))
    (and (= n1 n2)
         (eq? (hash-table-equivalence-function hash-table1)
              (hash-table-equivalence-function hash-table2))
         (receive (keys vals) (hash-table-entries hash-table1)
           (every (lambda (key val1)
                    (and (hash-table-contains? hash-table2 key)
                         (val=? val1 (hash-table-ref hash-table2 key))))
                  keys vals)))))


;;; Mutators.

(define (hash-table-set-single! hash-table key val)
  "If HASH-TABLE is immutable, an error is signaled. Otherwise, a new
association is created between KEY and VAL. If there is a previous
association for KEY, it is deleted."
  (assert-mutable hash-table)
  (with-hashx-values (h a real-table) hash-table
    (hashx-set! h a real-table key val))
  *unspecified*)

(define* (hash-table-set! hash-table #:optional (key1 hash-table-unspecified) (val1 hash-table-unspecified)
                          #:rest args)
  "If HASH-TABLE is immutable, an error is signaled. Otherwise,
repeatedly mutates the hash table HASH-TABLE, creating new associations
in it by processing the arguments from left to right. The ARGS alternate
between keys and values. Whenever there is a previous association for a
key, it is deleted."
  (if (null? args)
      (if (eq? val1 hash-table-unspecified)
          (if (eq? key1 hash-table-unspecified)
              ;; If one calls (hash-table-set! hash-table) with an
              ;; immutable hash table, something is really wrong.
              (assert-mutable hash-table)
              (error "No value provided for key" key1))
          (hash-table-set-single! hash-table key1 val1))
      (begin
        (assert-mutable hash-table)
        (with-hashx-values (h a real-table) hash-table
          (hashx-set! h a real-table key1 val1)
          (let loop ((kvs args))
            (cond
             ((null? kvs) *unspecified*)
             ((null? (cdr kvs))
              (error "Odd number of key-value pairs"
                     (cons* key1 val1 args)))
             (else (hashx-set! h a real-table (car kvs) (cadr kvs))
                   (loop (cddr kvs)))))))))

(define (hash-table-delete-single! hash-table key)
  "Deletes KEY and associated value in hash table HASH-TABLE. Returns #t
if KEY had an association and #f otherwise."
  (assert-mutable hash-table)
  (with-hashx-values (h a real-table) hash-table
    (if (eq? hash-table-unspecified (hashx-ref h a real-table key hash-table-unspecified))
        #f
        (begin (hashx-remove! h a real-table key)
               #t))))

(define* (hash-table-delete! hash-table #:optional (key1 hash-table-unspecified)
                             #:rest keys)
  "Deletes any association to each key in hash table HASH-TABLE and
returns the number of keys that had associations."
  (if (null? keys)
      (if (eq? hash-table-unspecified key1)
          (begin (assert-mutable hash-table) 0)
          (if (hash-table-delete-single! hash-table key1) 1 0))
      (begin
        (assert-mutable hash-table)
        (with-hashx-values (h a real-table) hash-table
          (let* ((count 0)
                 (delete-one! (lambda (key)
                                (when (not (eq? hash-table-unspecified
                                                (hashx-ref h a real-table key
                                                           hash-table-unspecified)))
                                  (set! count (+ 1 count))
                                  (hashx-remove! h a real-table key)))))
            (delete-one! key1)
            (for-each delete-one! keys)
            count)))))

(define (hash-table-intern! hash-table key failure)
  "Effectively invokes HASH-TABLE-REF with the given arguments and
returns what it returns. If KEY was not found in hash-table, its value
is set to the result of calling FAILURE and the new value is returned."
  (assert-mutable hash-table)
  (with-hashx-values (h a real-table) hash-table
    (if (hash-table-weakness hash-table)
        ;; Separate the case where hash-table is weak - don't use handle
        (let ((value (hashx-ref h a real-table key hash-table-unspecified)))
          (cond ((eq? hash-table-unspecified value)
                 (let ((value (failure)))
                   (hashx-set! h a real-table key value)
                   value))
                (else value)))
        (let ((handle (hashx-create-handle! h a real-table key hash-table-unspecified)))
          (if (eq? hash-table-unspecified (cdr handle))
              (set-cdr! handle (failure)))
          (cdr handle)))))

(define (hash-table-intern!/default hash-table key default)
  "Effectively invokes HASH-TABLE-REF with the given arguments and
returns what it returns. If KEY was not found in hash-table, its value
is set to DEFAULT and DEFAULT is returned."
  (assert-mutable hash-table)
  (with-hashx-values (h a real-table) hash-table
    (if (hash-table-weakness hash-table)
        ;; Separate the case where hash-table is weak - don't use handle
        (let ((value (hashx-ref h a real-table key hash-table-unspecified)))
          (cond ((eq? hash-table-unspecified value)
                 (hashx-set! h a real-table key default)
                 default)
                (else value)))
        (let ((handle (hashx-create-handle! h a real-table key default)))
          (cdr handle)))))

(define* (hash-table-update! hash-table key updater #:optional
                             (failure (lambda () (error "Key not in table"
                                                        key hash-table)))
                             success)
  "Semantically equivalent to, but may be more efficient than, the
following code:
 (HASH-TABLE-SET! HASH-TABLE KEY
                  (UPDATER (HASH-TABLE-REF HASH-TABLE KEY
                                           [FAILURE [SUCCESS]])))

Signals an error if HASH-TABLE is immutable. Otherwise, if KEY is found
in hash table HASH-TABLE, its associated VALUE is set
to (UPDATER (SUCCESS VALUE)), or (UPDATER VALUE) if SUCCESS isn't
provided. If KEY is not found, sets the new value to the result
of (UPDATER (FAILURE)) if FAILURE is provided, or signals an error
otherwise."
  (assert-mutable hash-table)
  (with-hashx-values (h a real-table) hash-table
    (if (hash-table-weakness hash-table)
        ;; Separate the case where hash-table is weak - don't use handle
        (let* ((old (hashx-ref h a real-table key hash-table-unspecified))
               (new (updater (if (eq? old hash-table-unspecified)
                                 (failure)
                                 (if success (success old) old)))))
          (hashx-set! h a real-table key new))
        (let ((handle (hashx-get-handle h a real-table key)))
          (cond (handle
                 (let* ((old (cdr handle))
                        (new (updater (if success (success old) old))))
                   (set-cdr! handle new)))
                (else
                 (let ((new (updater (failure))))
                   (hashx-set! h a real-table key new)))))))
  *unspecified*)

(define (hash-table-update!/default hash-table key updater default)
  "Semantically equivalent to, but may be more efficient than, the
following code:
 (HASH-TABLE-SET! HASH-TABLE KEY
                  (UPDATER (HASH-TABLE-REF/DEFAULT HASH-TABLE KEY DEFAULT)))

Signals an error if HASH-TABLE is immutable. Otherwise, modifies HASH-TABLE's value at
KEY by passing its old value, or DEFAULT if it doesn't have one, to
UPDATER, and setting it to the result thereof."
  (assert-mutable hash-table)
  (with-hashx-values (h a real-table) hash-table
    (if (hash-table-weakness hash-table)
        ;; J.M. separate the case where hash-table is weak - don't use handle
        (let ((old (hashx-ref h a real-table key default)))
          (hashx-set! h a real-table key (updater old)))
        (let ((handle (hashx-create-handle! h a real-table key default)))
          (set-cdr! handle (updater (cdr handle))))))
  *unspecified*)

(define (hash-table-pop! hash-table)
  "Signals an error if HASH-TABLE is immutable or empty. Otherwise, chooses an
arbitrary association from hash-table and removes it, returning the key
and value as two values."
  (assert-mutable hash-table)
  (call/cc
   (lambda (return)
     (with-hashx-values (h a real-table) hash-table
       (hash-for-each (lambda (key value)
                        (hashx-remove! h a real-table key)
                        (return key value))
                      real-table))
     (error "Hash table is empty" hash-table))))

(define* (hash-table-clear! hash-table #:optional capacity)
  "Deletes all associations from HASH-TABLE."
  (assert-mutable hash-table)
  (if capacity
      (let* ((weakness (hash-table-weakness hash-table))
             (new-real-table ((guile-hash-table-constructor weakness) capacity)))
        (hash-table-real-table! hash-table new-real-table))
      (hash-clear! (hash-table-real-table hash-table)))
  *unspecified*)


;; The whole hash table.

(define (hash-table-size hash-table)
  "Returns the number of associations in HASH-TABLE. This is guaranteed O(1) for
tables where #:WEAKNESS is #f."
  (hash-n-items (hash-table-real-table hash-table)))

(define (hash-table-keys hash-table)
  "Returns a list of the keys in HASH-TABLE."
  (hash-fold (lambda (key val lst) (cons key lst))
             '() (hash-table-real-table hash-table)))

(define (hash-table-values hash-table)
  "Returns a list of the values in HASH-TABLE."
  (hash-fold (lambda (key val lst) (cons val lst))
             '() (hash-table-real-table hash-table)))

(define (hash-table-entries hash-table)
  "Returns two values: a list of the keys and a list of the associated
values in the corresponding order."
  (let ((keys '()) (vals '()))
    (hash-for-each (lambda (key val)
                     (set! keys (cons key keys))
                     (set! vals (cons val vals)))
                   (hash-table-real-table hash-table))
    (values keys vals)))

(define (hash-table-key-vector hash-table)
  "Returns a vector of the keys in HASH-TABLE."
  (let* ((len (hash-table-size hash-table))
         (keys (make-vector len))
         ;; In a weak hash table, some values might get gargabe
         ;; collected while the procedure is running, so we double-check
         ;; if we collected the expected number of keys.
         (new-len (hash-fold (lambda (key val i)
                               (vector-set! keys i key)
                               (+ i 1))
                             0 (hash-table-real-table hash-table))))
    (if (< new-len len)
        (let ((new-keys (make-vector new-len)))
          (vector-move-left! keys 0 new-len new-keys 0)
          new-keys)
        keys)))

(define (hash-table-value-vector hash-table)
  "Returns a vector of the values in HASH-TABLE."
  (let* ((len (hash-table-size hash-table))
         (vals (make-vector len))
         ;; In a weak hash table, some values might get gargabe
         ;; collected while the procedure is running, so we double-check
         ;; if we collected the expected number of keys.
         (new-len (hash-fold (lambda (key val i)
                               (vector-set! vals i val)
                               (+ i 1))
                             0 (hash-table-real-table hash-table))))
    (if (< new-len len)
        (let ((new-vals (make-vector new-len)))
          (vector-move-left! vals 0 new-len new-vals 0)
          new-vals)
        vals)))

(define (hash-table-entry-vectors hash-table)
  "Returns two values: a vector of the keys and a vector of the
associated values in the corresponding order."
  (let* ((len (hash-table-size hash-table))
         (keys (make-vector len))
         (vals (make-vector len))
         ;; In a weak hash table, some values might get gargabe
         ;; collected while the procedure is running, so we double-check
         ;; if we collected the expected number of keys.
         (new-len (hash-fold (lambda (key val i)
                               (vector-set! keys i key)
                               (vector-set! vals i val)
                               (+ i 1))
                             0 (hash-table-real-table hash-table))))
    (if (< new-len len)
        (let ((new-keys (make-vector new-len))
              (new-vals (make-vector new-len)))
          (vector-move-left! keys 0 new-len new-keys 0)
          (vector-move-left! vals 0 new-len new-vals 0)
          (values new-keys new-vals))
        (values keys vals))))

(define (hash-table-find proc hash-table failure)
  "For each association of the hash table HASH-TABLE, invoke PROC on its
key and value. If PROC returns true, then HASH-TABLE-FIND returns what
PROC returns. If all the calls to PROC return #f, returns the result of
invoking the thunk FAILURE."
  (call/cc (lambda (return)
             (hash-for-each (lambda (key val)
                              (let ((x (proc key val)))
                                (if x (return x))))
                            (hash-table-real-table hash-table))
             (failure))))

(define (hash-table-count pred hash-table)
  "For each association of HASH-TABLE, invoke PRED on its key and
value. Return the number of calls to PRED which returned true."
  (hash-fold (lambda (key val n)
               (if (pred key val) (+ 1 n) n))
             0 (hash-table-real-table hash-table)))


;;; Mapping and folding.

(define* (hash-table-map proc hash-table equiv-function hash-function
                         #:key (mutable #t) (capacity (hash-table-size hash-table))
                         (weakness #f)
                         #:rest args)
  "Creates a new hash table by calling MAKE-HASH-TABLE with the given
arguments. After creation, HASH-TABLE-MAP calls PROC for every
association in hash-table with the value of the association. The key of
the association and the result of invoking PROC are entered into the new
hash table, which is then returned."
  (let ((result (%make-hash-table equiv-function hash-function
                                  mutable capacity weakness)))
    (with-hashx-values (h a real-table) result
      (hash-for-each
       (lambda (key val)
         (hashx-set! h a real-table key (proc val)))
       (hash-table-real-table hash-table)))
    result))

(define (hash-table-map->list proc hash-table)
  "Calls PROC for every association in HASH-TABLE with two arguments:
the key of the association and the value of the association. The values
returned by the invocations of PROC are accumulated into a list, which
is returned."
  (hash-map->list proc (hash-table-real-table hash-table)))

;;; With this particular implementation, the proc can safely mutate hash-table.
;;; That property is not guaranteed by the specification, but can be
;;; relied upon by procedures defined in this file.

(define (hash-table-for-each proc hash-table)
  "Calls PROC with each key and value as two arguments. Returns an
unspecified value."
  (hash-for-each proc (hash-table-real-table hash-table)))

(define (hash-table-map! proc hash-table)
  "Signals an error if HASH-TABLE is immutable. Otherwise, calls PROC
for every association in HASH-TABLE with two arguments: the key of the
association and the value of the association. The value returned by PROC
is used to update the value of the association. Return an unspecified
value."
  (assert-mutable hash-table)
  (if (hash-table-weakness hash-table)
      (with-hashx-values (h a real-table) hash-table
        (hash-for-each (lambda (key val)
                         (hashx-set! h a real-table key (proc key val)))
                       real-table))
      (let ((real-table (hash-table-real-table hash-table)))
        (hash-for-each-handle (lambda (handle)
                                (let ((key (car handle))
                                      (val (cdr handle)))
                                  (set-cdr! handle (proc key val))))
                              real-table))))

(define (hash-table-fold proc init hash-table)
  "Calls PROC for every association in HASH-TABLE with three arguments:
the key of the association, the value of the association, and an
accumulated value VAL. VAL is SEED for the first invocation of
procedure, and for subsequent invocations of PROC, the returned value of
the previous invocation. The value returned by HASH-TABLE-FOLD is the
return value of the last invocation of PROC."
  (hash-fold proc init (hash-table-real-table hash-table)))

(define (hash-table-prune! proc hash-table)
  "If HASH-TABLE is immutable, signals an error. Otherwise, calls PROC
for every association in hash-table with two arguments, the key and the
value of the association, and removes all associations from hash-table
for which PROC returns true. Returns an unspecified value."
  (assert-mutable hash-table)
  (with-hashx-values (h a real-table) hash-table
    (hash-for-each (lambda (key val)
                     (when (proc key val)
                       (hashx-remove! h a real-table key)))
                   real-table)))


;;; Copying and conversion.

(define* (hash-table-copy hash-table
                          #:key (mutable (hash-table-mutable? hash-table))
                          (capacity (hash-table-size hash-table))
                          (weakness (hash-table-weakness hash-table)))
  "Returns a newly allocated hash table with the associations as
HASH-TABLE and properties as given by keyword arguments, which default
to HASH-TABLE properties. If MUTABLE is true, the new hash table is
mutable, otherwise, it is immutable."
  (with-hashx-values (h a real-table) hash-table
    (let ((new-real-table ((guile-hash-table-constructor weakness) capacity)))
      (hash-for-each (lambda (key val)
                       (hashx-set! h a new-real-table key val))
                     real-table)
      ;; Arguments: real-table internal-hash-function associator
      ;;            weakness mutable? equivalence-function hash-function
      (make-generic-hash-table new-real-table h a weakness (and mutable #t)
                               (hash-table-equivalence-function hash-table)
                               (hash-table-hash-function hash-table)))))

(define* (hash-table-empty-copy hash-table #:key (mutable #t)
                                (capacity 1)
                                (weakness (hash-table-weakness hash-table)))
  "Returns a newly allocated mutable hash table with the same properties
as HASH-TABLE, but with no associations."
  (with-hashx-values (h a real-table) hash-table
    (let ((new-real-table ((guile-hash-table-constructor weakness) capacity)))
      ;; Arguments: real-table internal-hash-function associator
      ;;            weakness mutable? equivalence-function hash-function
      (make-generic-hash-table new-real-table h a weakness (and mutable #t)
                               (hash-table-equivalence-function hash-table)
                               (hash-table-hash-function hash-table)))))

(define (hash-table->alist hash-table)
  "Returns an alist with the same associations as hash-table in an
unspecified order."
  (hash-map->list cons (hash-table-real-table hash-table)))


;;; Hash tables as sets.

(define (hash-table-union! hash-table1 hash-table2)
  "If HASH-TABLE1 is immutable, signals an error. Otherwise, adds the
associations of HASH-TABLE2 to HASH-TABLE1 and return HASH-TABLE1. If a
key appears in both hash tables, its value is set to the value appearing
in HASH-TABLE1."
  (assert-mutable hash-table1)
  (hash-for-each (lambda (key2 val2)
                   (hash-table-intern!/default hash-table1 key2 val2))
                 (hash-table-real-table hash-table2))
  hash-table1)

(define (hash-table-intersection! hash-table1 hash-table2)
  "If HASH-TABLE1 is immutable, signals an error. Otherwise, deletes the
associations from HASH-TABLE whose keys don't also appear in HASH-TABLE2
and returns HASH-TABLE1."
  (assert-mutable hash-table1)
  (hash-for-each (lambda (key1 val1)
                   (if (not (hash-table-contains? hash-table2 key1))
                       (hash-table-delete! hash-table1 key1)))
                 (hash-table-real-table hash-table1))
  hash-table1)

(define (hash-table-difference! hash-table1 hash-table2)
  "If HASH-TABLE1 is immutable, signals an error. Otherwise, deletes the
associations of HASH-TABLE1 whose keys are also present in HASH-TABLE2
and returns HASH-TABLE1."
  (assert-mutable hash-table1)
  (hash-for-each (lambda (key1 val1)
                   (if (hash-table-contains? hash-table2 key1)
                       (hash-table-delete! hash-table1 key1)))
                 (hash-table-real-table hash-table1))
  hash-table1)

(define (hash-table-xor! hash-table1 hash-table2)
  "If HASH-TABLE1 is immutable, signals an error. Otherwise, deletes the
associations of HASH-TABLE1 whose keys are also present in HASH-TABLE2,
and then adds the associations of HASH-TABLE2 whose keys are not present
in HASH-TABLE1 to HASH-TABLE1. Returns HASH-TABLE1."
  (assert-mutable hash-table1)
  (hash-for-each (lambda (key2 val2)
                   (if (hash-table-contains? hash-table1 key2)
                       (hash-table-delete! hash-table1 key2)
                       (hash-table-set! hash-table1 key2 val2)))
                 (hash-table-real-table hash-table2))
  hash-table1)

;; eof
