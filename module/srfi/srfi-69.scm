;;; srfi-69.scm --- Basic hash tables

;; 	Copyright (C) 2007 Free Software Foundation, Inc.
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

;;;; Commentary:

;; My `hash' is compatible with core `hash', so I replace it.
;; However, my `hash-table?' and `make-hash-table' are different, so
;; importing this module will warn about them.  If you don't rename my
;; imports, you shouldn't use both my hash tables and Guile's hash
;; tables in the same module.
;;
;; SRFI-13 `string-hash' and `string-hash-ci' have more arguments, but
;; are compatible with my `string-hash' and `string-ci-hash', and are
;; furthermore primitive in Guile, so I use them as my own.
;;
;; I also have the extension of allowing hash functions that require a
;; second argument to be used as the `hash-table-hash-function', and use
;; these in defaults to avoid an indirection in the hashx functions.  The
;; only deviation this causes is:
;;
;;  ((hash-table-hash-function (make-hash-table)) obj)
;;  error> Wrong number of arguments to #<primitive-procedure hash>
;;
;; I don't think that SRFI 69 actually specifies that I *can't* do this,
;; because it only implies the signature of a hash function by way of the
;; named, exported hash functions.  However, if this matters enough I can
;; add a private derivation of hash-function to the srfi-69:hash-table
;; record type, like associator is to equivalence-function.
;;
;; Also, outside of the issue of how weak keys and values are referenced
;; outside the table, I always interpret key equivalence to be that of
;; the `hash-table-equivalence-function'.  For example, given the
;; requirement that `alist->hash-table' give earlier associations
;; priority, what should these answer?
;;
;;  (hash-table-keys
;;   (alist->hash-table '(("xY" . 1) ("Xy" . 2)) string-ci=?))
;;
;;  (let ((ht (make-hash-table string-ci=?)))
;;    (hash-table-set! ht "xY" 2)
;;    (hash-table-set! ht "Xy" 1)
;;    (hash-table-keys ht))
;;
;; My interpretation is that they can answer either ("Xy") or ("xY"),
;; where `hash-table-values' will of course always answer (1), because
;; the keys are the same according to the equivalence function.  In this
;; implementation, both answer ("xY").  However, I don't guarantee that
;; this won't change in the future.


;;;; Commentary by Jessica Milare 2018

;; Make bug fixes for weak hash-tables, since handles don't work anymore,
;; and also some optimizations.
;;
;; My personal comments are marked by J.M.

;;; Code:

;;;; Module definition & exports

(define-module (srfi srfi-69)
  #:use-module (srfi srfi-1)	;alist-cons,second&c,assoc
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)	;string-hash,string-hash-ci
  #:use-module (ice-9 optargs)
  #:export (;; Type constructors & predicate
	    make-hash-table hash-table? alist->hash-table
            ;; Reflective queries
            hash-table-equivalence-function hash-table-hash-function
            ;; Dealing with single elements
            hash-table-ref hash-table-ref/default hash-table-set!
            hash-table-delete! hash-table-exists? hash-table-update!
            hash-table-update!/default
            ;; Dealing with the whole contents
            hash-table-size hash-table-keys hash-table-values
            hash-table-walk hash-table-fold hash-table->alist
            hash-table-copy hash-table-merge!
            ;; Hashing
            string-ci-hash hash-by-identity)
  #:re-export (string-hash)
  #:replace (hash make-hash-table hash-table?))

(cond-expand-provide (current-module) '(srfi-69))

;;;; Internal helper macros

;; Define these first, so the compiler will pick them up.

;; I am a macro only for efficiency, to avoid varargs/apply.
(define-macro (hashx-invoke hashx-proc ht-var . args)
  "Invoke HASHX-PROC, a `hashx-*' procedure taking a hash-function,
assoc-function, and the hash-table as first args."
  `(,hashx-proc (hash-table-hash-function ,ht-var)
		(ht-associator ,ht-var)
		(ht-real-table ,ht-var)
		. ,args))

(define-macro (with-hashx-values bindings ht-var . body-forms)
  "Bind BINDINGS to the hash-function, associator, and real-table of
HT-VAR, while evaluating BODY-FORMS."
  `(let ((,(first bindings) (hash-table-hash-function ,ht-var))
	 (,(second bindings) (ht-associator ,ht-var))
	 (,(third bindings) (ht-real-table ,ht-var)))
     . ,body-forms))


;;;; Hashing

;;; The largest fixnum is in `most-positive-fixnum' in module (guile),
;;; though not documented anywhere but libguile/numbers.c.

(define (caller-with-default-size hash-fn)
  "Answer a function that makes `most-positive-fixnum' the default
second argument to HASH-FN, a 2-arg procedure."
  (lambda* (obj #:optional (size most-positive-fixnum))
    (hash-fn obj size)))

(define hash (caller-with-default-size (@ (guile) hash)))

(define string-ci-hash string-hash-ci)

(define hash-by-identity (caller-with-default-size hashq))

;;;; Reflective queries, construction, predicate

(define-record-type srfi-69:hash-table
  (make-srfi-69-hash-table real-table associator size weakness
			   equivalence-function hash-function)
  hash-table?
  (real-table ht-real-table)
  (associator ht-associator)
  ;; required for O(1) by SRFI-69.  It really makes a mess of things,
  ;; and I'd like to compute it in O(n) and memoize it because it
  ;; doesn't seem terribly useful, but SRFI-69 is final.
  (size ht-size ht-size!)
  ;; required for `hash-table-copy'
  (weakness ht-weakness)
  ;; used only to implement hash-table-equivalence-function; I don't
  ;; use it internally other than for `ht-associator'.
  (equivalence-function hash-table-equivalence-function)
  (hash-function hash-table-hash-function))

(define (guess-hash-function equal-proc)
  "Guess a hash function for EQUAL-PROC, falling back on `hash', as
specified in SRFI-69 for `make-hash-table'."
  (cond ((eq? equal? equal-proc) (@ (guile) hash)) ;shortcut most common case
	((eq? eq? equal-proc) hashq)
	((eq? eqv? equal-proc) hashv)
	((eq? string=? equal-proc) string-hash)
	((eq? string-ci=? equal-proc) string-ci-hash)
	(else (@ (guile) hash))))

(define (without-keyword-args rest-list)
  "Answer REST-LIST with all keywords removed along with items that
follow them."
  (let lp ((acc '()) (rest-list rest-list))
    (cond ((null? rest-list) (reverse! acc))
	  ((keyword? (first rest-list))
	   (lp acc (cddr rest-list)))
	  (else (lp (cons (first rest-list) acc) (cdr rest-list))))))

(define (guile-ht-ctor weakness)
  "Answer the Guile HT constructor for the given WEAKNESS."
  (case weakness
    ((#f) (@ (guile) make-hash-table))
    ((key) make-weak-key-hash-table)
    ((value) make-weak-value-hash-table)
    ((key-or-value) make-doubly-weak-hash-table)
    (else (error "Invalid weak hash table type" weakness))))

(define (equivalence-proc->associator equal-proc)
  "Answer an `assoc'-like procedure that compares the argument key to
alist keys with EQUAL-PROC."
  (cond ((or (eq? equal? equal-proc)
	     (eq? string=? equal-proc)) (@ (guile) assoc))
	((eq? eq? equal-proc) assq)
	((eq? eqv? equal-proc) assv)
	(else (lambda (item alist)
		(assoc item alist equal-proc)))))

(define* (make-hash-table
	  #:optional (equal-proc equal?)
	  (hash-proc (guess-hash-function equal-proc))
	  #:key (weak #f) #:rest guile-opts)
  "Answer a new hash table using EQUAL-PROC as the comparison
function, and HASH-PROC as the hash function.  See the reference
manual for specifics, of which there are many."
  (make-srfi-69-hash-table
   (apply (guile-ht-ctor weak) (without-keyword-args guile-opts))
   (equivalence-proc->associator equal-proc)
   0 weak equal-proc hash-proc))

(define (alist->hash-table alist . mht-args)
  "Convert ALIST to a hash table created with MHT-ARGS."
  (let* ((result (apply make-hash-table mht-args))
	 (size (ht-size result)))
    (with-hashx-values (hash-proc associator real-table) result
      (for-each (lambda (pair)
                  (let ((value (hashx-ref hash-proc associator
                                          real-table (car pair)
                                          ht-unspecified)))
                    (cond ((eq? ht-unspecified value)
                           (set! size (1+ size))
                           (hashx-set! hash-proc associator real-table
                                       (car pair) (cdr pair))))))
                alist))
    (ht-size! result size)
    result))

;;;; Accessing table items

;; We use this to denote missing or unspecified values to avoid

;; possible collision with *unspecified*.
(define ht-unspecified (cons *unspecified* "ht-value"))

(define* (hash-table-ref ht key  #:optional (default-thunk ht-unspecified))
  "Lookup KEY in HT and answer the value, invoke DEFAULT-THUNK if KEY
isn't present, or signal an error if DEFAULT-THUNK isn't provided."
  (let ((result (hashx-invoke hashx-ref ht key ht-unspecified)))
    (if (eq? ht-unspecified result)
	(if (eq? ht-unspecified default-thunk)
	    (error "Key not in table" key ht)
	    (default-thunk))
	result)))

(define (hash-table-ref/default ht key default)
  "Lookup KEY in HT and answer the value.  Answer DEFAULT if KEY isn't
present."
  (hashx-invoke hashx-ref ht key default))

(define (hash-table-set! ht key new-value)
  "Set KEY to NEW-VALUE in HT."
  (if (ht-weakness ht)
      ;; J.M. separate the case where ht is weak - don't use handle
      ;; J.M. don't need to update size for weak hash-tables
      (hashx-invoke hashx-set! ht key new-value)
      (let ((handle (hashx-invoke hashx-create-handle! ht key
                                  ht-unspecified)))
        (if (eq? ht-unspecified (cdr handle))
            (ht-size! ht (1+ (ht-size ht))))
        (set-cdr! handle new-value)))
  *unspecified*)

(define (hash-table-delete! ht key)
  "Remove KEY's association in HT."
  (with-hashx-values (h a real-ht) ht
    (if (not (eq? ht-unspecified (hashx-ref h a real-ht key ht-unspecified)))
        (begin
          (ht-size! ht (1- (ht-size ht)))
          (hashx-remove! h a real-ht key))))
  *unspecified*)

(define (hash-table-exists? ht key)
  "Return whether KEY is a key in HT."
  (not (eq? ht-unspecified (hashx-invoke hashx-ref ht key ht-unspecified))))

;;; `hash-table-update!' non-locally.
(define* (hash-table-update! ht key modifier
                             #:optional (default-thunk ht-unspecified))
  "Modify HT's value at KEY by passing its value to MODIFIER and
setting it to the result thereof.  Invoke DEFAULT-THUNK for the old
value if KEY isn't in HT, or signal an error if DEFAULT-THUNK is not
provided."
  (with-hashx-values (hash-proc associator real-table) ht
    (if (ht-weakness ht)
        ;; J.M. separate the case where ht is weak - don't use handle
        (let* ((old (hashx-ref hash-proc associator real-table key
                               ht-unspecified)))
          (cond ((eq? ht-unspecified old)
                 (if (eq? ht-unspecified default-thunk)
                     (error "Key not in table" key ht)
                     (hashx-set! hash-proc associator real-table key
                                 (modifier (default-thunk)))))
                (else
                 (hashx-set! hash-proc associator real-table key
                             (modifier old)))))
        (let ((handle (hashx-get-handle hash-proc associator real-table key)))
          (cond (handle (if (eq? ht-unspecified (cdr handle))
                            (begin (ht-size! ht (1+ (ht-size ht)))
                                   (set-cdr! handle (modifier (default-thunk))))
                            (set-cdr! handle (modifier (cdr handle)))))
                (else (if (eq? ht-unspecified default-thunk)
                          (error "Key not in table" key ht)
                          (let ((default (default-thunk)))
                            (ht-size! ht (1+ (ht-size ht)))
                            (hashx-set! hash-proc associator real-table key
                                        (modifier default)))))))))
  *unspecified*)

;;; J.M. Custom implementation instead of using hash-table-update!
(define (hash-table-update!/default ht key modifier default)
  "Modify HT's value at KEY by passing its old value, or DEFAULT if it
doesn't have one, to MODIFIER, and setting it to the result thereof."
  (with-hashx-values (hash-proc associator real-table) ht
    (if (ht-weakness ht)
        ;; J.M. separate the case where ht is weak - don't use handle
        (let* ((old (hashx-ref hash-proc associator real-table key default)))
          (hashx-set! hash-proc associator real-table key (modifier old)))
        (let ((handle (hashx-create-handle! hash-proc associator real-table key
                                            ht-unspecified)))
          (if (eq? ht-unspecified (cdr handle))
              (begin (ht-size! ht (1+ (ht-size ht)))
                     (set-cdr! handle (modifier default)))
              (set-cdr! handle (modifier (cdr handle))))))))

;;;; Accessing whole tables

(define (hash-table-size ht)
  "Return the number of associations in HT.  This is guaranteed O(1)
for tables where #:weak was #f or not specified at creation time."
  (if (ht-weakness ht)
      (let ((size (hash-table-fold ht (lambda (k v ans) (1+ ans)) 0)))
        (ht-size! ht size)
        size)
      (ht-size ht)))

(define (hash-table-keys ht)
  "Return a list of the keys in HT."
  (hash-table-fold ht (lambda (k v lst) (cons k lst)) '()))

(define (hash-table-values ht)
  "Return a list of the values in HT."
  (hash-table-fold ht (lambda (k v lst) (cons v lst)) '()))

(define (hash-table-walk ht proc)
  "Call PROC with each key and value as two arguments."
  (hash-for-each proc (ht-real-table ht)))

(define (hash-table-fold ht f knil)
  "Invoke (F KEY VAL PREV) for each KEY and VAL in HT, where PREV is
the result of the previous invocation, using KNIL as the first PREV.
Answer the final F result."
  (hash-fold f knil (ht-real-table ht)))

(define (hash-table->alist ht)
  "Return an alist for HT."
  (hash-map->list cons (ht-real-table ht)))

(define (hash-table-copy ht)
  "Answer a copy of HT."
  (with-hashx-values (h a real-ht) ht
    (let* ((size (hash-table-size ht)) (weak (ht-weakness ht))
	   (new-real-ht ((guile-ht-ctor weak) size)))
      (hash-for-each (lambda (k v) (hashx-set! h a new-real-ht k v))
                     real-ht)
      (make-srfi-69-hash-table		;real,assoc,size,weak,equiv,h
       new-real-ht a size weak
       (hash-table-equivalence-function ht) h))))

(define (hash-table-merge! ht other-ht)
  "Add all key/value pairs from OTHER-HT to HT, overriding HT's
mappings where present.  Return HT."
  (hash-for-each (lambda (k v) (hash-table-set! ht k v))
                 (ht-real-table other-ht))
  ht)

;;; srfi-69.scm ends here
