;;; srfi-69.scm --- Basic hash tables

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


;;;; Original (stale) SRFI-69 commentary:

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

;; Now implemented using module (ice-9 generic-hash-tables)
;;
;; My personal comments are marked by J.M.

;;; Code:

;;;; Module definition & exports

(define-module (srfi srfi-69)
  #:use-module (srfi srfi-1)
  #:use-module ((ice-9 generic-hash-tables) #:prefix gen:)
  #:replace (make-hash-table hash-table? hash)
  #:export (;; Type constructors
            make-hash-table
            hash-table? alist->hash-table
            ;; Reflective queries
            hash-table-equivalence-function hash-table-hash-function
            ;; Dealing with single elements
            hash-table-ref hash-table-ref/default hash-table-set!
            hash-table-delete! hash-table-update!
            hash-table-update!/default
            hash-table-exists?
            ;; Dealing with the whole contents
            hash-table-size hash-table-keys hash-table-values
            hash-table->alist hash-table-fold
            hash-table-walk hash-table-merge!
            hash-table-copy
            ;; Hashing
            string-ci-hash hash-by-identity hash)
  #:re-export (string-hash))

(cond-expand-provide (current-module) '(srfi-69))

;;;; Hashing
(define string-ci-hash string-hash-ci)
(define hash gen:hash)
(define hash-by-identity gen:hash-by-identity)


;;;; Reflective queries, construction, predicate

(define hash-table? gen:hash-table?)

(define (guess-hash-function equal-proc)
  "Guess a hash function for EQUAL-PROC, falling back on `hash', as
specified in SRFI-69 for `make-hash-table'."
  (cond ((eq? equal? equal-proc) gen:hash) ;shortcut most common case
	((eq? eq? equal-proc) gen:hash-by-identity)
	((eq? eqv? equal-proc) gen:hash-by-value)
	((eq? string=? equal-proc) gen:string-hash)
	((eq? string-ci=? equal-proc) gen:string-ci-hash)
	(else gen:hash)))

(define (normalize-weakness weak)
  "Normalizes SRFI-69 standard #:weak to SRFI-126 weakness argument."
  (case weak
    ((#f) #f)
    ((key) 'weak-key)
    ((value) 'weak-value)
    ((key-or-value) 'weak-key-and-value)
    (else (error "Invalid weak hash table type" weak))))

(define* (make-hash-table #:optional (equal-proc equal?)
                          (hash-proc (guess-hash-function equal-proc))
                          #:key (weak #f) #:rest args)
  "Answer a new hash table using EQUAL-PROC as the comparison
function, and HASH-PROC as the hash function.  See the reference
manual for specifics, of which there are many."
  (let ((capacity (find integer? args)))
    (gen:make-hash-table equal-proc hash-proc
                         #:weakness (normalize-weakness weak)
                         #:capacity (or capacity 1))))

(define hash-table-equivalence-function gen:hash-table-equivalence-function)
(define hash-table-hash-function gen:hash-table-hash-function)
(define* (alist->hash-table alist #:optional (equal-proc equal?)
                            (hash-proc (guess-hash-function equal-proc))
                            #:key (weak #f) #:rest args)
  (let ((capacity (find integer? args)))
    (gen:alist->hash-table alist equal-proc hash-proc
                           #:weakness (normalize-weakness weak)
                           #:capacity (or capacity 1))))


;; Dealing with single elements

(define* (hash-table-ref ht key #:optional failure)
  "Lookup KEY in HT and answer the value, invoke DEFAULT-THUNK if KEY
isn't present, or signal an error if DEFAULT-THUNK isn't provided."
  (if failure
      (gen:hash-table-ref ht key failure)
      (gen:hash-table-ref ht key)))

(define hash-table-ref/default gen:hash-table-ref/default)
(define hash-table-exists? gen:hash-table-contains?)
(define hash-table-set! gen:hash-table-set-single!)
(define (hash-table-delete! ht key)
  (gen:hash-table-delete-single! ht key)
  *unspecified*)

(define* (hash-table-update! ht key modifier #:optional default-thunk)
  "Modify HT's value at KEY by passing its value to MODIFIER and
setting it to the result thereof.  Invoke DEFAULT-THUNK for the old
value if KEY isn't in HT, or signal an error if DEFAULT-THUNK is not
provided."
  (if default-thunk
      (gen:hash-table-update! ht key modifier default-thunk)
      (gen:hash-table-update! ht key modifier)))

(define hash-table-update!/default gen:hash-table-update!/default)


;;;; Accessing whole tables

(define hash-table-size gen:hash-table-size)
(define hash-table-keys gen:hash-table-keys)
(define hash-table-values gen:hash-table-values)
(define hash-table->alist gen:hash-table->alist)

(define (hash-table-walk ht proc)
  "Call PROC with each key and value as two arguments."
  (gen:hash-table-for-each proc ht))

(define (hash-table-fold ht f knil)
  "Invoke (F KEY VAL PREV) for each KEY and VAL in HT, where PREV is
the result of the previous invocation, using KNIL as the first PREV.
Answer the final F result."
  (gen:hash-table-fold f knil ht))

(define (hash-table-copy ht)
  "Answer a copy of HT."
  (gen:hash-table-copy ht))

(define (hash-table-merge! ht other-ht)
  "Add all key/value pairs from OTHER-HT to HT, overriding HT's
mappings where present.  Return HT."
  ;; HASH-TABLE-SET! tests if HT is mutable.
  (gen:hash-table-for-each (lambda (k v) (hash-table-set! ht k v))
                           other-ht)
  ht)

;;; srfi-69.scm ends here
