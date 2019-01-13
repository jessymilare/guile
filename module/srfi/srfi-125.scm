;;; srfi-125.scm --- Intermediate hash tables

;;    Copyright (C) 2019 Free Software Foundation, Inc.
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

;; This file contains code from SRFI 128 reference implementation, by
;; William D Clinger

;;; Copyright 2015 William D Clinger.
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright and permission notice in full.
;;;
;;; I also request that you send me a copy of any improvements that you
;;; make to this software so that they may be incorporated within it to
;;; the benefit of the Scheme community.


(define-module (srfi srfi-125)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-128)
  #:use-module ((rnrs base) #:select (symbol=?))
  #:use-module ((ice-9 generic-hash-tables) #:prefix gen:)
  #:export (;; Type constructors and predicate
            make-hash-table
            hash-table hash-table-unfold alist->hash-table
            ;; Predicates
            hash-table? hash-table-contains? hash-table-empty? hash-table=?
            hash-table-mutable?
            ;; Accessors
            hash-table-ref hash-table-ref/default
            ;; Mutators
            hash-table-set! hash-table-delete! hash-table-intern! hash-table-update!
            hash-table-update!/default hash-table-pop! hash-table-clear!
            ;; The whole hash table
            hash-table-size hash-table-keys hash-table-values hash-table-entries
            hash-table-find hash-table-count
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
            ;; The following procedures are deprecated by SRFI 125:
            (deprecated:hash-table-exists? . hash-table-exists?)
            (deprecated:hash-table-walk . hash-table-walk)
            (deprecated:hash-table-merge! . hash-table-merge!)
            ;; Fixme: should we really deprecate these in Guile?
            (deprecated:hash . hash)
            (deprecated:string-hash . string-hash)
            (deprecated:string-ci-hash . string-ci-hash)
            (deprecated:hash-by-identity . hash-by-identity)
            (deprecated:hash-table-equivalence-function . hash-table-equivalence-function)
            (deprecated:hash-table-hash-function . hash-table-hash-function))
  #:replace (make-hash-table hash-table?))

(cond-expand-provide (current-module) '(srfi-125))


;;; Private stuff, not exported.

;; Ten of the SRFI 125 procedures are deprecated, and another
;; two allow alternative arguments that are deprecated.

(define (issue-deprecated-warnings?) #t)

(define (issue-warning-deprecated name-of-deprecated-misfeature)
  (if (not (memq name-of-deprecated-misfeature already-warned))
      (begin
        (set! already-warned
          (cons name-of-deprecated-misfeature already-warned))
        (if (issue-deprecated-warnings?)
            (let ((out (current-error-port)))
              (display "WARNING: " out)
              (display name-of-deprecated-misfeature out)
              (newline out)
              (display "    is deprecated by SRFI 125.  See" out)
              (newline out)
              (display "    " out)
              (display url:deprecated out)
              (newline out))))))

(define url:deprecated
  "http://srfi.schemers.org/srfi-125/srfi-125.html")

;; List of deprecated features for which a warning has already
;; been issued.

(define already-warned '())

;;; Comentary from SRFI 125 standard implementation
;;;
;;; Comparators contain a type test predicate, which implementations
;;; of the hash-table-set! procedure can use to reject invalid keys.
;;; That's hard to do without sacrificing interoperability with R6RS
;;; and/or SRFI 69 and/or SRFI 126 hash tables.
;;;
;;; Full interoperability means the hash tables implemented here are
;;; interchangeable with the SRFI 126 hashtables used to implement them.
;;; SRFI 69 and R6RS and SRFI 126 hashtables don't contain comparators,
;;; so any association between a hash table and its comparator would have
;;; to be maintained outside the representation of hash tables themselves,
;;; which is problematic unless weak pointers are available.
;;;
;;; Not all of the hash tables implemented here will have comparators
;;; associated with them anyway, because an equivalence procedure
;;; and hash function can be used to create a hash table instead of
;;; a comparator (although that usage is deprecated by SRFI 125).
;;;
;;; One way to preserve interoperability while enforcing a comparator's
;;; type test is to incorporate that test into a hash table's hash
;;; function.  The advantage of doing that should be weighed against
;;; these disadvantages:
;;;
;;;     If the type test is slow, then hashing would also be slower.
;;;
;;;     The R6RS, SRFI 69, and SRFI 126 APIs allow extraction of
;;;     a hash function from some hash tables.
;;;     Some programmers might expect that hash function to be the
;;;     hash function encapsulated by the comparator (in the sense
;;;     of eq?, perhaps) even though this API makes no such guarantee
;;;     (and extraction of that hash function from an existing hash
;;;     table can only be done by calling a deprecated procedure).

;; If %enforce-comparator-type-tests is true, then make-hash-table,
;; when passed a comparator, will use a hash function that enforces
;; the comparator's type test.

(define %enforce-comparator-type-tests #t)

;;; Don't use HASH-FUNCTION if EQUIV is a (known) refinement of EQUAL?
(define (%get-hash-table-hash-function equiv hash-function)
  (if (or (eq? eq? equiv)
          (eq? eqv? equiv)
          (eq? equal? equiv)
          (eq? string=? equiv))
      ;; Let GENERIC-HASH-TABLES decide a better HASH-FUNCTION
      #f
      ;; Not required by specification, but implemented by standard
      ;; implementation
      (if (eq? symbol=? equiv)
          symbol-hash
          hash-function)))

;;; Given a comparator, return its hash function, possibly augmented
;;; by the comparator's type test.
(define (%comparator-hash-function comparator)
  (let ((okay? (comparator-type-test-predicate comparator))
        (hash-function (%get-hash-table-hash-function
                        (comparator-equality-predicate comparator)
                        (comparator-hash-function comparator))))
    (and hash-function
         (if (and %enforce-comparator-type-tests
                  ;; These procedures already test type
                  (not (or (eq? hash-function symbol-hash)
                           (eq? hash-function string-ci-hash))))
             (lambda (x)
               (cond ((not (okay? x))
                      (error "Key rejected by hash-table comparator"
                             x
                             comparator))
                     (else
                      (hash-function x))))
             hash-function))))

;;; We let GENERIC-HASH-TABLES decide which weaknesses are supported
(define (%check-optional-arguments procname args)
  (if (memq 'thread-safe args)
      (error (string-append (symbol->string procname)
                            ": unsupported optional argument(s)")
             args)))

(define (%get-hash-table-weakness args)
  (cond
   ((memq 'ephemeral-values args)
    (if (or (memq 'ephemeral-keys args)
            (memq 'weak-keys args))
        'ephemeral-key-and-value
        'ephemeral-value))
   ((memq 'ephemeral-keys args)
    (if (memq 'weak-values args)
        'ephemeral-key-and-value
        'ephemeral-key))
   ((memq 'weak-keys args)
    (if (memq 'weak-values args)
        'weak-key-and-value
        'weak-key))
   ((memq 'weak-values args)
    'weak-value)
   (else #f)))

(define (%get-hash-table-capacity args)
  (or (find integer? args) 1))


;;; Constructors.

;;; Comentary from SRFI 125 standard implementation
;;;
;;; The first argument can be a comparator or an equality predicate.
;;;
;;; If the first argument is a comparator, any remaining arguments
;;; are implementation-dependent, but a non-negative exact integer
;;; should be interpreted as an initial capacity and the symbols
;;; thread-safe, weak-keys, ephemeral-keys, weak-values, and
;;; emphemeral-values should be interpreted specially.  (These
;;; special symbols are distinct from the analogous special symbols
;;; in SRFI 126.)
;;;
;;; If the first argument is not a comparator, then it had better
;;; be an equality predicate (which is deprecated by SRFI 125).
;;; If a second argument is present and is a procedure, then it's
;;; a hash function (which is allowed only for the deprecated case
;;; in which the first argument is an equality predicate).  If a
;;; second argument is not a procedure, then it's some kind of
;;; implementation-dependent optional argument, as are all arguments
;;; beyond the second.
;;;
;;; SRFI 128 defines make-eq-comparator, make-eqv-comparator, and
;;; make-equal-comparator procedures whose hash function is the
;;; default-hash procedure of SRFI 128, which is inappropriate
;;; for use with eq? and eqv? unless the object being hashed is
;;; never mutated.  Neither SRFI 125 nor 128 provide any way to
;;; define a comparator whose hash function is truly compatible
;;; with the use of eq? or eqv? as an equality predicate.
;;;
;;; That would make SRFI 125 almost as bad as SRFI 69 if not for
;;; the following paragraph of SRFI 125:
;;;
;;;     Implementations are permitted to ignore user-specified
;;;     hash functions in certain circumstances. Specifically,
;;;     if the equality predicate, whether passed as part of a
;;;     comparator or explicitly, is more fine-grained (in the
;;;     sense of R7RS-small section 6.1) than equal?, the
;;;     implementation is free — indeed, is encouraged — to
;;;     ignore the user-specified hash function and use something
;;;     implementation-dependent. This allows the use of addresses
;;;     as hashes, in which case the keys must be rehashed if
;;;     they are moved by the garbage collector. Such a hash
;;;     function is unsafe to use outside the context of
;;;     implementation-provided hash tables. It can of course be
;;;     exposed by an implementation as an extension, with
;;;     suitable warnings against inappropriate uses.
;;;
;;; That gives implementations permission to do something more
;;; useful, but when should implementations take advantage of
;;; that permission?  This implementation uses the superior
;;; solution provided by SRFI 126 whenever:
;;;
;;;     A comparator is passed as first argument and its equality
;;;     predicate is eq? or eqv?.
;;;
;;;     The eq? or eqv? procedure is passed as first argument
;;;     (which is a deprecated usage).

(define (make-hash-table comparator/equiv . rest)
  (if (comparator? comparator/equiv)
      (let ((equiv (comparator-equality-predicate comparator/equiv))
            (hash-function (%comparator-hash-function comparator/equiv)))
        (%make-hash-table equiv hash-function rest))
      (let* ((equiv comparator/equiv)
             (hash-function (if (and (not (null? rest))
                                     (procedure? (car rest)))
                                (car rest)
                                #f))
             (rest (if hash-function (cdr rest) rest)))
        (issue-warning-deprecated 'srfi-69-style:make-hash-table)
        (%make-hash-table equiv (%get-hash-table-hash-function equiv hash-function)
                          rest))))

(define (%make-hash-table equiv hash-function opts)
  (%check-optional-arguments 'make-hash-table opts)
  (let ((weakness (%get-hash-table-weakness opts))
        (capacity (%get-hash-table-capacity opts)))
    (gen:make-hash-table equiv hash-function
                         #:capacity capacity #:weakness weakness)))

(define (hash-table comparator . args)
  (let ((equiv (comparator-equality-predicate comparator))
        (hash-function (%comparator-hash-function comparator)))
    (apply gen:hash-table (if hash-function
                              (list equiv hash-function)
                              equiv)
           args)))

(define (hash-table-unfold stop? mapper successor seed comparator . rest)
  (let ((equiv (comparator-equality-predicate comparator))
        (hash-function (%comparator-hash-function comparator))
        (weakness (%get-hash-table-weakness rest))
        (capacity (%get-hash-table-capacity rest)))
    (gen:hash-table-unfold stop? mapper successor seed
                           equiv hash-function #:weakness weakness
                           #:capacity capacity)))

(define (alist->hash-table alist comparator/equiv . rest)
  (if (procedure? comparator/equiv)
      (let* ((equiv comparator/equiv)
             (hash-function (and (pair? rest) (procedure? (car rest))
                                 (car rest)))
             (rest (if hash-function (cdr rest) rest))
             (hash-function (%get-hash-table-hash-function equiv hash-function))
             (weakness (%get-hash-table-weakness rest))
             (capacity (%get-hash-table-capacity rest)))
        (issue-warning-deprecated 'srfi-69-style:alist->hash-table)
        (gen:alist->hash-table alist equiv hash-function
                               #:capacity capacity #:weakness weakness))
      (let* ((equiv (comparator-equality-predicate comparator/equiv))
             (hash-function (%comparator-hash-function comparator/equiv))
             (weakness (%get-hash-table-weakness rest))
             (capacity (%get-hash-table-capacity rest)))
        (gen:alist->hash-table alist equiv hash-function
                               #:capacity capacity #:weakness weakness))))


;;;; Accessing table items

(define hash-table-ref gen:hash-table-ref)
(define hash-table-ref/default gen:hash-table-ref/default)


;;; Predicates.

(define hash-table? gen:hash-table?)
(define hash-table-empty? gen:hash-table-empty?)
(define hash-table-contains? gen:hash-table-contains?)
(define hash-table-mutable? gen:hash-table-mutable?)

(define (hash-table=? value-comparator ht1 ht2)
  (gen:hash-table=? (comparator-equality-predicate value-comparator)
                    ht1 ht2))


;;; Mutators.

(define hash-table-set! gen:hash-table-set!)
(define hash-table-delete! gen:hash-table-delete!)
(define hash-table-intern! gen:hash-table-intern!)
(define hash-table-update! gen:hash-table-update!)
(define hash-table-update!/default gen:hash-table-update!/default)
(define hash-table-pop! gen:hash-table-pop!)
(define (hash-table-clear! ht) (gen:hash-table-clear! ht))


;; The whole hash table.

(define hash-table-size gen:hash-table-size)
(define hash-table-keys gen:hash-table-keys)
(define hash-table-values gen:hash-table-values)
(define hash-table-entries gen:hash-table-entries)
(define hash-table-find gen:hash-table-find)
(define hash-table-count gen:hash-table-count)


;;; Mapping and folding.

(define hash-table-map->list gen:hash-table-map->list)
(define hash-table-for-each gen:hash-table-for-each)
(define hash-table-prune! gen:hash-table-prune!)
(define hash-table-map! gen:hash-table-map!)

(define (hash-table-map proc comparator ht)
  (let ((equiv (comparator-equality-predicate comparator))
        (hash-function (%comparator-hash-function comparator)))
    (gen:hash-table-map proc ht equiv hash-function)))

(define (hash-table-fold proc init ht)
  (if (hash-table? proc)
      (begin (issue-warning-deprecated 'srfi-69-style:hash-table-fold)
             (hash-table-fold init ht proc))
      (gen:hash-table-fold proc init ht)))



;;; Copying and conversion.

(define hash-table->alist gen:hash-table->alist)

(define* (hash-table-copy ht #:optional mutable)
  (gen:hash-table-copy ht #:mutable mutable))

(define (hash-table-empty-copy ht)
  (gen:hash-table-empty-copy ht))


;;; Hash tables as sets.

(define (hash-table-union! ht1 ht2)
  (unless (eq? (gen:hash-table-equivalence-function ht1)
               (gen:hash-table-equivalence-function ht2))
    (error "Hash tables have different equivalence functions" ht1 ht2))
  (gen:hash-table-union! ht1 ht2))

(define (hash-table-intersection! ht1 ht2)
  (unless (eq? (gen:hash-table-equivalence-function ht1)
               (gen:hash-table-equivalence-function ht2))
    (error "Hash tables have different equivalence functions" ht1 ht2))
  (gen:hash-table-intersection! ht1 ht2))

(define (hash-table-difference! ht1 ht2)
  (unless (eq? (gen:hash-table-equivalence-function ht1)
               (gen:hash-table-equivalence-function ht2))
    (error "Hash tables have different equivalence functions" ht1 ht2))
  (gen:hash-table-difference! ht1 ht2))

(define (hash-table-xor! ht1 ht2)
  (unless (eq? (gen:hash-table-equivalence-function ht1)
               (gen:hash-table-equivalence-function ht2))
    (error "Hash tables have different equivalence functions" ht1 ht2))
  (gen:hash-table-xor! ht1 ht2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The following procedures are deprecated by SRFI 125, but must
;;; be exported nonetheless.
;;;
;;; Programs that import the (srfi 125) library must rename the
;;; deprecated string-hash and string-ci-hash procedures to avoid
;;; conflict with the string-hash and string-ci-hash procedures
;;; exported by SRFI 126 and SRFI 128.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deprecated:hash obj . rest)
  (issue-warning-deprecated 'hash)
  (apply gen:hash obj rest))

(define (deprecated:string-hash obj . rest)
  (issue-warning-deprecated 'srfi-125:string-hash)
  (apply string-hash obj rest))

(define (deprecated:string-ci-hash obj . rest)
  (issue-warning-deprecated 'srfi-125:string-ci-hash)
  (apply string-ci-hash obj rest))

(define (deprecated:hash-by-identity obj . rest)
  (issue-warning-deprecated 'hash-by-identity)
  (apply gen:hash-by-identity obj rest))

(define (deprecated:hash-table-equivalence-function ht)
  (issue-warning-deprecated 'hash-table-equivalence-function)
  (gen:hash-table-equivalence-function ht))

(define (deprecated:hash-table-hash-function ht)
  (issue-warning-deprecated 'hash-table-hash-function)
  (gen:hash-table-hash-function ht))

(define (deprecated:hash-table-exists? ht key)
  (issue-warning-deprecated 'hash-table-exists?)
  (gen:hash-table-contains? ht key))

(define (deprecated:hash-table-walk ht proc)
  (issue-warning-deprecated 'hash-table-walk)
  (gen:hash-table-for-each proc ht))

(define (deprecated:hash-table-merge! ht1 ht2)
  (issue-warning-deprecated 'hash-table-merge!)
  (gen:hash-table-union! ht1 ht2))

;; eof
