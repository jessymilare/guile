;;; srfi-128.scm --- Comparators

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
;; John Cowan

;;; Copyright (C) John Cowan (2015). All Rights Reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.


(define-module (srfi srfi-128)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  ;; HASH-BOUND, HASH-SALT and WITH-HASH-SALT are defined here because
  ;; the latter is not standard
  #:use-module ((srfi srfi-128 gnu) #:select (hash-bound hash-salt))
  #:use-module ((rnrs unicode) #:select (char-foldcase))
  #:use-module (rnrs bytevectors)
  #:use-module ((ice-9 generic-hash-tables)
                #:select ((hash . equal-hash)
                          string-ci-hash hash-by-identity hash-by-value))
  #:export (comparator?
            make-comparator
            comparator-type-test-predicate comparator-equality-predicate
            comparator-ordering-predicate comparator-hash-function
            comparator-ordered? comparator-hashable?
            comparator-test-type comparator-check-type
            comparator-hash
            make-pair-comparator make-list-comparator make-vector-comparator
            make-eq-comparator make-eqv-comparator make-equal-comparator
            boolean-hash char-hash char-ci-hash number-hash
            make-default-comparator default-hash
            comparator-register-default!
            =? <? >? <=? >=?
            comparator-if<=>)
  #:re-export (string-hash string-ci-hash symbol-hash hash-bound hash-salt))

(cond-expand-provide (current-module) '(srfi-128))


;; Arithmetic if
(define-syntax comparator-if<=>
  (syntax-rules ()
    ((if<=> a b less equal greater)
     (comparator-if<=> default-comparator a b less equal greater))
    ((comparator-if<=> comparator a b less equal greater)
     (cond
      ((<? comparator a b) less)
      ((=? comparator a b) equal)
      (else greater)))))


;;; Definition of comparator records with accessors and basic comparator

(define-record-type comparator
  (make-raw-comparator type-test equality ordering hash ordering? hash?)
  comparator?
  (type-test comparator-type-test-predicate)
  (equality comparator-equality-predicate)
  (ordering comparator-ordering-predicate)
  (hash comparator-hash-function)
  (ordering? comparator-ordered?)
  (hash? comparator-hashable?))

(define (always-true obj) #t)

;; Public constructor
(define (make-comparator type-test equality ordering hash)
  (make-raw-comparator
   (if (eq? type-test #t) always-true type-test)
   (if (eq? equality #t) (lambda (x y) (eqv? (ordering x y) 0)) equality)
   (if ordering ordering (lambda (x y) (error "ordering not supported")))
   (if hash hash (lambda (x y) (error "hashing not supported")))
   (if ordering #t #f)
   (if hash #t #f)))


;;; Invokers

;; Invoke the test type
(define (comparator-test-type comparator obj)
  ((comparator-type-test-predicate comparator) obj))

;; Invoke the test type and throw an error if it fails
(define (comparator-check-type comparator obj)
  (if (comparator-test-type comparator obj)
      #t
      (error "Comparator type check failed" comparator obj)))

;; Invoke the hash function
(define (comparator-hash comparator obj)
  ((comparator-hash-function comparator) obj))


;;; Comparison predicates

;; Binary versions for internal use

(define (binary=? comparator a b)
  ((comparator-equality-predicate comparator) a b))

(define (binary<? comparator a b)
  ((comparator-ordering-predicate comparator) a b))

(define (binary>? comparator a b)
  (binary<? comparator b a))

(define (binary<=? comparator a b)
  (not (binary>? comparator a b)))

(define (binary>=? comparator a b)
  (not (binary<? comparator a b)))

;; General versions for export

(define (=? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary=? comparator a b)
	 (if (null? objs) #t (loop b (car objs) (cdr objs))))))

(define (<? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary<? comparator a b)
	 (if (null? objs) #t (loop b (car objs) (cdr objs))))))

(define (>? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary>? comparator a b)
	 (if (null? objs) #t (loop b (car objs) (cdr objs))))))

(define (<=? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary<=? comparator a b)
	 (if (null? objs) #t (loop b (car objs) (cdr objs))))))

(define (>=? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary>=? comparator a b)
	 (if (null? objs) #t (loop b (car objs) (cdr objs))))))


;;; Simple ordering and hash functions

(define boolean-hash hash-by-identity)
(define char-hash    hash-by-identity)
(define number-hash  hash-by-value)

(define* (char-ci-hash c #:optional (size most-positive-fixnum))
  (hashq (char-foldcase c) size))

;; Lexicographic ordering of complex numbers
(define (complex<? a b)
  (or (< (real-part a) (real-part b))
      (and (= (real-part a) (real-part b))
           (< (imag-part a) (imag-part b)))))

(define (symbol<? a b)
  ;; Valid according to spec.
  ;; It's faster to hash than to compare strings.
  (let ((ha (hashq a (hash-bound)))
        (hb (hashq b (hash-bound))))
    (or (< ha hb)
        (and (= ha hb)
             (not (eq? a b))
             (string<? (symbol->string a) (symbol->string b))))))

;; Stick to fixnums
(define lower-mask (ash (hash-bound) -5)) ; (/ (hash-bound) 32)

;; Hash helper
(define (mix h1 h2)
  (logxor (* (logand h1 lower-mask) 31) h2))



;;; Pair comparator
(define (make-pair-comparator car-comparator cdr-comparator)
  (make-comparator
   (make-pair-type-test car-comparator cdr-comparator)
   (make-pair=? car-comparator cdr-comparator)
   (make-pair<? car-comparator cdr-comparator)
   (make-pair-hash car-comparator cdr-comparator)))

(define (make-pair-type-test car-comparator cdr-comparator)
  (let ((car-test (comparator-type-test-predicate car-comparator))
        (cdr-test (comparator-type-test-predicate cdr-comparator)))
    (if (eq? always-true car-test cdr-test)
        pair?
        (lambda (obj)
          (and (pair? obj)
               (car-test (car obj))
               (cdr-test (cdr obj)))))))

(define (make-pair=? car-comparator cdr-comparator)
  (let ((car-equiv (comparator-equality-predicate car-comparator))
        (cdr-equiv (comparator-equality-predicate cdr-comparator)))
    (if (eq? equal? car-equiv cdr-equiv)
        equal?
        (lambda (a b)
          (and (car-equiv (car a) (car b))
               (cdr-equiv (cdr a) (cdr b)))))))

(define (make-pair<? car-comparator cdr-comparator)
  (let ((car-equiv (comparator-equality-predicate car-comparator))
        (car<? (comparator-ordering-predicate car-comparator))
        (cdr<? (comparator-ordering-predicate cdr-comparator)))
    (lambda (a b)
      (or (car<? (car a) (car b))
          (and (car-equiv (car a) (car b))
               (cdr<? (cdr a) (cdr b)))))))

(define pair-hash-salt (mix (symbol-hash 'pair) (hash-salt)))

(define (make-pair-hash car-comparator cdr-comparator)
  (let ((car-hash (comparator-hash-function car-comparator))
        (cdr-hash (comparator-hash-function cdr-comparator)))
    (if (eq? equal-hash car-hash cdr-hash)
        equal-hash
        (lambda (obj)
          (mix (mix pair-hash-salt (car-hash (car obj)))
               (cdr-hash (cdr obj)))))))


;;; List comparator

(define (make-list-comparator element-comparator type-test empty? head tail)
  (make-comparator
   (make-list-type-test element-comparator type-test empty? head tail)
   (make-list=? element-comparator type-test empty? head tail)
   (make-list<? element-comparator type-test empty? head tail)
   (make-list-hash element-comparator type-test empty? head tail)))

(define (make-list-type-test element-comparator type-test empty? head tail)
  (let ((elem-type-test (comparator-type-test-predicate element-comparator)))
    (lambda (obj)
      (and
       (type-test obj)
       (let loop ((obj obj))
         (cond
          ((empty? obj) #t)
          ((not (elem-type-test (head obj))) #f)
          (else (loop (tail obj)))))))))

(define (make-list=? element-comparator type-test empty? head tail)
  (let ((elem=? (comparator-equality-predicate element-comparator)))
    (lambda (a b)
      (let loop ((a a) (b b))
        (cond
         ((empty? a) (empty? b))
         ((empty? b) #f)
         ((elem=? (head a) (head b)) (loop (tail a) (tail b)))
         (else #f))))))

(define (make-list<? element-comparator type-test empty? head tail)
  (let ((elem=? (comparator-equality-predicate element-comparator))
        (elem<? (comparator-ordering-predicate element-comparator)))
    (lambda (a b)
      (let loop ((a a) (b b))
        (cond
         ((empty? a) (not (empty? b)))
         ((empty? b) #f)
         ((elem<? (head a) (head b)) #t)
         ((elem=? (head a) (head b)) (loop (tail a) (tail b)))
         (else #f))))))

(define list-hash-salt (mix (symbol-hash 'list) (hash-salt)))

(define (make-list-hash element-comparator type-test empty? head tail)
  (let ((elem-hash (comparator-hash-function element-comparator)))
    (lambda (obj)
      (let loop ((obj obj)
                 (result list-hash-salt))
        (cond
         ((empty? obj) result)
         (else (loop (tail obj) (mix result (elem-hash (head obj))))))))))


;;; Vector comparator

(define (make-vector-comparator element-comparator type-test length ref)
  (make-comparator
   (make-vector-type-test element-comparator type-test length ref)
   (make-vector=? element-comparator type-test length ref)
   (make-vector<? element-comparator type-test length ref)
   (make-vector-hash element-comparator type-test length ref)))

(define (make-vector-type-test element-comparator type-test length ref)
  (let ((elem-type-test (comparator-type-test-predicate element-comparator)))
    (lambda (obj)
      (and
       (type-test obj)
       (let ((len (length obj)))
         (let loop ((n 0))
           (cond
            ((= n len) #t)
            ((not (elem-type-test (ref obj n))) #f)
            (else (loop (+ n 1))))))))))

(define (make-vector=? element-comparator type-test length ref)
  (let ((elem=? (comparator-equality-predicate element-comparator)))
    (lambda (a b)
      (let ((len (length b)))
        (and
         (= (length a) len)
         (let loop ((n 0))
           (cond
            ((= n len) #t)
            ((elem=? (ref a n) (ref b n)) (loop (+ n 1)))
            (else #f))))))))

(define (make-vector<? element-comparator type-test length ref)
  (let ((elem=? (comparator-equality-predicate element-comparator))
        (elem<? (comparator-ordering-predicate element-comparator)))
    (lambda (a b)
      (let ((lena (length a))
            (lenb (length b)))
        (cond
         ((< lena lenb) #t)
         ((> lena lenb) #f)
         (else
          (let loop ((n 0))
            (cond
             ((= n lena) #f)
             ((elem<? (ref a n) (ref b n)) #t)
             ((elem=? (ref a n) (ref b n)) (loop (+ n 1)))
             (else #f)))))))))

(define vector-hash-salt (mix (symbol-hash 'vector) (hash-salt)))

(define (make-vector-hash element-comparator type-test length ref)
  (let ((elem-hash (comparator-hash-function element-comparator)))
    (lambda (obj)
      (let ((len (length obj)))
        (let loop ((n 0) (result vector-hash-salt))
          (cond
           ((= n len) result)
           (else (loop (+ n 1) (mix result (elem-hash (ref obj n)))))))))))


;;; The default comparator

;;; Standard comparators and their functions

;; The unknown-object comparator, used as a fallback to everything else
;; Everything compares exactly the same and hashes to 0
(define unknown-object-comparator
  (make-comparator
   always-true
   (lambda (a b) #t)
   (lambda (a b) #f)
   (lambda (obj) 0)))

;; Next index for added comparator

(define *next-comparator-index* 0)
(define *registered-comparators* (make-vector 8 unknown-object-comparator))

;; Register a new comparator for use by the default comparator.
(define (comparator-register-default! comparator)
  (let ((len (vector-length *registered-comparators*)))
    (if (= *next-comparator-index* (- len 1))
        (set! *registered-comparators*
          (vector-copy *registered-comparators* 0 (* 2 len) unknown-object-comparator))))
  (vector-set! *registered-comparators* *next-comparator-index* comparator)
  (set! *next-comparator-index* (+ 1 *next-comparator-index*)))

;; Return ordinal for object types: null sorts before booleans, which sort
;; before numbers, etc.  Implementations can extend this.
;; People who call comparator-register-default! effectively do extend it.
(define (internal-object-type obj)
  (cond
   ((null? obj) 0)
   ((boolean? obj) 1)
   ((number? obj) 2)
   ((char? obj) 3)
   ((string? obj) 4)
   ((symbol? obj) 5)
   ((bytevector? obj) 6)
   ((vector? obj) 7)
   ((pair? obj) 8)
   ((unspecified? obj) 9)
   ((eof-object? obj) 10)
   ;; Add more here if you want
   (else #f)))

(define (external-object-type obj)
  (registered-index obj))

;; Return the index for the registered type of obj.
(define (registered-index obj)
  (vector-index (lambda (comparator)
                  (comparator-test-type comparator obj))
                *registered-comparators*))

(define (external-object-comparator obj)
  (vector-any (lambda (comparator)
                (and (comparator-test-type comparator obj)
                     comparator))
              *registered-comparators*))

;; Given an index, retrieve a registered conductor.
(define (registered-comparator i)
  (vector-ref *registered-comparators* i))

(define (internal-dispatch-equality type a b)
  ;; EQUAL? already returns #t for many internal types
  (case type
    ;; ((0) #t) ; All empty lists are equal
    ;; ((1) (if a (and b #t) (not b)))
    ;; ((2) (= a b))
    ;; ((3) (char=? a b))
    ;; ((4) (string=? a b))
    ;; ((5) (eq? a b))
    ;; ((6) (default-bytevector=? a b))
    ((7) (default-vector=? a b))
    ((8) (default-pair=? a b))
    ;; ((9 10) #t)
    ;; Add more here
    (else #f)))

(define (external-dispatch-equality type a b)
  (binary=? (registered-comparator type) a b))

(define (internal-dispatch-ordering type a b)
  ;; EQUAL? already eliminates some internal types
  (case type
    ;; ((0) #f) ; All empty lists are equal
    ((1) (and (not a) b)) ; #f < #t but not otherwise
    ((2) (complex<? a b))
    ((3) (char<? a b))
    ((4) (string<? a b))
    ((5) (symbol<? a b))
    ((6) (default-bytevector<? a b))
    ((7) (default-vector<? a b))
    ((8) (default-pair<? a b))
    ;; ((9 10) #f)
    ;; Add more here
    ))

(define (external-dispatch-ordering type a b)
  (binary<? (registered-comparator type) a b))

;; EQUAL-HASH returns the same as HASH-BY-VALUE on numbers and
;; HASH-BY-IDENTITY on booleans, chars and symbols.

(define (default-hash obj)
  (let ((type (internal-object-type obj)))
    (if type
        (if (or (<= type 6) (>= type 9))
            (equal-hash obj)
            (case type
              ((7) (default-vector-hash obj))
              ((8) (default-pair-hash obj))
              ;; Add more here
              ))
        (let ((comparator (external-object-comparator obj)))
          (comparator-hash comparator obj)))))

(define (default-ordering a b)
  (and (not (equal? a b)) ; should be much faster than this procedure
       (let ((a-itype (internal-object-type a))
             (b-itype (internal-object-type b)))
         (cond
          ((not b-itype)
           (or a-itype
               ;; Neither a nor b are of internal type:
               ;; dispatch ordering on external type
               (let ((a-etype (external-object-type a))
                     (b-etype (external-object-type b)))
                 (cond
                  ((< a-etype b-etype) #t)
                  ((> a-etype b-etype) #f)
                  (else (external-dispatch-ordering a-etype a b))))))
          ((not a-itype) #f)
          ;; Both a and b are of internal type
          ((< a-itype b-itype) #t)
          ((> a-itype b-itype) #f)
          (else (internal-dispatch-ordering a-itype a b))))))

(define (default-equality a b)
  (or (equal? a b) ; should be much faster than this procedure
      (let ((a-itype (internal-object-type a))
            (b-itype (internal-object-type b)))
        (and (eqv? a-itype b-itype)
             (if a-itype
                 (internal-dispatch-equality a-itype a b)
                 (let ((a-comp (external-object-comparator a))
                       (b-comp (external-object-comparator b)))
                   (and (eq? a-comp b-comp) (binary=? a-comp a b))))))))

;; Note: comparators are immutable, no reason to allocate a new one
(define default-comparator (make-comparator always-true default-equality
                                            default-ordering default-hash))

(define (make-default-comparator) default-comparator)

(define default-pair-comparator
  (make-pair-comparator default-comparator default-comparator))

(define default-pair=?
  (comparator-equality-predicate default-pair-comparator))
(define default-pair<?
  (comparator-ordering-predicate default-pair-comparator))
(define default-pair-hash
  (comparator-hash-function default-pair-comparator))

(define default-vector-comparator
  (make-vector-comparator default-comparator vector?
                          vector-length vector-ref))

(define default-vector=?
  (comparator-equality-predicate default-vector-comparator))
(define default-vector<?
  (comparator-ordering-predicate default-vector-comparator))
(define default-vector-hash
  (comparator-hash-function default-vector-comparator))

(define default-bytevector-comparator
  (make-vector-comparator default-comparator bytevector?
                          bytevector-length bytevector-u8-ref))

(define default-bytevector=?
  (comparator-equality-predicate default-bytevector-comparator))
(define default-bytevector<?
  (comparator-ordering-predicate default-bytevector-comparator))
(define default-bytevector-hash
  (comparator-hash-function default-bytevector-comparator))

;;; Wrapped equality predicates
;;; These comparators don't have ordering functions.

;; Note: comparators are immutable, no reason to allocate a new one
(define eq-comparator (make-comparator #t eq? #f default-hash))
(define (make-eq-comparator) eq-comparator)

(define eqv-comparator (make-comparator #t eqv? #f default-hash))
(define (make-eqv-comparator) eqv-comparator)

(define equal-comparator (make-comparator #t equal? #f default-hash))
(define (make-equal-comparator) equal-comparator)

;; eof
