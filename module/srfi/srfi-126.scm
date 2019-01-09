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


(define-module (srfi srfi-126)
  #:use-module (srfi srfi-1)
  #:use-module ((ice-9 generic-hash-tables) #:prefix gen:)
  #:use-module ((rnrs hashtables)
                #:select (hashtable?
                          hashtable-contains? hashtable-mutable?
                          hashtable-set! hashtable-delete! hashtable-clear!
                          hashtable-size
                          hashtable-keys hashtable-entries
                          hashtable-equivalence-function
                          hashtable-hash-function
                          equal-hash symbol-hash
                          string-hash string-ci-hash))
  #:re-export (hashtable?
               hashtable-contains? hashtable-mutable?
               hashtable-set! hashtable-delete! hashtable-clear!
               hashtable-size
               hashtable-keys hashtable-entries
               hashtable-equivalence-function
               hashtable-hash-function
               equal-hash symbol-hash
               string-hash string-ci-hash)
  #:export (make-eq-hashtable
            make-eqv-hashtable make-hashtable
            alist->eq-hashtable alist->eqv-hashtable alist->hashtable
            hashtable-weakness
            hashtable-ref hashtable-lookup
            hashtable-update! hashtable-intern!
            hashtable-copy hashtable-empty-copy
            hashtable-values
            hashtable-key-list hashtable-value-list hashtable-entry-lists
            hashtable-walk hashtable-update-all!
            hashtable-prune! hashtable-merge! hashtable-sum
            hashtable-map->lset
            hashtable-find hashtable-empty?
            hashtable-pop!
            hashtable-inc! hashtable-dec!
            hash-salt))

(cond-expand-provide (current-module) '(srfi-126))

(define hashtable? gen:hash-table?)

(define* (make-hashtable hash-function equiv-function
                         #:optional capacity weakness)
  "Creates a new hash table. EQUIV-FUNCTION is used as the comparison
function and HASH-FUNCTION, if provided and not false, is used as the
hash function, otherwise a suitable hash-function for the hash table is
guessed or, if one can't be guessed, an error is signaled. WEAKNESS
should be either #f, WEAK-KEY, WEAK-VALUE or WEAK-KEY-AND-VALUE.
CAPACITY is the minimal number of buckets of the hash table."
  (gen:make-hash-table equiv-function (if (pair? hash-function)
                                          (car hash-function)
                                          hash-function)
                       #:capacity (or capacity 1)
                       #:weakness weakness))

(define* (make-eq-hashtable #:optional capacity weakness)
  (gen:make-hash-table eq? gen:hash-by-identity
                       #:capacity (or capacity 1)
                       #:weakness weakness))

(define* (make-eqv-hashtable #:optional capacity weakness)
  (gen:make-hash-table eqv? gen:hash-by-value
                       #:capacity (or capacity 1)
                       #:weakness weakness))

(define alist->hashtable
  (case-lambda
    ((hash-function equiv-function alist)
     (gen:alist->hash-table alist equiv-function
                            (if (pair? hash-function)
                                (car hash-function)
                                hash-function)))
    ((hash-function equiv-function capacity alist)
     (gen:alist->hash-table alist equiv-function
                            (if (pair? hash-function)
                                (car hash-function)
                                hash-function)
                            #:capacity (or capacity 1)))
    ((hash-function equiv-function capacity weakness alist)
     (gen:alist->hash-table alist equiv-function
                            (if (pair? hash-function)
                                (car hash-function)
                                hash-function)
                            #:capacity (or capacity 1)
                            #:weakness weakness))))

(define (alist->eq-hashtable . args)
  (apply alist->hashtable #f eq? args))

(define (alist->eqv-hashtable . args)
  (apply alist->hashtable #f eqv? args))

(define* (hashtable-ref ht key #:optional default)
  (if default
      (gen:hash-table-ref/default ht key default)
      (gen:hash-table-ref ht key)))

;; (define hashtable-contains? rnrs:hashtable-contains?)
;; (define hashtable-set! rnrs:hashtable-set!)
;; (define hashtable-delete! rnrs:hashtable-delete!)

(define (hashtable-lookup hashtable key)
  (let* ((found? #t)
         (value (gen:hash-table-ref hashtable key
                                    (lambda () (set! found? #f) #f))))
    (values value found?)))

(define* (hashtable-update! ht key modifier #:optional default)
  (if default
      (gen:hash-table-update!/default ht key modifier default)
      (gen:hash-table-update! ht key modifier)))

(define hashtable-intern! gen:hash-table-intern!)

(define* (hashtable-copy hashtable #:optional mutable
                         (weakness (hashtable-weakness hashtable)))
  (gen:hash-table-copy hashtable #:mutable mutable #:weakness weakness))

;; (define hashtable-clear! rnrs:hashtable-clear!)

(define* (hashtable-empty-copy hashtable #:optional capacity
                               (weakness (hashtable-weakness hashtable)))
  (let ((capacity (case capacity
                    ((#f) 1)
                    ((#t) (hashtable-size hashtable))
                    (else capacity))))
    (gen:hash-table-empty-copy hashtable #:capacity capacity #:weakness weakness)))


;;;; Accessing whole tables

;; (define hashtable-size rnrs:hashtable-size)
;; (define hashtable-keys gen:hash-table-key-vector)
(define hashtable-values gen:hash-table-value-vector)
;; (define hashtable-entries rnrs:hashtable-entries)
(define hashtable-key-list gen:hash-table-keys)
(define hashtable-value-list gen:hash-table-values)
(define hashtable-entry-lists gen:hash-table-entries)
(define hashtable->alist gen:hash-table->alist)

(define (hashtable-walk hashtable proc)
  (gen:hash-table-for-each proc hashtable))

(define (hashtable-update-all! hashtable proc)
  (gen:hash-table-map! proc hashtable))

(define (hashtable-prune! hashtable proc)
  (gen:hash-table-prune! proc hashtable))

(define (hashtable-sum hashtable init proc)
  (gen:hash-table-fold proc init hashtable))

(define (hashtable-merge! ht other-ht)
  (gen:hash-table-for-each (lambda (k v) (hashtable-set! ht k v))
                           other-ht)
  ht)

(define (hashtable-map->lset hashtable proc)
  (gen:hash-table-map->list proc hashtable))

(define (hashtable-find hashtable proc)
  (call/cc (lambda (return)
             (gen:hash-table-for-each (lambda (k v)
                                        (when (proc k v) (return k v #t)))
                                      hashtable)
             (values *unspecified* *unspecified* #f))))

(define hashtable-empty? gen:hash-table-empty?)
(define hashtable-pop! gen:hash-table-pop!)

(define* (hashtable-inc! hashtable key #:optional (number 1))
  (hashtable-update! hashtable key (lambda (v) (+ v number)) 0))

(define* (hashtable-dec! hashtable key #:optional (number 1))
  (hashtable-update! hashtable key (lambda (v) (- v number)) 0))

(define hashtable-weakness gen:hash-table-weakness)
;; (define hashtable-mutable? rnrs:hashtable-mutable?)
;; (define hashtable-equivalence-function rnrs:hashtable-equivalence-function)
;; (define hashtable-hash-function rnrs:hashtable-hash-function)
;; (define equal-hash rnrs:equal-hash)
;; (define string-hash rnrs:string-hash)
;; (define string-ci-hash rnrs:string-ci-hash)
;; (define symbol-hash rnrs:symbol-hash)

(define *hash-salt*
  (let ((seed (getenv "SRFI_126_HASH_SEED")))
    (if (or (not seed) (string=? seed ""))
        (random most-positive-fixnum)
        (modulo (string-hash seed) most-positive-fixnum))))

(define (hash-salt) *hash-salt*)

;; eof
