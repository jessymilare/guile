;;; hashtables.scm --- The R6RS hashtables library

;;      Copyright (C) 2010 Free Software Foundation, Inc.
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


(library (rnrs hashtables (6))
  (export make-eq-hashtable
	  make-eqv-hashtable
	  make-hashtable

	  hashtable?
	  hashtable-size
	  hashtable-ref
	  hashtable-set!
	  hashtable-delete!
	  hashtable-contains?
	  hashtable-update!
	  hashtable-copy
	  hashtable-clear!
	  hashtable-keys
	  hashtable-entries
	  
	  hashtable-equivalence-function
	  hashtable-hash-function
	  hashtable-mutable?

	  equal-hash
	  string-hash
	  string-ci-hash
	  symbol-hash)
  (import (rename (only (guile) string-hash-ci 
                        string-hash
                        hashq
                        hashv
                        modulo
                        *unspecified*)
		  (string-hash-ci string-ci-hash))
	  (only (ice-9 optargs) define*)
	  (rename (only (ice-9 generic-hash-tables)
                        make-hash-table
                        hash-table?
                        hash-table-mutable?
                        hash
                        hash-by-identity
                        hash-by-value
                        hash-table-size
                        hash-table-ref/default
                        hash-table-set-single!
                        hash-table-delete-single!
                        hash-table-contains?
                        hash-table-update!/default
                        hash-table-clear!
                        hash-table-copy
                        hash-table-entry-vectors
                        hash-table-equivalence-function
                        hash-table-hash-function
                        hash-table-key-vector)
                  (make-hash-table gen:make-hash-table)
		  (hash equal-hash))
          (only (srfi srfi-69) hash-table-set! hash-table-delete!)
	  (rnrs base (6))
	  (rnrs records procedural (6)))

  (define hashtable? hash-table?)

  (define hashtable-mutable? hash-table-mutable?)

  (define symbol-hash hash-by-identity)

  (define* (make-eq-hashtable #:optional capacity)
    (if capacity
        (gen:make-hash-table eq? #f #:capacity capacity)
        (gen:make-hash-table eq? #f)))

  (define* (make-eqv-hashtable #:optional capacity)
    (if capacity
        (gen:make-hash-table eqv? #f #:capacity capacity)
        (gen:make-hash-table eqv? #f)))

  (define* (make-hashtable hash-function equiv #:optional capacity)
    (if capacity
        (gen:make-hash-table equiv hash-function #:capacity capacity)
        (gen:make-hash-table equiv hash-function)))
  
  (define hashtable-size hash-table-size)

  (define hashtable-ref hash-table-ref/default)

  (define hashtable-set! hash-table-set!)

  (define hashtable-delete! hash-table-delete!)

  (define hashtable-contains? hash-table-contains?)

  (define hashtable-update! hash-table-update!/default)

  (define* (hashtable-copy ht #:optional mutable)
    (hash-table-copy ht #:mutable mutable))

  (define hashtable-clear! hash-table-clear!)

  (define hashtable-keys hash-table-key-vector)

  (define hashtable-entries hash-table-entry-vectors)

  (define hashtable-equivalence-function hash-table-equivalence-function)

  (define (hashtable-hash-function hashtable)
    (let ((hash-function (hash-table-hash-function hashtable)))
      (cond ((or (eq? hash-by-identity hash-function)
                 (eq? hash-by-value    hash-function))
             #f)
            (else hash-function)))))
