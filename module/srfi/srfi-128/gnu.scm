;;; Extensions to SRFI-128

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

(define-module (srfi srfi-128 gnu)
  #:export (hash-bound hash-salt with-hash-salt))

(define-syntax hash-bound
  (syntax-rules ()
    ((hash-bound) most-positive-fixnum)))

(define %salt% (make-parameter (random (hash-bound)
                                       (seed->random-state (current-time)))))

(define-syntax hash-salt
  (syntax-rules ()
    ((hash-salt) (%salt%))))

(define-syntax with-hash-salt
  (syntax-rules ()
    ((with-hash-salt new-salt hash-func obj)
     (parameterize ((%salt% new-salt)) (hash-func obj)))))

;; eof
