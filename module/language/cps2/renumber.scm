;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014, 2015 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:
;;;
;;; A pass to renumber variables and continuation labels so that they
;;; are contiguous within each function and, in the case of labels,
;;; topologically sorted.
;;;
;;; Code:

(define-module (language cps2 renumber)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (language cps2)
  #:use-module (language cps2 utils)
  #:use-module (language cps intset)
  #:use-module (language cps intmap)
  #:export (renumber))

(define* (compute-tail-path-lengths conts kfun preds)
  (define (add-lengths labels lengths length)
    (intset-fold (lambda (label lengths)
                   (intmap-add! lengths label length))
                 labels
                 lengths))
  (define (compute-next labels lengths)
    (intset-fold (lambda (label labels)
                   (fold1 (lambda (pred labels)
                            (if (intmap-ref lengths pred (lambda (_) #f))
                                labels
                                (intset-add! labels pred)))
                          (intmap-ref preds label)
                          labels))
                 labels
                 empty-intset))
  (define (visit labels lengths length)
    (let ((lengths (add-lengths labels lengths length)))
      (values (compute-next labels lengths) lengths (1+ length))))
  (match (intmap-ref conts kfun)
    (($ $kfun src meta self tail clause)
     (worklist-fold visit (intset-add empty-intset tail) empty-intmap 0))))

;; Topologically sort the continuation tree starting at k0, using
;; reverse post-order numbering.
(define (sort-labels-locally conts k0 path-lengths)
  (let ((order '())
        (visited empty-intset))
    (define (visit k)
      (define (maybe-visit k)
        (unless (intset-ref visited k)
          (visit k)))
      (define (visit-successors k)
        (match (intmap-ref conts k)
          (($ $kargs names syms ($ $continue k src exp))
           (match exp
             (($ $prompt escape? tag handler)
              (maybe-visit handler)
              (maybe-visit k))
             (($ $branch kt)
              ;; Visit the successor with the shortest path length
              ;; to the tail first, so that if the branches are
              ;; unsorted, the longer path length will appear
              ;; first.  This will move a loop exit out of a loop.
              (let ((k-len (intmap-ref path-lengths k
                                       (lambda (_) #f)))
                    (kt-len (intmap-ref path-lengths kt
                                        (lambda (_) #f))))
                (cond
                 ((if kt-len
                      (or (not k-len)
                          (< k-len kt-len)
                          ;; If the path lengths are the
                          ;; same, preserve original order
                          ;; to avoid squirreliness.
                          (and (= k-len kt-len) (< kt k)))
                      (if k-len #f (< kt k)))
                  (maybe-visit k)
                  (maybe-visit kt))
                 (else
                  (maybe-visit kt)
                  (maybe-visit k)))))
             (_
              (maybe-visit k))))
          (($ $kreceive arity k) (maybe-visit k))
          (($ $kclause arity kbody kalt)
           (when kalt (visit kalt))
           (maybe-visit kbody))
          (($ $kfun src meta self tail clause)
           (visit tail)
           (when clause (visit clause)))
          (_ #f)))

      ;; Mark this continuation as visited.
      (set! visited (intset-add! visited k))

      ;; Visit unvisited successors.
      (visit-successors k)

      ;; Add k to the reverse post-order.
      (set! order (cons k order)))

    ;; Recursively visit all continuations reachable from k0.
    (visit k0)

    ;; Return the sorted order.
    order))

(define (compute-renaming conts kfun)
  ;; labels := old -> new
  ;; vars := old -> new
  (define *next-label* -1)
  (define *next-var* -1)
  (define (rename-label label labels)
    (set! *next-label* (1+ *next-label*))
    (intmap-add! labels label *next-label*))
  (define (rename-var sym vars)
    (set! *next-var* (1+ *next-var*))
    (intmap-add! vars sym *next-var*))
  (define (rename label labels vars)
    (values (rename-label label labels)
            (match (intmap-ref conts label)
              (($ $kargs names syms exp)
               (fold1 rename-var syms vars))
              (($ $kfun src meta self tail clause)
               (rename-var self vars))
              (_ vars))))
  (define (visit-nested-funs k labels vars)
    (match (intmap-ref conts k)
      (($ $kargs names syms ($ $continue k src ($ $fun kfun)))
       (visit-fun kfun labels vars))
      (($ $kargs names syms ($ $continue k src ($ $rec names* syms*
                                                  (($ $fun kfun) ...))))
       (fold2 visit-fun kfun labels vars))
      (_ (values labels vars))))
  (define (visit-fun kfun labels vars)
    (let* ((preds (compute-predecessors conts kfun))
           (path-lengths (compute-tail-path-lengths conts kfun preds))
           (order (sort-labels-locally conts kfun path-lengths)))
      ;; First rename locally, then recurse on nested functions.
      (let-values (((labels vars) (fold2 rename order labels vars)))
        (fold2 visit-nested-funs order labels vars))))
  (let-values (((labels vars) (visit-fun kfun empty-intmap empty-intmap)))
    (values (persistent-intmap labels) (persistent-intmap vars))))

(define* (renumber conts #:optional (kfun 0))
  (let-values (((label-map var-map) (compute-renaming conts kfun)))
    (define (rename-label label) (intmap-ref label-map label))
    (define (rename-var var) (intmap-ref var-map var))
    (define (rename-exp exp)
      (rewrite-exp exp
        ((or ($ $const) ($ $prim)) ,exp)
        (($ $closure k nfree)
         ($closure (rename-label k) nfree))
        (($ $fun body)
         ($fun (rename-label body)))
        (($ $rec names vars funs)
         ($rec names (map rename-var vars) (map rename-exp funs)))
        (($ $values args)
         ($values ,(map rename-var args)))
        (($ $call proc args)
         ($call (rename-var proc) ,(map rename-var args)))
        (($ $callk k proc args)
         ($callk (rename-label k) (rename-var proc) ,(map rename-var args)))
        (($ $branch kt exp)
         ($branch (rename-label kt) ,(rename-exp exp)))
        (($ $primcall name args)
         ($primcall name ,(map rename-var args)))
        (($ $prompt escape? tag handler)
         ($prompt escape? (rename-var tag) (rename-label handler)))))
    (define (rename-arity arity)
      (match arity
        (($ $arity req opt rest () aok?)
         arity)
        (($ $arity req opt rest kw aok?)
         (match kw
           (() arity)
           (((kw kw-name kw-var) ...)
            (let ((kw (map list kw kw-name (map rename-var kw-var))))
              (make-$arity req opt rest kw aok?)))))))
    (persistent-intmap
     (intmap-fold
      (lambda (old-k new-k out)
        (intmap-add!
         out
         new-k
         (rewrite-cont (intmap-ref conts old-k)
                       (($ $kargs names syms ($ $continue k src exp))
                        ($kargs names (map rename-var syms)
                          ($continue (rename-label k) src ,(rename-exp exp))))
                       (($ $kreceive ($ $arity req () rest () #f) k)
                        ($kreceive req rest (rename-label k)))
                       (($ $ktail)
                        ($ktail))
                       (($ $kfun src meta self tail clause)
                        ($kfun src meta (rename-var self) (rename-label tail)
                          (and clause (rename-label clause))))
                       (($ $kclause arity body alternate)
                        ($kclause ,(rename-arity arity) (rename-label body)
                                  (and alternate (rename-label alternate)))))))
      label-map
      empty-intmap))))
