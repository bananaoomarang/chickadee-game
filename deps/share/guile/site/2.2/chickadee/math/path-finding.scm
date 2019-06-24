;;; Copyright © 2017 David Thompson <davet@gnu.org>
;;;
;;; Chickadee is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; Chickadee is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Generalized A* pathfinding algorithm.
;;
;;; Code

(define-module (chickadee math path-finding)
  #:use-module (chickadee heap)
  #:use-module (srfi srfi-9)
  #:export (make-path-finder
            path-finder?
            a*))

(define-record-type <path-finder>
  (%make-path-finder frontier came-from cost-so-far)
  path-finder?
  (frontier path-finder-frontier)
  (came-from path-finder-came-from)
  (cost-so-far path-finder-cost-so-far))

(define (make-path-finder)
  "Create a new path finder object."
  (%make-path-finder (make-heap (lambda (a b) (< (cdr a) (cdr b))))
                     (make-hash-table)
                     (make-hash-table)))

(define (a* path-finder start goal neighbors cost distance)
  "Return a list of nodes forming a path from START to GOAL using
PATH-FINDER.  NEIGHBORS is a procedure that accepts a node and returns
a list of nodes that neighbor it.  COST is a procedure that accepts
two neighboring nodes and returns the cost of moving from the first to
the second as a number.  DISTANCE is a procedure that accepts two
nodes and returns an approximate distance between them."
  (let ((frontier (path-finder-frontier path-finder))
        (came-from (path-finder-came-from path-finder))
        (cost-so-far (path-finder-cost-so-far path-finder)))
    (heap-insert! frontier (cons start 0))
    (hashq-set! came-from start #f)
    (hashq-set! cost-so-far start 0)
    (let loop ()
      (unless (heap-empty? frontier)
        (let ((current (car (heap-min frontier))))
          (heap-remove! frontier)
          (unless (eq? current goal)
            (for-each (lambda (next)
                        (let ((new-cost (+ (hashq-ref cost-so-far current)
                                           (cost current next))))
                          (when (or (not (hashq-ref cost-so-far next))
                                    (< new-cost (hashq-ref cost-so-far next)))
                            (hashq-set! cost-so-far next new-cost)
                            (let ((priority (+ new-cost (distance goal next))))
                              (heap-insert! frontier (cons next priority)))
                            (hashq-set! came-from next current))))
                      (neighbors current))
            (loop)))))
    ;; Walk backwards to build the path from start to goal as a list.
    (let loop ((node goal)
               (path '()))
      (if (eq? node start)
          (cons start path)
          (loop (hashq-ref came-from node) (cons node path))))))
