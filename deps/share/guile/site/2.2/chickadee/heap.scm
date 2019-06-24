;;; Chickadee Game Toolkit
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

(define-module (chickadee heap)
  #:use-module (ice-9 format)
  #:use-module (rnrs base)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-43)
  #:export (make-heap
            heap?
            heap-empty?
            heap-size
            heap-min
            heap-insert!
            heap-remove!
            heap-clear!))

;; A binary heap.
(define-record-type <heap>
  (%make-heap vector size <)
  heap?
  (vector heap-vector set-heap-vector!)
  (size heap-size set-heap-size!)
  (< heap-<))

(define (display-heap heap port)
  (format port "#<heap size: ~d>" (heap-size heap)))

(set-record-type-printer! <heap> display-heap)

(define* (make-heap #:optional (< <))
  "Create a new heap that uses the < procedure to determine order."
  (%make-heap (make-vector 32 #f) 0 <))

(define (heap-empty? heap)
  "Return #t if HEAP is empty."
  (zero? (heap-size heap)))

(define (heap-capacity heap)
  (1- (vector-length (heap-vector heap))))

(define (heap-full? heap)
  (= (heap-size heap) (heap-capacity heap)))

(define (double-heap-capacity! heap)
  (let* ((old-vec (heap-vector heap))
         (new-vec (make-vector (* (vector-length old-vec) 2) #f)))
    (vector-copy! new-vec 0 old-vec)
    (set-heap-vector! heap new-vec)))

(define (heap-min heap)
  "Return the minimum element of HEAP."
  (if (zero? (heap-size heap))
      (error "empty heap" heap)
      (vector-ref (heap-vector heap) 1)))

(define (heap-set! heap i item)
  (vector-set! (heap-vector heap) i item))

(define (heap-ref heap i)
  (vector-ref (heap-vector heap) i))

(define (heap-insert! heap item)
  "Add ITEM to HEAP."
  (when (heap-full? heap)
    (double-heap-capacity! heap))
  (let ((hole (1+ (heap-size heap)))
        (< (heap-< heap)))
    (set-heap-size! heap hole)
    (let loop ((hole hole))
      (let* ((parent-hole (div hole 2))
             (parent-item (heap-ref heap parent-hole)))
        (if (and (> hole 1) (< item parent-item))
            (begin
              (heap-set! heap hole parent-item)
              (loop parent-hole))
            (heap-set! heap hole item))))))

(define (heap-remove! heap)
  "Remove the minimum element of HEAP."
  (let ((size (1- (heap-size heap)))
        (< (heap-< heap)))
    (define (finish hole)
      (heap-set! heap hole (heap-ref heap (heap-size heap)))
      (set-heap-size! heap size)
      *unspecified*)

    (define (leaf? hole)
      (> (* hole 2) size))

    (define (smallest-child hole)
      (let ((left-child (* hole 2))
            (right-child (1+ (* hole 2))))
        (if (or (= left-child size)
                (< (heap-ref heap left-child) (heap-ref heap right-child)))
            left-child
            right-child)))

    (heap-set! heap 1 (heap-ref heap (heap-size heap)))

    (let loop ((hole 1))
      (if (leaf? hole)
          (finish hole)
          (let ((child (smallest-child hole)))
            (if (< (heap-ref heap hole) (heap-ref heap child))
                (finish hole)
                (begin
                  (heap-set! heap hole (heap-ref heap child))
                  (loop child))))))))

(define (heap-clear! heap)
  "Remove all elements from HEAP."
  (vector-fill! (heap-vector heap) #f)
  (set-heap-size! heap 0))
