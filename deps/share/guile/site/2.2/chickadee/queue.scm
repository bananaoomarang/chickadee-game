;;; Chickadee Game Toolkit
;;; Copyright © 2017, 2018 David Thompson <davet@gnu.org>
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

(define-module (chickadee queue)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chickadee array-list)
  #:export (make-queue
            queue?
            queue-length
            queue-empty?
            enqueue!
            dequeue!))

(define-record-type <queue>
  (%make-queue input output)
  queue?
  (input queue-input)
  (output queue-output))

(define (display-queue q port)
  (format port "#<queue length: ~d>" (queue-length q)))

(set-record-type-printer! <queue> display-queue)

(define (make-queue)
  "Return a new, empty queue."
  (%make-queue (make-array-list) (make-array-list)))

(define (queue-length q)
  "Return the number of elements in Q."
  (+ (array-list-size (queue-input q))
     (array-list-size (queue-output q))))

(define (queue-empty? q)
  "Return #t if Q is empty."
  (zero? (queue-length q)))

(define (enqueue! q item)
  "Add ITEM to Q."
  (array-list-push! (queue-input q) item))

(define (dequeue! q)
  "Remove the first element of Q."
  (and (not (queue-empty? q))
       (let ((input (queue-input q))
             (output (queue-output q)))
         (when (array-list-empty? output)
           (let loop ()
             (unless (array-list-empty? input)
               (array-list-push! output (array-list-pop! input))
               (loop))))
         (array-list-pop! output))))
