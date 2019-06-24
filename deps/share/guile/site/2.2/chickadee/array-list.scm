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

(define-module (chickadee array-list)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-43)
  #:export (make-array-list
            array-list
            array-list?
            array-list-empty?
            array-list-size
            array-list-ref
            array-list-set!
            array-list-push!
            array-list-pop!
            array-list-clear!
            array-list-for-each))

(define-record-type <array-list>
  (%make-array-list vector size)
  array-list?
  (vector array-list-vector set-array-list-vector!)
  (size array-list-size set-array-list-size!))

(define (display-array-list array-list port)
  (display "<array-list" port)
  (array-list-for-each (lambda (i item)
                         (display " " port)
                         (display item port))
                       array-list)
  (display ">" port))

(set-record-type-printer! <array-list> display-array-list)

(define (make-array-list)
  (%make-array-list (make-vector 32) 0))

(define (array-list . items)
  (let ((l (make-array-list)))
    (for-each (lambda (item)
                (array-list-push! l item))
              items)
    l))

(define (array-list-capacity array-list)
  (vector-length (array-list-vector array-list)))

(define (array-list-full? array-list)
  (= (array-list-size array-list)
     (array-list-capacity array-list)))

(define (array-list-empty? array-list)
  (zero? (array-list-size array-list)))

(define (expand-array-list! array-list)
  (let* ((old-vec (array-list-vector array-list))
         (new-size (* (vector-length old-vec) 2))
         (new-vec (make-vector new-size)))
    (vector-copy! new-vec 0 old-vec)
    (set-array-list-vector! array-list new-vec)))

(define (array-list-ref array-list i)
  (vector-ref (array-list-vector array-list) i))

(define (array-list-set! array-list i x)
  (vector-set! (array-list-vector array-list) i x))

(define (array-list-push! array-list item)
  (when (array-list-full? array-list)
    (expand-array-list! array-list))
  (let ((index (array-list-size array-list)))
    (set-array-list-size! array-list (1+ index))
    (array-list-set! array-list index item)))

(define (array-list-pop! array-list)
  (let* ((index (1- (array-list-size array-list)))
         (item (array-list-ref array-list index)))
    (set-array-list-size! array-list index)
    item))

(define (array-list-clear! array-list)
  (set-array-list-size! array-list 0)
  *unspecified*)

(define (array-list-for-each proc array-list)
  (let ((size (array-list-size array-list))
        (vec (array-list-vector array-list)))
    (let loop ((i 0))
      (when (< i size)
        (proc i (vector-ref vec i))
        (loop (1+ i))))))
