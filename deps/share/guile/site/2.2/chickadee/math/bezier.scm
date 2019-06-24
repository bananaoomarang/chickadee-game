;;; Chickadee Game Toolkit
;;; Copyright © 2018 David Thompson <davet@gnu.org>
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
;; Cubic Bezier curves in 2D space.
;;
;;; Code:

(define-module (chickadee math bezier)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (chickadee math vector)
  #:export (make-bezier-curve
            bezier-curve?
            bezier-curve-p0
            bezier-curve-p1
            bezier-curve-p2
            bezier-curve-p3
            bezier-curve-point-at!
            bezier-curve-point-at
            bezier-path))

(define-record-type <bezier-curve>
  (make-bezier-curve p0 p1 p2 p3)
  bezier-curve?
  (p0 bezier-curve-p0)
  (p1 bezier-curve-p1)
  (p2 bezier-curve-p2)
  (p3 bezier-curve-p3))

(define (bezier-curve-point-at! dest bezier t)
  "Write the coordinates for BEZIER at T (a value in the range [0, 1])
to the 2D vector DEST."
  (let* ((u (- 1.0 t))
         (tt (* t t))
         (uu (* u u))
         (uuu (* uu u))
         (ttt (* tt t))
         (p0 (bezier-curve-p0 bezier))
         (p1 (bezier-curve-p1 bezier))
         (p2 (bezier-curve-p2 bezier))
         (p3 (bezier-curve-p3 bezier)))
    (set-vec2-x! dest (+ (* uuu (vec2-x p0))
                         (* 3 uu t (vec2-x p1))
                         (* 3 u tt (vec2-x p2))
                         (* ttt (vec2-x p3))))
    (set-vec2-y! dest (+ (* uuu (vec2-y p0))
                         (* 3 uu t (vec2-y p1))
                         (* 3 u tt (vec2-y p2))
                         (* ttt (vec2-y p3))))))

(define (bezier-curve-point-at bezier t)
  "Return the coordinates for BEZIER at T (a value in the range [0,
1]) as a 2D vector."
  (let ((v #v(0.0 0.0)))
    (bezier-curve-point-at! v bezier t)
    v))

(define (bezier-path . control-points)
  "Return a list of connected bezier curves defined by CONTROL-POINTS.
The first curve is defined by the first 4 arguments and every
additional curve thereafter requires 3 additional arguments."
  (match control-points
    ((_) '())
    ((p0 p1 p2 p3 . prest)
     (cons (make-bezier-curve p0 p1 p2 p3)
           (apply bezier-path p3 prest)))))
