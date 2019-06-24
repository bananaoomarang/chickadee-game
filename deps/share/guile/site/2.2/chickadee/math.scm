;;; Chickadee Game Toolkit
;;; Copyright © 2016 David Thompson <davet@gnu.org>
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

(define-module (chickadee math)
  #:export (pi
            pi/2
            cotan
            square
            clamp
            min
            max
            lerp)
  #:replace (min max))

(define pi 3.14159265358979323846)
(define pi/2 (/ pi 2.0))

(define-inlinable (cotan z)
  "Return the cotangent of Z."
  (/ 1.0 (tan z)))

(define-inlinable (square x)
  (* x x))

(define-inlinable (clamp min max x)
  "Restrict X to the range defined by MIN and MAX. Assumes that MIN is
actually less than MAX."
  (cond ((< x min) min)
        ((> x max) max)
        (else x)))

;; Some macro trickery to inline calls to min/max with 2 arguments.
;; We often call min/max on floating point values, so inlining such
;; calls allows the compiler to unbox many of these operations,
;; reducing allocation.
(define-syntax min
  (syntax-rules ()
    ((_ a b)
     (if (< a b) a b))
    ((_ a b ...)
     (let ((m (min b ...)))
       (if (< a m) a m)))))

(define-syntax max
  (syntax-rules ()
    ((_ a b) (if (> a b) a b))
    ((_ a b ...)
     (let ((m (max b ...)))
       (if (> a m) a m)))))

(define-inlinable (lerp start end alpha)
  (+ (* start (- 1.0 alpha))
     (* end alpha)))
