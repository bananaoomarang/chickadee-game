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

(define-module (chickadee math easings)
  #:use-module (chickadee math)
  #:export (linear
            smoothstep
            ease-in-quad
            ease-out-quad
            ease-in-out-quad
            ease-in-cubic
            ease-out-cubic
            ease-in-out-cubic
            ease-in-quart
            ease-out-quart
            ease-in-out-quart
            ease-in-quint
            ease-out-quint
            ease-in-out-quint
            ease-in-sine
            ease-out-sine
            ease-in-out-sine))

(define-inlinable (linear t)
  t)

(define-inlinable (smoothstep t)
  (* t t (- 3 (* 2 t))))

(define-inlinable (ease-in-quad t)
  (* t t))

(define-inlinable (ease-out-quad t)
  (* t (- 2.0 t)))

(define-inlinable (ease-in-out-quad t)
  (if (< t 0.5)
      (* 2.0 t t)
      (- (* (- 4 (* 2.0 t)) t) 1.0)))

(define-inlinable (ease-in-cubic t)
  (* t t t))

(define-inlinable (ease-out-cubic t)
  (let ((t* (- t 1.0)))
    (+ 1.0 (* t* t* t*))))

(define-inlinable (ease-in-out t)
  (if (< t 0.5)
      (* 4.0 t t t)
      (+ 1.0
         (* (- t 1.0)
            (* 2 (- t 2.0))
            (* 2 (- t 2.0))))))

(define-inlinable (ease-in-quart t)
  (* t t t t))

(define-inlinable (ease-out-quart t)
  (let ((t* (- t 1.0)))
    (- 1.0 (* t* t* t* t*))))

(define-inlinable (ease-in-out-quart t)
  (if (< t 0.5)
      (* 8.0 t t t t)
      (let ((t* (- t 1.0)))
        (- 1.0 (* 8.0 t* t* t* t*)))))

(define-inlinable (ease-in-quint t)
  (* t t t t t))

(define-inlinable (ease-out-quint t)
  (let ((t* (- t 1.0)))
    (+ 1.0 (* t* t* t* t* t*))))

(define-inlinable (ease-in-out-quint t)
  (if (< t 0.5)
      (* 16.0 t t t t t)
      (let ((t* (- t 1.0)))
        (+ 1.0 (* 16.0 t* t* t* t* t*)))))

(define-inlinable (ease-in-sine t)
  (+ (* (- t) (cos (* t pi/2))) t))

(define-inlinable (ease-out-sine t)
  (* t (sin (* t pi/2))))

(define-inlinable (ease-in-out-sine t)
  (* (/ t -2)
     (1- (cos (* t pi)))))
