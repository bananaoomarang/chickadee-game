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

;;; Commentary:
;;
;; Useful representation of 3D rotations.
;;
;;; Code:

(define-module (chickadee math quaternion)
  #:use-module (chickadee math)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:export (quaternion
            quaternion?
            quaternion-w
            quaternion-x
            quaternion-y
            quaternion-z
            make-identity-quaternion))

(define-record-type <quaternion>
  (wrap-quaternion bv pointer)
  quaternion?
  (bv unwrap-quaternion)
  (pointer quaternion-pointer set-quaternion-pointer!))

(define (quaternion->pointer q)
  "Return a foreign pointer to Q."
  ;; Create foreign pointer lazily.
  (or (quaternion-pointer q)
      (let ((pointer (bytevector->pointer (unwrap-quaternion q))))
        (set-quaternion-pointer! q pointer)
        pointer)))

(define-inlinable (quaternion-ref q i)
  (f32vector-ref (unwrap-quaternion q) i))

(define-inlinable (quaternion-set! q i x)
  (f32vector-set! (unwrap-quaternion q) i x))

(define-syntax-rule (with-new-quaternion name body ...)
  (let ((name (wrap-quaternion (f32vector 0.0 0.0 0.0 0.0) #f)))
    body ... name))

(define-inlinable (quaternion x y z w)
  "Return a new quaternion with values X, Y, Z, and W."
  (with-new-quaternion q
    (quaternion-set! q 0 x)
    (quaternion-set! q 1 y)
    (quaternion-set! q 2 z)
    (quaternion-set! q 3 w)))

(define-inlinable (make-identity-quaternion)
  "Return the identity quaternion."
  (quaternion 0.0 0.0 0.0 1.0))

(define-inlinable (make-null-quaternion)
  (quaternion 0.0 0.0 0.0 0.0))

(define-inlinable (quaternion-x q)
  "Return the X coordinate of the quaternion Q."
  (quaternion-ref q 0))

(define-inlinable (quaternion-y q)
  "Return the Y coordinate of the quaternion Q."
  (quaternion-ref q 1))

(define-inlinable (quaternion-z q)
  "Return the Z coordinate of the quaternion Q."
  (quaternion-ref q 2))

(define-inlinable (quaternion-w q)
  "Return the W coordinate of the quaternion Q."
  (quaternion-ref q 3))

(define (display-quaternion q port)
  (format port "#<quaterion ~f ~f ~f ~f>"
          (quaternion-x q)
          (quaternion-y q)
          (quaternion-z q)
          (quaternion-w q)))

(set-record-type-printer! <quaternion> display-quaternion)

(define-inlinable (quaternion-magnitude q)
  "Return the magnitude of the quaternion Q."
  (sqrt
   (+ (square (quaternion-w q))
      (square (quaternion-x q))
      (square (quaternion-y q))
      (square (quaternion-z q)))))
