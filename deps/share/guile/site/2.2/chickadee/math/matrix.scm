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

(define-module (chickadee math matrix)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-4)
  #:use-module (system foreign)
  #:use-module (chickadee math)
  #:use-module (chickadee math quaternion)
  #:use-module (chickadee math rect)
  #:use-module (chickadee math vector)
  #:export (make-matrix4
            make-null-matrix4
            matrix4?
            matrix4-mult!
            matrix4*
            matrix4-identity!
            make-identity-matrix4
            orthographic-projection
            perspective-projection
            matrix4-translate!
            matrix4-translate
            matrix4-scale!
            matrix4-scale
            matrix4-rotate!
            matrix4-rotate
            matrix4-rotate-z!
            matrix4-rotate-z
            matrix4-2d-transform!
            transform-x
            transform-y
            transform!))

;; 4x4 matrix
(define-record-type <matrix4>
  (%make-matrix4 bv ptr)
  matrix4?
  (bv matrix4-bv)
  (ptr matrix4-ptr))

(define-inlinable (matrix-set! matrix row column x)
  (f32vector-set! matrix (+ (* row 4) column) x))

(define-inlinable (matrix-ref matrix row column)
  (f32vector-ref matrix (+ (* row 4) column)))

(define (display-matrix4 matrix port)
  (let ((m (matrix4-bv matrix)))
    (format port
            "#<matrix4 [[~f ~f ~f ~f] [~f ~f ~f ~f] [~f ~f ~f ~f] [~f ~f ~f ~f]]>"
            (matrix-ref m 0 0)
            (matrix-ref m 0 1)
            (matrix-ref m 0 2)
            (matrix-ref m 0 3)
            (matrix-ref m 1 0)
            (matrix-ref m 1 1)
            (matrix-ref m 1 2)
            (matrix-ref m 1 3)
            (matrix-ref m 2 0)
            (matrix-ref m 2 1)
            (matrix-ref m 2 2)
            (matrix-ref m 2 3)
            (matrix-ref m 3 0)
            (matrix-ref m 3 1)
            (matrix-ref m 3 2)
            (matrix-ref m 3 3))))

(set-record-type-printer! <matrix4> display-matrix4)

(define (init-matrix4 matrix
                      aa ab ac ad
                      ba bb bc bd
                      ca cb cc cd
                      da db dc dd)
  (let ((bv (matrix4-bv matrix)))
    (matrix-set! bv 0 0 aa)
    (matrix-set! bv 0 1 ab)
    (matrix-set! bv 0 2 ac)
    (matrix-set! bv 0 3 ad)
    (matrix-set! bv 1 0 ba)
    (matrix-set! bv 1 1 bb)
    (matrix-set! bv 1 2 bc)
    (matrix-set! bv 1 3 bd)
    (matrix-set! bv 2 0 ca)
    (matrix-set! bv 2 1 cb)
    (matrix-set! bv 2 2 cc)
    (matrix-set! bv 2 3 cd)
    (matrix-set! bv 3 0 da)
    (matrix-set! bv 3 1 db)
    (matrix-set! bv 3 2 dc)
    (matrix-set! bv 3 3 dd)))

(define (make-null-matrix4)
  (let ((bv (make-f32vector 16)))
    (%make-matrix4 bv (bytevector->pointer bv))))

(define (make-matrix4 aa ab ac ad
                      ba bb bc bd
                      ca cb cc cd
                      da db dc dd)
  "Return a new 4x4 matrix initialized with the given 16 values in
column-major format."
  (let ((matrix (make-null-matrix4)))
    (init-matrix4 matrix
                  aa ab ac ad
                  ba bb bc bd
                  ca cb cc cd
                  da db dc dd)
    matrix))

(define (matrix4-mult! dest a b)
  "Multiply matrices A and B, storing the result in DEST."
  (let ((m1 (matrix4-bv a))
        (m2 (matrix4-bv b))
        (m3 (matrix4-bv dest)))
    (let ((m1-0-0 (matrix-ref m1 0 0))
          (m1-0-1 (matrix-ref m1 0 1))
          (m1-0-2 (matrix-ref m1 0 2))
          (m1-0-3 (matrix-ref m1 0 3))
          (m1-1-0 (matrix-ref m1 1 0))
          (m1-1-1 (matrix-ref m1 1 1))
          (m1-1-2 (matrix-ref m1 1 2))
          (m1-1-3 (matrix-ref m1 1 3))
          (m1-2-0 (matrix-ref m1 2 0))
          (m1-2-1 (matrix-ref m1 2 1))
          (m1-2-2 (matrix-ref m1 2 2))
          (m1-2-3 (matrix-ref m1 2 3))
          (m1-3-0 (matrix-ref m1 3 0))
          (m1-3-1 (matrix-ref m1 3 1))
          (m1-3-2 (matrix-ref m1 3 2))
          (m1-3-3 (matrix-ref m1 3 3))
          (m2-0-0 (matrix-ref m2 0 0))
          (m2-0-1 (matrix-ref m2 0 1))
          (m2-0-2 (matrix-ref m2 0 2))
          (m2-0-3 (matrix-ref m2 0 3))
          (m2-1-0 (matrix-ref m2 1 0))
          (m2-1-1 (matrix-ref m2 1 1))
          (m2-1-2 (matrix-ref m2 1 2))
          (m2-1-3 (matrix-ref m2 1 3))
          (m2-2-0 (matrix-ref m2 2 0))
          (m2-2-1 (matrix-ref m2 2 1))
          (m2-2-2 (matrix-ref m2 2 2))
          (m2-2-3 (matrix-ref m2 2 3))
          (m2-3-0 (matrix-ref m2 3 0))
          (m2-3-1 (matrix-ref m2 3 1))
          (m2-3-2 (matrix-ref m2 3 2))
          (m2-3-3 (matrix-ref m2 3 3)))
      (matrix-set! m3 0 0
                   (+ (* m1-0-0 m2-0-0)
                      (* m1-0-1 m2-1-0)
                      (* m1-0-2 m2-2-0)
                      (* m1-0-3 m2-3-0)))
      (matrix-set! m3 0 1
                   (+ (* m1-0-0 m2-0-1)
                      (* m1-0-1 m2-1-1)
                      (* m1-0-2 m2-2-1)
                      (* m1-0-3 m2-3-1)))
      (matrix-set! m3 0 2
                   (+ (* m1-0-0 m2-0-2)
                      (* m1-0-1 m2-1-2)
                      (* m1-0-2 m2-2-2)
                      (* m1-0-3 m2-3-2)))
      (matrix-set! m3 0 3
                   (+ (* m1-0-0 m2-0-3)
                      (* m1-0-1 m2-1-3)
                      (* m1-0-2 m2-2-3)
                      (* m1-0-3 m2-3-3)))
      (matrix-set! m3 1 0
                   (+ (* m1-1-0 m2-0-0)
                      (* m1-1-1 m2-1-0)
                      (* m1-1-2 m2-2-0)
                      (* m1-1-3 m2-3-0)))
      (matrix-set! m3 1 1
                   (+ (* m1-1-0 m2-0-1)
                      (* m1-1-1 m2-1-1)
                      (* m1-1-2 m2-2-1)
                      (* m1-1-3 m2-3-1)))
      (matrix-set! m3 1 2
                   (+ (* m1-1-0 m2-0-2)
                      (* m1-1-1 m2-1-2)
                      (* m1-1-2 m2-2-2)
                      (* m1-1-3 m2-3-2)))
      (matrix-set! m3 1 3
                   (+ (* m1-1-0 m2-0-3)
                      (* m1-1-1 m2-1-3)
                      (* m1-1-2 m2-2-3)
                      (* m1-1-3 m2-3-3)))
      (matrix-set! m3 2 0
                   (+ (* m1-2-0 m2-0-0)
                      (* m1-2-1 m2-1-0)
                      (* m1-2-2 m2-2-0)
                      (* m1-2-3 m2-3-0)))
      (matrix-set! m3 2 1
                   (+ (* m1-2-0 m2-0-1)
                      (* m1-2-1 m2-1-1)
                      (* m1-2-2 m2-2-1)
                      (* m1-2-3 m2-3-1)))
      (matrix-set! m3 2 2
                   (+ (* m1-2-0 m2-0-2)
                      (* m1-2-1 m2-1-2)
                      (* m1-2-2 m2-2-2)
                      (* m1-2-3 m2-3-2)))
      (matrix-set! m3 2 3
                   (+ (* m1-2-0 m2-0-3)
                      (* m1-2-1 m2-1-3)
                      (* m1-2-2 m2-2-3)
                      (* m1-2-3 m2-3-3)))
      (matrix-set! m3 3 0
                   (+ (* m1-3-0 m2-0-0)
                      (* m1-3-1 m2-1-0)
                      (* m1-3-2 m2-2-0)
                      (* m1-3-3 m2-3-0)))
      (matrix-set! m3 3 1
                   (+ (* m1-3-0 m2-0-1)
                      (* m1-3-1 m2-1-1)
                      (* m1-3-2 m2-2-1)
                      (* m1-3-3 m2-3-1)))
      (matrix-set! m3 3 2
                   (+ (* m1-3-0 m2-0-2)
                      (* m1-3-1 m2-1-2)
                      (* m1-3-2 m2-2-2)
                      (* m1-3-3 m2-3-2)))
      (matrix-set! m3 3 3
                   (+ (* m1-3-0 m2-0-3)
                      (* m1-3-1 m2-1-3)
                      (* m1-3-2 m2-2-3)
                      (* m1-3-3 m2-3-3))))))

(define (matrix4-copy matrix)
  (let ((bv (bytevector-copy (matrix4-bv matrix))))
    (%make-matrix4 bv (bytevector->pointer bv))))

(define (matrix4* . matrices)
  "Return the product of MATRICES."
  (match matrices
    (() (make-identity-matrix4))
    ((a b)
     (let ((result (make-identity-matrix4)))
       (matrix4-mult! result a b)
       result))
    ((first . rest)
     (let loop ((temp (make-identity-matrix4))
                (prev (matrix4-copy first))
                (matrices rest))
       (match matrices
         (() prev)
         ((current . rest)
          (matrix4-mult! temp prev current)
          (loop prev temp rest)))))))

(define (matrix4-identity! matrix)
  (init-matrix4 matrix
                1.0 0.0 0.0 0.0
                0.0 1.0 0.0 0.0
                0.0 0.0 1.0 0.0
                0.0 0.0 0.0 1.0))

(define (make-identity-matrix4)
  (let ((matrix (make-null-matrix4)))
    (matrix4-identity! matrix)
    matrix))

(define (orthographic-projection left right top bottom near far)
  "Return a new matrix4 that represents an orthographic projection for
the horizontal clipping plane LEFT and RIGHT, the vertical clipping
plane TOP and BOTTOM, and the depth clipping plane NEAR and FAR."
  (make-matrix4 (/ 2 (- right left)) 0.0 0.0 0.0
                0.0 (/ 2 (- top bottom)) 0.0 0.0
                0.0 0.0 (/ 2 (- far near)) 0.0
                (- (/ (+ right left) (- right left)))
                (- (/ (+ top bottom) (- top bottom)))
                (- (/ (+ far near) (- far near)))
                1.0))

(define (perspective-projection field-of-vision aspect-ratio near far)
  "Return a new matrix4 that represents a perspective projection with
a FIELD-OF-VISION in radians, the desired ASPECT-RATIO, and the depth
clipping plane NEAR and FAR."
  (let ((f (cotan (/ field-of-vision 2))))
    (make-matrix4 (/ f aspect-ratio) 0 0 0
                  0 f 0 0
                  0 0 (/ (+ far near) (- near far)) -1
                  0 0 (/ (* 2 far near) (- near far)) 0)))

(define (matrix4-translate! matrix v)
  (cond
   ((vec2? v)
    (init-matrix4 matrix
                  1.0 0.0 0.0 0.0
                  0.0 1.0 0.0 0.0
                  0.0 0.0 1.0 0.0
                  (vec2-x v) (vec2-y v) 0.0 1.0))
   ((rect? v)
    (init-matrix4 matrix
                  1.0 0.0 0.0 0.0
                  0.0 1.0 0.0 0.0
                  0.0 0.0 1.0 0.0
                  (rect-x v) (rect-y v) 0.0 1.0))
   ((vec3? v)
    (init-matrix4 matrix
                  1.0 0.0 0.0 0.0
                  0.0 1.0 0.0 0.0
                  0.0 0.0 1.0 0.0
                  (vec3-x v) (vec3-y v) (vec3-z v) 1.0))))

(define (matrix4-translate v)
  (let ((matrix (make-null-matrix4)))
    (matrix4-translate! matrix v)
    matrix))

(define (matrix4-scale! matrix s)
  (init-matrix4  matrix
                 s    0.0  0.0  0.0
                 0.0  s    0.0  0.0
                 0.0  0.0  s    0.0
                 0.0  0.0  0.0  1.0))

(define (matrix4-scale s)
  (let ((matrix (make-null-matrix4)))
    (matrix4-scale! matrix s)
    matrix))

(define (matrix4-rotate! matrix q)
  "Return a new rotation matrix for the quaternion Q."
  (let ((x (quaternion-x q))
        (y (quaternion-y q))
        (z (quaternion-z q))
        (w (quaternion-w q)))
    (init-matrix4 matrix
                  (- 1.0 (* 2.0 (* y y)) (* 2.0 (* z z)))
                  (- (* 2.0 x y) (* 2.0 w z))
                  (+ (* 2.0 x z) (* 2.0 w y))
                  0
                  (+ (* 2.0 x y) (* 2.0 w z))
                  (- 1.0 (* 2.0 (* x x)) (* 2.0 (* z z)))
                  (- (* 2.0 y z) (* 2.0 w x))
                  0
                  (- (* 2.0 x z) (* 2.0 w y))
                  (+ (* 2.0 y z) (* 2.0 w x))
                  (- 1.0 (* 2.0 (* x x)) (* 2.0 (* y y)))
                  0.0
                  0.0
                  0.0
                  0.0
                  1.0)))

(define (matrix4-rotate q)
  (let ((matrix (make-null-matrix4)))
    (matrix4-rotate! matrix q)
    matrix))

(define (matrix4-rotate-z! matrix angle)
  (init-matrix4 matrix
                (cos angle) (- (sin angle)) 0.0 0.0
                (sin angle) (cos angle)     0.0 0.0
                0.0         0.0             1.0 0.0
                0.0         0.0             0.0 1.0))

(define (matrix4-rotate-z angle)
  "Return a new matrix that rotates the Z axis by ANGLE radians."
  (let ((matrix (make-null-matrix4)))
    (matrix4-rotate-z! matrix angle)
    matrix))

(define matrix4-2d-transform!
  (let ((tmp (make-null-matrix4))
        (offset (vec2 0.0 0.0))
        (null-vec2 (vec2 0.0 0.0))
        (default-scale (vec2 1.0 1.0)))
    (lambda* (matrix
              #:key
              (origin null-vec2)
              (position null-vec2)
              (rotation 0.0)
              (scale default-scale)
              (skew null-vec2))
      "Store in MATRIX the transformation described by POSITION, a 2D
vector or rect, ROTATION, a scalar representing a rotation about the Z
axis, SCALE, a 2D vector, and SKEW, a 2D vector.  The transformation
happens with respect to ORIGIN, a 2D vector."
      (let* ((bv (matrix4-bv matrix))
             (x (vec2-x position))
             (y (vec2-y position))
             (ox (vec2-x origin))
             (oy (vec2-y origin))
             (sx (vec2-x scale))
             (sy (vec2-y scale))
             (kx (vec2-x skew))
             (ky (vec2-y skew))
             (c (cos rotation))
             (s (sin rotation))
             (q (- (* c sx) (* s sy ky)))
             (r (+ (* s sx) (* c sy ky)))
             (s (- (* c sx kx) (* s sy)))
             (t (+ (* s sx kx) (* c sy))))
        (bytevector-fill! bv 0)
        (f32vector-set! bv 10 1.0)
        (f32vector-set! bv 15 1.0)
        (f32vector-set! bv 0 q)
        (f32vector-set! bv 1 r)
        (f32vector-set! bv 4 s)
        (f32vector-set! bv 5 t)
        (f32vector-set! bv 12 (- x (* ox q) (* oy s)))
        (f32vector-set! bv 13 (- y (* ox r) (* oy t)))))))

(define-inlinable (transform-x matrix x y)
  (let ((bv (matrix4-bv matrix)))
    (+ (* x (matrix-ref bv 0 0))
       (* y (matrix-ref bv 1 0))
       (matrix-ref bv 3 0))))

(define-inlinable (transform-y matrix x y)
  (let ((bv (matrix4-bv matrix)))
    (+ (* x (matrix-ref bv 0 1))
       (* y (matrix-ref bv 1 1))
       (matrix-ref bv 3 1))))

(define-inlinable (transform! matrix v)
  (let ((x (vec2-x v))
        (y (vec2-y v)))
    (set-vec2-x! v (transform-x matrix x y))
    (set-vec2-y! v (transform-y matrix x y))))
