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

(define-module (chickadee math vector)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:use-module (chickadee math)
  #:export (vec2
            vec2/polar
            vec2?
            vec2->pointer
            vec2-copy
            vec2-copy!
            vec2-x
            vec2-y
            vec2-magnitude
            vec2-dot-product
            vec2-normalize
            set-vec2-x!
            set-vec2-y!
            set-vec2!
            vec2-normalize!
            vec2-mult!
            vec2-add!
            vec2-sub!
            vec2*
            vec2+
            vec2-
            vec3
            vec3?
            vec3->pointer
            vec3-copy
            vec3-copy!
            vec3-x
            vec3-y
            vec3-z
            vec3-magnitude
            vec3-dot-product
            vec3-normalize
            set-vec3-x!
            set-vec3-y!
            set-vec3!
            vec3-normalize!
            vec3-mult!
            vec3-add!
            vec3-sub!))

(define-record-type <vec2>
  (wrap-vec2 bv pointer)
  vec2?
  (bv unwrap-vec2)
  (pointer vec2-pointer set-vec2-pointer!))

(define-record-type <vec3>
  (wrap-vec3 bv pointer)
  vec3?
  (bv unwrap-vec3)
  (pointer vec3-pointer set-vec3-pointer!))

(define (vec2->pointer v)
  "Return a foreign pointer to V."
  ;; Create foreign pointer lazily.
  (or (vec2-pointer v)
      (let ((pointer (bytevector->pointer (unwrap-vec2 v))))
        (set-vec2-pointer! v pointer)
        pointer)))

(define (vec3->pointer v)
  "Return a foreign pointer to V."
  ;; Create foreign pointer lazily.
  (or (vec3-pointer v)
      (let ((pointer (bytevector->pointer (unwrap-vec3 v))))
        (set-vec3-pointer! v pointer)
        pointer)))

(define (make-null-vec2)
  (wrap-vec2 (make-f32vector 2) #f))

(define (make-null-vec3)
  (wrap-vec3 (make-f32vector 3) #f))

(define-syntax-rule (with-new-vec2 name body ...)
  (let ((name (make-null-vec2))) body ... name))

(define-syntax-rule (with-new-vec3 name body ...)
  (let ((name (make-null-vec3))) body ... name))

(define-inlinable (vec2-ref v i)
  (f32vector-ref (unwrap-vec2 v) i))

(define-inlinable (vec3-ref v i)
  (f32vector-ref (unwrap-vec3 v) i))

(define-inlinable (vec2-set! v i x)
  (f32vector-set! (unwrap-vec2 v) i x))

(define-inlinable (vec3-set! v i x)
  (f32vector-set! (unwrap-vec3 v) i x))

(define-inlinable (vec2 x y)
  "Return a new vec2 with coordinates (X, Y)."
  (with-new-vec2 v
    (vec2-set! v 0 x)
    (vec2-set! v 1 y)))

(define-inlinable (vec3 x y z)
  (with-new-vec3 v
    (vec3-set! v 0 x)
    (vec3-set! v 1 y)
    (vec3-set! v 2 z)))

(define-inlinable (vec2/polar r theta)
  "Return a new vec2 containing the Cartesian representation of the
polar coordinate (R, THETA)."
  (vec2 (* r (cos theta)) (* r (sin theta))))

(define-inlinable (vec2-x v)
  "Return the x coordinate of the vec2 V."
  (vec2-ref v 0))

(define-inlinable (vec3-x v)
  "Return the x coordinate of the vec3 V."
  (vec3-ref v 0))

(define-inlinable (vec2-y v)
  "Return the y coordinate of the vec2 V."
  (vec2-ref v 1))

(define-inlinable (vec3-y v)
  "Return the y coordinate of the vec3 V."
  (vec3-ref v 1))

(define-inlinable (vec3-z v)
  "Return the z coordinate of the vec3 V."
  (vec3-ref v 2))

(define-inlinable (set-vec2-x! v x)
  (vec2-set! v 0 x))

(define-inlinable (set-vec3-x! v x)
  (vec3-set! v 0 x))

(define-inlinable (set-vec2-y! v y)
  (vec2-set! v 1 y))

(define-inlinable (set-vec3-y! v y)
  (vec3-set! v 1 y))

(define-inlinable (set-vec3-z! v z)
  (vec3-set! v 2 z))

(define-inlinable (set-vec2! v x y)
  (set-vec2-x! v x)
  (set-vec2-y! v y))

(define-inlinable (set-vec3! v x y z)
  (set-vec3-x! v x)
  (set-vec3-y! v y)
  (set-vec3-z! v z))

(define (display-vec2 v port)
  (format port "#<vec2 x: ~f y: ~f>" (vec2-x v) (vec2-y v)))

(set-record-type-printer! <vec2> display-vec2)

(define (display-vec3 v port)
  (format port "#<vec3 x: ~f y: ~f z: ~f>" (vec3-x v) (vec3-y v) (vec3-z v)))

(set-record-type-printer! <vec3> display-vec3)

(define (vec2-copy! source-vec2 target-vec2)
  "Copy SOURCE-VEC2 to TARGET-VEC2."
  (set-vec2-x! target-vec2 (vec2-x source-vec2))
  (set-vec2-y! target-vec2 (vec2-y source-vec2)))

(define (vec3-copy! source-vec3 target-vec3)
  "Copy SOURCE-VEC3 to TARGET-VEC3."
  (set-vec3-x! target-vec3 (vec3-x source-vec3))
  (set-vec3-y! target-vec3 (vec3-y source-vec3)))

(define (vec2-copy vec2)
  "Return a new vec2 that is a copy of VEC2."
  (with-new-vec2 new
    (vec2-copy! vec2 new)))

(define (vec3-copy vec3)
  "Return a new vec3 that is a copy of VEC3."
  (with-new-vec3 new
    (vec3-copy! vec3 new)))

(define-inlinable (vec2-magnitude v)
  "Return the magnitude of the vec2 V."
  (sqrt (+ (square (vec2-x v)) (square (vec2-y v)))))

(define-inlinable (vec3-magnitude v)
  "Return the magnitude of the vec3 V."
  (sqrt (+ (square (vec3-x v))
           (square (vec3-y v))
           (square (vec3-z v)))))

(define-inlinable (vec2-dot-product v1 v2)
  "Return the dot product of the vec2s V1 and V2."
  (+ (* (vec2-x v1) (vec2-x v2))
     (* (vec2-y v1) (vec2-y v2))))

(define-inlinable (vec3-dot-product v1 v2)
  "Return the dot product of the vec3s V1 and V2."
  (+ (* (vec3-x v1) (vec3-x v2))
     (* (vec3-y v1) (vec3-y v2))
     (* (vec3-z v1) (vec3-z v2))))

(define-inlinable (vec2-normalize! v)
  "Normalize the vec2 V in-place."
  (unless (and (zero? (vec2-x v)) (zero? (vec2-y v)))
    (let ((m (vec2-magnitude v)))
      (set-vec2-x! v (/ (vec2-x v) m))
      (set-vec2-y! v (/ (vec2-y v) m)))))

(define-inlinable (vec3-normalize! v)
  "Normalize the vec3 V in-place."
  (unless (and (zero? (vec3-x v))
               (zero? (vec3-y v))
               (zero? (vec3-z v)))
    (let ((m (vec3-magnitude v)))
      (set-vec3-x! v (/ (vec3-x v) m))
      (set-vec3-y! v (/ (vec3-y v) m))
      (set-vec3-z! v (/ (vec3-z v) m)))))

(define (vec2-normalize v)
  "Return the normalized form of the vec2 V."
  (with-new-vec2 new
    (vec2-copy! v new)
    (vec2-normalize! new)))

(define (vec3-normalize v)
  "Return the normalized form of the vec3 V."
  (with-new-vec3 new
    (vec3-copy! v new)
    (vec3-normalize! new)))

(define-inlinable (vec2-mult! v x)
  "Multiply the vec2 V by X, a real number or vec2."
  (if (real? x)
      (begin
        (set-vec2-x! v (* (vec2-x v) x))
        (set-vec2-y! v (* (vec2-y v) x)))
      (begin
        (set-vec2-x! v (* (vec2-x v) (vec2-x x)))
        (set-vec2-y! v (* (vec2-y v) (vec2-y x))))))

(define-inlinable (vec3-mult! v x)
  "Multiply the vec3 V by X, a real number or vec3."
  (if (real? x)
      (begin
        (set-vec3-x! v (* (vec3-x v) x))
        (set-vec3-y! v (* (vec3-y v) x))
        (set-vec3-z! v (* (vec3-z v) x)))
      (begin
        (set-vec3-x! v (* (vec3-x v) (vec3-x x)))
        (set-vec3-y! v (* (vec3-y v) (vec3-y x)))
        (set-vec3-z! v (* (vec3-z v) (vec3-z x))))))

(define-inlinable (vec2-add! v x)
  "Add X, a real number or vec2, to the vec2 V."
  (if (real? x)
      (begin
        (set-vec2-x! v (+ (vec2-x v) x))
        (set-vec2-y! v (+ (vec2-y v) x)))
      (begin
        (set-vec2-x! v (+ (vec2-x v) (vec2-x x)))
        (set-vec2-y! v (+ (vec2-y v) (vec2-y x))))))

(define-inlinable (vec3-add! v x)
  "Add X, a real number or vec3, to the vec3 V."
  (if (real? x)
      (begin
        (set-vec3-x! v (+ (vec3-x v) x))
        (set-vec3-y! v (+ (vec3-y v) x))
        (set-vec3-z! v (+ (vec3-z v) x)))
      (begin
        (set-vec3-x! v (+ (vec3-x v) (vec3-x x)))
        (set-vec3-y! v (+ (vec3-y v) (vec3-y x)))
        (set-vec3-z! v (+ (vec3-z v) (vec3-z x))))))

(define-inlinable (vec2-sub! v x)
  "Subtract X, a real number or vec2, from the vec2 V."
  (if (real? x)
      (begin
        (set-vec2-x! v (- (vec2-x v) x))
        (set-vec2-y! v (- (vec2-y v) x)))
      (begin
        (set-vec2-x! v (- (vec2-x v) (vec2-x x)))
        (set-vec2-y! v (- (vec2-y v) (vec2-y x))))))

(define-inlinable (vec3-sub! v x)
  "Subtract X, a real number or vec3, from the vec3 V."
  (if (real? x)
      (begin
        (set-vec3-x! v (- (vec3-x v) x))
        (set-vec3-y! v (- (vec3-y v) x))
        (set-vec3-z! v (- (vec3-z v) x)))
      (begin
        (set-vec3-x! v (- (vec3-x v) (vec3-x x)))
        (set-vec3-y! v (- (vec3-y v) (vec3-y x)))
        (set-vec3-z! v (- (vec3-z v) (vec3-z x))))))

(define-inlinable (vec2* v x)
  "Multiply V by X."
  (let ((new (vec2-copy v)))
    (vec2-mult! new x)
    new))

(define-inlinable (vec2+ v x)
  "Add X to V."
  (let ((new (vec2-copy v)))
    (vec2-add! new x)
    new))

(define-inlinable (vec2- v x)
  "Subtract X from V."
  (let ((new (vec2-copy v)))
    (vec2-sub! new x)
    new))

;; Reader macro for vectors.
(define (read-vec chr port)
  (define (consume-whitespace port)
    (when (char-whitespace? (peek-char port))
      (read-char port)
      (consume-whitespace port)))
  (if (eq? (peek-char port) #\()
      (read-char port)
      (error "expected opening #\\("))
  (consume-whitespace port)
  (let ((x (read port))
        (y (read port)))
    (if (eq? (peek-char port) #\))
        (begin
          (read-char port)
          `(vec2 ,x ,y))
        (let ((z (read port)))
          (consume-whitespace port)
          (if (eq? (peek-char port) #\))
              (begin
                (read-char port)
                `(vec3 ,x ,y ,z))
              (error "expected terminating #\\)"))))))

(read-hash-extend #\v read-vec)
