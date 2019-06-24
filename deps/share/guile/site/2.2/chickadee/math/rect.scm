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

(define-module (chickadee math rect)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-9)
  #:use-module (chickadee math)
  #:use-module (chickadee math vector)
  #:export (make-rect
            rect
            rect?
            rect-copy!
            rect-copy
            rect-x
            rect-y
            rect-width
            rect-height
            rect-area
            rect-left
            rect-right
            rect-top
            rect-bottom
            rect-center-x
            rect-center-y
            rect-clamp-x
            rect-clamp-y
            vec2-clamp-to-rect
            rect-clamp
            rect-move
            rect-move-vec2
            rect-move-by
            rect-move-by-vec2
            rect-inflate
            rect-union
            rect-clip
            set-rect-x!
            set-rect-y!
            set-rect-width!
            set-rect-height!
            rect-move!
            rect-move-vec2!
            rect-move-by!
            rect-move-by-vec2!
            rect-inflate!
            rect-union!
            rect-clip!
            vec2-clamp-to-rect!
            rect-clamp!
            rect-within?
            rect-intersects?
            rect-contains?
            rect-contains-vec2?))

;; This record type just wraps a 4 element f32vector as a workaround
;; for Guile not being able to unbox struct fields.  Since floating
;; point numbers are heap-allocated in Guile, the name of this game is
;; to help the compiler unbox as much floating point math as possible.
;; Doing so greatly reduces allocation and thus improves the user
;; experience because there are less GC pauses.  By using bytevectors
;; and inlining nearly everything, the compiler is able to optimize
;; away a lot of scm->f64 and f64->scm instructions.

(define-record-type <rect>
  (wrap-rect bv)
  rect?
  (bv unwrap-rect))

(define (make-null-rect)
  (wrap-rect (make-f32vector 4)))

(define-syntax-rule (with-new-rect name body ...)
  (let ((name (make-null-rect))) body ... name))

(define-inlinable (rect-get rect i)
  (f32vector-ref (unwrap-rect rect) i))

(define-inlinable (rect-set! rect i x)
  (f32vector-set! (unwrap-rect rect) i x))

(define-inlinable (make-rect x y width height)
  (with-new-rect rect
    (rect-set! rect 0 x)
    (rect-set! rect 1 y)
    (rect-set! rect 2 width)
    (rect-set! rect 3 height)))

(define-inlinable (rect x y width height)
  (make-rect x y width height))

(define (rect-copy! source-rect target-rect)
  "Copy TARGET-RECT to SOURCE-RECT."
  (bytevector-copy! (unwrap-rect source-rect)
                    0
                    (unwrap-rect target-rect)
                    0
                    16))

(define (rect-copy rect)
  "Return a new rect that is a copy of RECT."
  (with-new-rect new
    (rect-copy! rect new)))


;;;
;;; Functional operations
;;;

(define-inlinable (rect-x rect)
  "Return the x coordinate of the lower left corner of RECT."
  (rect-get rect 0))

(define-inlinable (rect-left rect)
  "Return the x coordinate of the lower left corner of RECT."
  (rect-get rect 0))

(define-inlinable (rect-y rect)
  "Return the y coordinate of the lower left corner of RECT."
  (rect-get rect 1))

(define-inlinable (rect-bottom rect)
  "Return the y coordinate of the lower left corner of RECT."
  (rect-get rect 1))

(define-inlinable (rect-right rect)
  "Return the x coordinate of the upper right corner of RECT."
  (+ (rect-x rect) (rect-width rect)))

(define-inlinable (rect-top rect)
  "Return the y coordinate of the upper right corner of RECT."
  (+ (rect-y rect) (rect-height rect)))

(define-inlinable (rect-center-x rect)
  "Return the x coordinate of the center of RECT."
  (+ (rect-x rect) (/ (rect-width rect) 2.0)))

(define-inlinable (rect-center-y rect)
  "Return the y coordinate of the center of RECT."
  (+ (rect-y rect) (/ (rect-height rect) 2.0)))

(define-inlinable (rect-width rect)
  "Return the width of RECT."
  (rect-get rect 2))

(define-inlinable (rect-height rect)
  "Return the height of RECT."
  (rect-get rect 3))

(define-inlinable (rect-area rect)
  "Return the area of RECT."
  (* (rect-width rect) (rect-height rect)))

(define-inlinable (rect-clamp-x rect x)
  "Restrict X to the portion of the x axis covered by RECT."
  (clamp (rect-left rect) (rect-right rect) x))

(define-inlinable (rect-clamp-y rect y)
  "Restrict Y to the portion of the y axis covered by RECT."
  (clamp (rect-bottom rect) (rect-top rect) y))

(define (vec2-clamp-to-rect v rect)
  "Return a new vec2 with the x and y coordinates of the vec2 V
restricted so that they are within the bounds of RECT."
  (vec2-clamp-to-rect! (vec2-copy v) rect))

(define (rect-clamp rect1 rect2)
  "Return a new rect that adjusts the location of RECT1 so that it is
completely within RECT2.  An exception is thrown in the case that
RECT1 cannot fit completely within RECT2."
  (with-new-rect new
    (rect-copy! rect1 new)
    (rect-clamp! new rect2)))

(define-inlinable (rect-move rect x y)
  "Return a new rect based on RECT but moved to location (X, Y)."
  (make-rect x y (rect-width rect) (rect-height rect)))

(define-inlinable (rect-move-vec2 rect v)
  "Return a new rect based on RECT but moved to the vec2 V."
  (make-rect (vec2-x v) (vec2-y v) (rect-width rect) (rect-height rect)))

(define-inlinable (rect-move-by rect x y)
  "Return a new rect based on RECT but moved by (X, Y) units relative
to its current location."
  (with-new-rect new
    (rect-copy! rect new)
    (rect-move-by! new x y)))

(define-inlinable (rect-move-by-vec2 rect v)
  "Return a new rect based on RECT but moved by the vec2 V relative to
its current location."
  (with-new-rect new
    (rect-copy! rect new)
    (rect-move-by-vec2! new v)))

(define-inlinable (rect-inflate rect width height)
  "Return a new rect based on RECT but grown by WIDTH on the x axis
and HEIGHT on the y axis while keeping the rect centered around the
same point."
  (with-new-rect rect
    (rect-inflate! rect width height)))

(define (rect-union rect1 rect2)
  "Return a new rect that completely covers the area of RECT1 and
RECT2."
  (with-new-rect rect
    (rect-copy! rect2 rect1)
    (rect-union! rect1 rect2)))

(define (rect-clip rect1 rect2)
  "Return a new rectangle that is the overlapping region of RECT1 and
RECT2.  If the rects do not overlap, a rect of size 0 is returned."
  (with-new-rect rect
    (rect-copy! rect2 rect1)
    (rect-clip! rect1 rect2)))


;;;
;;; In-place operations
;;;

(define-inlinable (set-rect-x! rect x)
  "Set the left x coordinate of RECT to X."
  (rect-set! rect 0 x))

(define-inlinable (set-rect-y! rect y)
  "Set the bottom y coordinate of RECT to Y."
  (rect-set! rect 1 y))

(define-inlinable (set-rect-width! rect width)
  "Set the width of RECT to WIDTH."
  (rect-set! rect 2 width))

(define-inlinable (set-rect-height! rect height)
  "Set the height of RECT to HEIGHT."
  (rect-set! rect 3 height))

(define-inlinable (rect-move! rect x y)
  "Move RECT to location (X, Y) in-place."
  (set-rect-x! rect x)
  (set-rect-y! rect y))

(define-inlinable (rect-move-vec2! rect v)
  "Move RECT to the vec2 V in-place."
  (set-rect-x! rect (vec2-x v))
  (set-rect-y! rect (vec2-y v)))

(define-inlinable (rect-move-by! rect x y)
  "Move RECT in-place by (X, Y) units relative to its current
location."
  (set-rect-x! rect (+ (rect-x rect) x))
  (set-rect-y! rect (+ (rect-y rect) y)))

(define (rect-move-by-vec2! rect v)
  "Move RECT in-place by the vec2 V relative to its current location."
  (set-rect-x! rect (+ (rect-x rect) (vec2-x v)))
  (set-rect-y! rect (+ (rect-y rect) (vec2-y v))))

(define-inlinable (rect-inflate! rect width height)
  "Grow RECT in-place by WIDTH on the x axis and HEIGHT on the y axis
while keeping the rect centered around the same point."
  (set-rect-x! rect (- (rect-x rect) (/ width 2.0)))
  (set-rect-y! rect (- (rect-y rect) (/ height 2.0)))
  (set-rect-width! rect (+ (rect-width rect) width))
  (set-rect-height! rect (+ (rect-height rect) height)))

(define-inlinable (rect-union! rect1 rect2)
  "Update RECT1 in-place to completely cover the area of RECT1 and
RECT2."
  (let ((x1 (min (rect-left rect1) (rect-left rect2)))
        (x2 (max (rect-right rect1) (rect-right rect2)))
        (y1 (min (rect-bottom rect1) (rect-bottom rect2)))
        (y2 (max (rect-top rect1) (rect-top rect2))))
    (set-rect-x! rect1 x1)
    (set-rect-y! rect1 y1)
    (set-rect-width! rect1 (- x2 x1))
    (set-rect-height! rect1 (- y2 y1))))

(define-inlinable (rect-clip! rect1 rect2)
  "Update RECT1 in-place to be the overlapping region of RECT1 and RECT2.
If the rects do not overlap, RECT1 will have an area of 0."
  (let ((x1 (max (rect-left rect1) (rect-left rect2)))
        (x2 (min (rect-right rect1) (rect-right rect2)))
        (y1 (max (rect-bottom rect1) (rect-bottom rect2)))
        (y2 (min (rect-top rect1) (rect-top rect2))))
    (set-rect-x! rect1 x1)
    (set-rect-y! rect1 y1)
    (set-rect-width! rect1 (max (- x2 x1) 0.0))
    (set-rect-height! rect1 (max (- y2 y1) 0.0))))

(define-inlinable (vec2-clamp-to-rect! v rect)
  "Restrict the x and y coordinates of the vec2 V so that they are
within the bounds of RECT.  V is modified in-place."
  (set-vec2-x! v (clamp (rect-left rect) (rect-right rect) (vec2-x v)))
  (set-vec2-y! v (clamp (rect-bottom rect) (rect-top rect) (vec2-y v))))

(define (rect-clamp! rect1 rect2)
  "Adjust the location of RECT1 in-place so that it is completely
within RECT2.  An exception is thrown in the case that RECT1 cannot
fit completely within RECT2."
  (if (or (> (rect-width rect1) (rect-width rect2))
          (> (rect-height rect1) (rect-height rect2)))
      (error "cannot clamp a rect to a smaller rect" rect1 rect2)
      (begin
        (set-rect-x! rect1
                     (clamp (rect-left rect2)
                            (- (rect-right rect2) (rect-width rect1))
                            (rect-x rect1)))
        (set-rect-y! rect1
                     (clamp (rect-bottom rect2)
                            (- (rect-top rect2) (rect-height rect1))
                            (rect-y rect1))))))


;;;
;;; Queries
;;;

(define (rect-within? rect1 rect2)
  "Return #t if RECT2 is completely within RECT1."
  (and (>= (rect-left rect2) (rect-left rect1))
       (<= (rect-right rect2) (rect-right rect1))
       (>= (rect-bottom rect2) (rect-bottom rect1))
       (<= (rect-top rect2) (rect-top rect1))))

(define (rect-intersects? rect1 rect2)
  "Return #t if RECT2 overlaps RECT1."
  (and (< (rect-left rect1) (rect-right rect2))
       (> (rect-right rect1) (rect-left rect2))
       (< (rect-bottom rect1) (rect-top rect2))
       (> (rect-top rect1) (rect-bottom rect2))))

(define-inlinable (rect-contains? rect x y)
  "Return #t if the coordinates (X, Y) are within RECT."
  (and (>= x (rect-left rect))
       (< x (rect-right rect))
       (>= y (rect-bottom rect))
       (< y (rect-top rect))))

(define-inlinable (rect-contains-vec2? rect v)
  "Return #t if the vec2 V is within RECT."
  (and (>= (vec2-x v) (rect-left rect))
       (< (vec2-x v) (rect-right rect))
       (>= (vec2-y v) (rect-bottom rect))
       (< (vec2-y v) (rect-top rect))))
