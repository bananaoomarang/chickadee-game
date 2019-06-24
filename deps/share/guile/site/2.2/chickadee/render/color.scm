;;; Chickadee Game Toolkit
;;; Copyright © 2016, 2018 David Thompson <davet@gnu.org>
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
;; Colors!
;;
;;; Code:

(define-module (chickadee render color)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (chickadee math)
  #:export (color make-color
            color?
            color-r color-g color-b color-a
            rgba rgb transparency
            color* color+ color- color-inverse color-lerp

            white black red green blue yellow magenta cyan transparent
            tango-light-butter tango-butter tango-dark-butter
            tango-light-orange tango-orange tango-dark-orange
            tango-light-chocolate tango-chocolate tango-dark-chocolate
            tango-light-chameleon tango-chameleon tango-dark-chameleon
            tango-light-sky-blue tango-sky-blue tango-dark-sky-blue
            tango-light-plum tango-plum tango-dark-plum
            tango-light-scarlet-red tango-scarlet-red tango-dark-scarlet-red
            tango-aluminium-1 tango-aluminium-2 tango-aluminium-3
            tango-aluminium-4 tango-aluminium-5 tango-aluminium-6))

(define-record-type <color>
  (wrap-color bv)
  color?
  (bv unwrap-color))

(define-inlinable (color-r color)
  (f32vector-ref (unwrap-color color) 0))

(define-inlinable (color-g color)
  (f32vector-ref (unwrap-color color) 1))

(define-inlinable (color-b color)
  (f32vector-ref (unwrap-color color) 2))

(define-inlinable (color-a color)
  (f32vector-ref (unwrap-color color) 3))

(define-inlinable (make-color r g b a)
  (wrap-color
   (f32vector
    (clamp 0.0 1.0 r)
    (clamp 0.0 1.0 g)
    (clamp 0.0 1.0 b)
    (clamp 0.0 1.0 a))))

(define-inlinable (color r g b a)
  (make-color r g b a))

(define (color-component color-code offset)
  "Return the value of an 8-bit color channel in the range [0,1] for
the integer COLOR-CODE, given an OFFSET in bits."
  (let ((mask (ash #xff offset)))
    (/ (ash (logand mask color-code)
            (- offset))
       255.0)))

(define (rgba color-code)
  "Translate an RGBA format string COLOR-CODE into a color object.
For example: #xffffffff will return a color with RGBA values 1, 1, 1,
1."
  (make-color (color-component color-code 24)
              (color-component color-code 16)
              (color-component color-code 8)
              (color-component color-code 0)))

(define (rgb color-code)
  "Translate an RGB format string COLOR-CODE into a color object.
For example: #xffffff will return a color with RGBA values 1, 1, 1,
1."
  (make-color (color-component color-code 16)
              (color-component color-code 8)
              (color-component color-code 0)
              1.0))

(define (transparency alpha)
  "Create a new color that is white with a transparency value of
ALPHA.  ALPHA is clamped to the range [0, 1]."
  (make-color 1 1 1 alpha))

;; TODO: Optimize and inline
(define color*
  (match-lambda*
   ((($ <color> r1 g1 b1 a1) ($ <color> r2 g2 b2 a2))
    (make-color (* r1 r2)
                (* g1 g2)
                (* b1 b2)
                (* a1 a2)))
   ((($ <color> r g b a) (? number? k))
    (make-color (* r k)
                (* g k)
                (* b k)
                (* a k)))))

(define color+
  (match-lambda*
   ((($ <color> r1 g1 b1 a1) ($ <color> r2 g2 b2 a2))
    (make-color (+ r1 r2)
                (+ g1 g2)
                (+ b1 b2)
                (+ a1 a2)))))

(define color-
  (match-lambda*
   ((($ <color> r1 g1 b1 a1) ($ <color> r2 g2 b2 a2))
    (make-color (- r1 r2)
                (- g1 g2)
                (- b1 b2)
                (- a1 a2)))))

(define color-inverse
  (match-lambda
   (($ <color> r g b a)
    (make-color (- 1 r)
                (- 1 g)
                (- 1 b)
                a)))) ; Do not alter alpha channel.

(define-inlinable (color-lerp start end alpha)
  (color+ (color* start (- 1.0 alpha))
          (color* end alpha)))

;;;
;;; Pre-defined Colors
;;;

;; Basic
(define white (rgb #xffffff))
(define black (rgb #x000000))
(define red (rgb #xff0000))
(define green (rgb #x00ff00))
(define blue (rgb #x0000ff))
(define yellow (rgb #xffff00))
(define magenta (rgb #xff00ff))
(define cyan (rgb #x00ffff))
(define transparent (make-color 0 0 0 0))

;; Tango color pallete
;; http://tango.freedesktop.org
(define tango-light-butter (rgb #xfce94f))
(define tango-butter (rgb #xedd400))
(define tango-dark-butter (rgb #xc4a000))
(define tango-light-orange (rgb #xfcaf3e))
(define tango-orange (rgb #xf57900))
(define tango-dark-orange (rgb #xce5c00))
(define tango-light-chocolate (rgb #xe9b96e))
(define tango-chocolate (rgb #xc17d11))
(define tango-dark-chocolate (rgb #x8f5902))
(define tango-light-chameleon (rgb #x8ae234))
(define tango-chameleon (rgb #x73d216))
(define tango-dark-chameleon (rgb #x4e9a06))
(define tango-light-sky-blue (rgb #x729fcf))
(define tango-sky-blue (rgb #x3465a4))
(define tango-dark-sky-blue (rgb #x204a87))
(define tango-light-plum (rgb #xad7fa8))
(define tango-plum (rgb #x75507b))
(define tango-dark-plum (rgb #x5c3566))
(define tango-light-scarlet-red (rgb #xef2929))
(define tango-scarlet-red (rgb #xcc0000))
(define tango-dark-scarlet-red (rgb #xa40000))
(define tango-aluminium-1 (rgb #xeeeeec))
(define tango-aluminium-2 (rgb #xd3d7cf))
(define tango-aluminium-3 (rgb #xbabdb6))
(define tango-aluminium-4 (rgb #x888a85))
(define tango-aluminium-5 (rgb #x555753))
(define tango-aluminium-6 (rgb #x2e3436))
