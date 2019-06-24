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
;; Viewports specify the renderable section of a window.
;;
;;; Code:

(define-module (chickadee render viewport)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (gl)
  #:use-module (chickadee utils)
  #:use-module (chickadee render color)
  #:use-module (chickadee render gl)
  #:use-module (chickadee render gpu)
  #:export (make-viewport
            viewport?
            viewport-x
            viewport-y
            viewport-width
            viewport-height
            viewport-clear-color
            viewport-clear-flags
            null-viewport
            %default-clear-flags
            %default-clear-color
            apply-viewport
            *viewport-state*))

(define-record-type <viewport>
  (%make-viewport x y width height clear-color clear-flags)
  viewport?
  (x viewport-x)
  (y viewport-y)
  (width viewport-width)
  (height viewport-height)
  (clear-color viewport-clear-color)
  (clear-flags viewport-clear-flags))

(define %default-clear-flags '(color-buffer depth-buffer))
;; Just a fun color from the Dawnbringer 32-color palette instead of
;; boring old black.
(define %default-clear-color (rgb #x45283c))

(define (assert-non-negative-integer n)
  (if (and (integer? n) (>= n 0))
      n
      (error "expecting non-negative integer:" n)))

(define* (make-viewport x y width height #:key
                        (clear-color %default-clear-color)
                        (clear-flags %default-clear-flags))
  "Create a viewport that covers an area of the window starting from
coordinates (X, Y) and spanning WIDTH x HEIGHT pixels.  Fill the
viewport with CLEAR-COLOR when clearing the screen.  Clear the buffers
denoted by the list of symbols in CLEAR-FLAGS.  Possible values for
CLEAR-FLAGS are 'color-buffer', 'depth-buffer', 'accum-buffer', and
'stencil-buffer'."
  (%make-viewport (assert-non-negative-integer x)
                  (assert-non-negative-integer y)
                  (assert-non-negative-integer width)
                  (assert-non-negative-integer height)
                  clear-color
                  clear-flags))

(define null-viewport (make-viewport 0 0 0 0))

(define clear-buffer-mask
  (memoize
   (lambda (flags)
     (apply logior
            ;; Map symbols to OpenGL constants.
            (map (match-lambda
                  ('depth-buffer 256)
                  ('accum-buffer 512)
                  ('stencil-buffer 1024)
                  ('color-buffer 16384))
                 flags)))))

(define (apply-viewport viewport)
  "Set the OpenGL state for VIEWPORT.  Clip rendering to the viewport
area, set the clear color, and clear necessary buffers."
  (unless (eq? viewport null-viewport)
    (let ((x (viewport-x viewport))
          (y (viewport-y viewport))
          (w (viewport-width viewport))
          (h (viewport-height viewport))
          (c (viewport-clear-color viewport)))
      (gl-enable (enable-cap scissor-test))
      (gl-viewport x y w h)
      (gl-scissor x y w h)
      (gl-clear-color (color-r c) (color-g c) (color-b c) (color-a c))
      (gl-clear (clear-buffer-mask (viewport-clear-flags viewport))))))

(define *viewport-state*
  (make-gpu-state apply-viewport null-viewport))
