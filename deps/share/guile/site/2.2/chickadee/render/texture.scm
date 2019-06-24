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

(define-module (chickadee render texture)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:use-module (gl)
  #:use-module (gl enums)
  #:use-module (sdl2 surface)
  #:use-module (oop goops)
  #:use-module (chickadee math rect)
  #:use-module (chickadee render gl)
  #:use-module (chickadee render gpu)
  #:export (make-texture
            make-texture-region
            load-image
            texture?
            texture-region?
            texture-null?
            texture-parent
            texture-min-filter
            texture-mag-filter
            texture-wrap-s
            texture-wrap-t
            texture-x
            texture-y
            texture-width
            texture-height
            texture-gl-rect
            texture-gl-tex-rect
            null-texture
            texture-set!
            texture-ref

            texture-atlas
            list->texture-atlas
            split-texture
            texture-atlas?
            texture-atlas-texture
            texture-atlas-ref))


;;;
;;; Textures
;;;

;; The <texture> object is a simple wrapper around an OpenGL texture
;; id.
(define-record-type <texture>
  (%make-texture id parent min-filter mag-filter wrap-s wrap-t
                 x y width height gl-rect gl-tex-rect)
  texture?
  (id texture-id)
  (parent texture-parent)
  (min-filter texture-min-filter)
  (mag-filter texture-mag-filter)
  (wrap-s texture-wrap-s)
  (wrap-t texture-wrap-t)
  (x texture-x)
  (y texture-y)
  (width texture-width)
  (height texture-height)
  (gl-rect texture-gl-rect)
  (gl-tex-rect texture-gl-tex-rect))

(set-record-type-printer! <texture>
  (lambda (texture port)
    (format port
            "#<texture region?: ~a x: ~d y: ~d width: ~d height: ~d min-filter: ~a mag-filter: ~a wrap-s: ~a wrap-t: ~a>"
            (texture-region? texture)
            (texture-x texture)
            (texture-y texture)
            (texture-width texture)
            (texture-height texture)
            (texture-min-filter texture)
            (texture-mag-filter texture)
            (texture-wrap-s texture)
            (texture-wrap-t texture))))

(define null-texture
  (%make-texture 0 #f 'linear 'linear 'repeat 'repeat 0 0 0 0
                 (make-rect 0.0 0.0 0.0 0.0) (make-rect 0.0 0.0 0.0 0.0)))

(define <<texture>> (class-of null-texture))

(define (texture-null? texture)
  "Return #t if TEXTURE is the null texture."
  (eq? texture null-texture))

(define (texture-region? texture)
  (texture? (texture-parent texture)))

(define (free-texture texture)
  (gl-delete-texture (texture-id texture)))

(define-method (gpu-finalize (texture <<texture>>))
  (free-texture texture))

(define (make-apply-texture n)
  (let ((texture-unit (+ (version-1-3 texture0) n)))
    (lambda (texture)
      (set-gl-active-texture texture-unit)
      (gl-bind-texture (texture-target texture-2d)
                       (texture-id texture)))))

(define *texture-states*
  (let ((states (make-vector 32)))
    (let loop ((i 0))
      (if (< i 32)
          (begin
            (vector-set! states i (make-gpu-state (make-apply-texture i)
                                                  null-texture))
            (loop (1+ i)))
          states))))

(define (texture-ref! n)
  (gpu-state-ref (vector-ref *texture-states* n)))

(define (texture-set! n texture)
  (gpu-state-set! (vector-ref *texture-states* n) texture))

(define* (make-texture pixels width height #:key
                       flip?
                       (min-filter 'linear)
                       (mag-filter 'linear)
                       (wrap-s 'repeat)
                       (wrap-t 'repeat)
                       (format 'rgba))
  "Translate the bytevector PIXELS into an OpenGL texture with
dimensions WIDTHxHEIGHT where each pixel is in 32-bit RGBA format.
The first pixe lin PIXELS corresponds to the upper-left corner of the
image.  If this is not the case and the first pixel corresponds to the
lower-left corner of the image, set FLIP? to #t.  The generated
texture uses MIN-FILTER for downscaling and MAG-FILTER for upscaling.
WRAP-S and WRAP-T are symbols that control how texture access is
handled for texture coordinates outside the [0, 1] range.  Allowed
symbols are: repeat (the default), clamp, clamp-to-border,
clamp-to-edge.  FORMAT specifies the pixel format.  Currently only
32-bit RGBA format is supported."
  (define (gl-wrap mode)
    (match mode
      ('repeat (texture-wrap-mode repeat))
      ('clamp (texture-wrap-mode clamp))
      ('clamp-to-border (texture-wrap-mode clamp-to-border-sgis))
      ('clamp-to-edge (texture-wrap-mode clamp-to-edge-sgis))))

  (let ((texture (gpu-guard
                  (%make-texture (gl-generate-texture) #f
                                 min-filter mag-filter wrap-s wrap-t
                                 0 0 width height
                                 (make-rect 0.0 0.0 width height)
                                 (if flip?
                                     (make-rect 0.0 1.0 1.0 -1.0)
                                     (make-rect 0.0 0.0 1.0 1.0))))))
    (texture-set! 0 texture)
    (gl-texture-parameter (texture-target texture-2d)
                          (texture-parameter-name texture-min-filter)
                          (match min-filter
                            ('nearest 9728)
                            ('linear 9729)))
    (gl-texture-parameter (texture-target texture-2d)
                          (texture-parameter-name texture-mag-filter)
                          (match mag-filter
                            ('nearest 9728)
                            ('linear 9729)))
    (gl-texture-parameter (texture-target texture-2d)
                          (texture-parameter-name texture-wrap-s)
                          (gl-wrap wrap-s))
    (gl-texture-parameter (texture-target texture-2d)
                          (texture-parameter-name texture-wrap-t)
                          (gl-wrap wrap-t))
    (gl-texture-image-2d (texture-target texture-2d)
                         0 (pixel-format rgba) width height 0
                         (match format
                           ('rgba (pixel-format rgba)))
                         (color-pointer-type unsigned-byte)
                         (or pixels %null-pointer))
    texture))

(define (make-texture-region texture rect)
  "Create a new texture region covering a section of TEXTURE defined
by the bounding box RECT."
  (let* ((pw (texture-width texture))
         (ph (texture-height texture))
         (x (rect-x rect))
         (y (rect-y rect))
         (w (rect-width rect))
         (h (rect-height rect))
         (vert-rect (make-rect 0.0 0.0 w h))
         (tex-rect (make-rect (/ x pw) (/ y ph) (/ w pw) (/ h ph))))
    (%make-texture (texture-id texture)
                   texture
                   (texture-min-filter texture)
                   (texture-mag-filter texture)
                   (texture-wrap-s texture)
                   (texture-wrap-t texture)
                   x y w h
                   vert-rect
                   tex-rect)))

(define (flip-pixels-vertically pixels width height)
  "Create a new bytevector that reverses the rows in PIXELS, a WIDTH x
HEIGHT, 32 bit color bytevector."
  (let ((buffer (make-u8vector (bytevector-length pixels)))
        (row-width (* width 4))) ; assuming 32 bit color
    (let loop ((y 0))
      (when (< y height)
        (let* ((y* (- height y 1))
               (source-start (* y row-width))
               (target-start (* y* row-width)))
          (bytevector-copy! pixels source-start buffer target-start row-width)
          (loop (1+ y)))))
    buffer))

(define (surface->texture surface min-filter mag-filter wrap-s wrap-t)
  "Convert SURFACE, an SDL2 surface object, into a texture that uses
the given MIN-FILTER and MAG-FILTER."
  ;; Convert to 32 bit RGBA color.
  (call-with-surface (convert-surface-format surface 'abgr8888)
    (lambda (surface)
      (let* ((width (surface-width surface))
             (height (surface-height surface))
             (pixels (surface-pixels surface)))
        (make-texture pixels width height
                      #:min-filter min-filter
                      #:mag-filter mag-filter
                      #:wrap-s wrap-s
                      #:wrap-t wrap-t)))))

(define* (load-image file #:key
                     (min-filter 'nearest)
                     (mag-filter 'nearest)
                     (wrap-s 'repeat)
                     (wrap-t 'repeat))
  "Load a texture from an image in FILE.  MIN-FILTER and MAG-FILTER
describe the method that should be used for minification and
magnification.  Valid values are 'nearest and 'linear.  By default,
'nearest is used."
  (call-with-surface ((@ (sdl2 image) load-image) file)
    (lambda (surface)
      (surface->texture surface min-filter mag-filter wrap-s wrap-t))))


;;;
;;; Texture Atlas
;;;

(define-record-type <texture-atlas>
  (%make-texture-atlas texture vector)
  texture-atlas?
  (texture texture-atlas-texture)
  (vector texture-atlas-vector))

(define (display-texture-atlas atlas port)
  (format port
          "#<texture-atlas texture: ~a size: ~d>"
          (texture-atlas-texture atlas)
          (vector-length (texture-atlas-vector atlas))))

(set-record-type-printer! <texture-atlas> display-texture-atlas)

(define (list->texture-atlas texture rects)
  "Return a new atlas for TEXTURE containing RECTS, a list of texture
coordinate rects denoting the various regions within."
  (let ((v (make-vector (length rects))))
    (let loop ((i 0)
               (rects rects))
      (match rects
        (() (%make-texture-atlas texture v))
        (((x y width height) . rest)
         (vector-set! v i (make-texture-region texture (make-rect x y width height)))
         (loop (1+ i) rest))))))

(define (texture-atlas texture . rects)
  "Return a new atlas for TEXTURE containing RECTS, a series of
4-tuples in the form (x y width height) describing the various tiles
within."
  (list->texture-atlas texture rects))

(define (texture-atlas-ref atlas index)
  "Return the texture region associated with INDEX in
ATLAS."
  (vector-ref (texture-atlas-vector atlas) index))

(define* (split-texture texture tile-width tile-height #:key
                        (margin 0) (spacing 0))
  "Return a new texture atlas that splits TEXTURE into a grid of
TILE-WIDTH by TILE-HEIGHT rectangles.  Optionally, each tile may have
SPACING pixels of horizontal and vertical space between surrounding
tiles and the entire image may have MARGIN pixels of empty space
around its border.

This type of texture atlas layout is very common for tile map
terrain."
  (let* ((w (texture-width texture))
         (h (texture-height texture))
         (rows (inexact->exact (ceiling (/ (- h margin) (+ tile-height spacing)))))
         (columns (inexact->exact (ceiling (/ (- w margin) (+ tile-width spacing)))))
         (v (make-vector (* rows columns))))
    (define (make-tile tx ty)
      (let* ((x (+ (* tx (+ tile-width spacing)) margin))
             (y (+ (* ty (+ tile-height spacing)) margin)))
        (make-texture-region texture (make-rect x y tile-width tile-height))))
    (let y-loop ((y 0))
      (when (< y rows)
        (let x-loop ((x 0))
          (when (< x columns)
            (vector-set! v (+ x (* y columns)) (make-tile x y))
            (x-loop (1+ x))))
        (y-loop (1+ y))))
    (%make-texture-atlas texture v)))
