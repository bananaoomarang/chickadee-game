;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;;
;;; This file is part of guile-sdl2.
;;;
;;; Guile-sdl2 is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Guile-sdl2 is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with guile-sdl2.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; SDL surface manipulation.
;;
;;; Code:

(define-module (sdl2 surface)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:use-module (sdl2)
  #:export (color?
            color-r
            color-g
            color-b
            color-a

            palette?
            palette-length
            palette-colors

            pixel-format?
            pixel-format-name
            pixel-format-palette
            pixel-format-bits-per-pixel
            pixel-format-bytes-per-pixel
            pixel-format-red-mask
            pixel-format-green-mask
            pixel-format-blue-mask
            pixel-format-alpha-mask

            make-rgb-surface
            bytevector->surface
            surface?
            delete-surface!
            call-with-surface
            load-bmp
            surface-pixel-format
            surface-width
            surface-height
            surface-pitch
            surface-pixels
            convert-surface-format
            blit-surface
            blit-scaled
            fill-rect))


;;;
;;; Color
;;;

(define-record-type <color>
  (make-color r g b a)
  color?
  (r color-r)
  (g color-g)
  (b color-b)
  (a color-a))


;;;
;;; Palette
;;;

(define-wrapped-pointer-type <palette>
  palette?
  wrap-palette unwrap-palette
  (lambda (palette port)
    (format port "#<palette length: ~d>"
            (palette-length palette))))

(define %palette-types
  (list int  ; number of colors
        '*)) ; colors

(define-syntax-rule (palette-parse-match palette matchers ...)
  (match (parse-c-struct (unwrap-palette palette) %palette-types)
    matchers ...))

(define (palette-length palette)
  "Return the number of colors in PALETTE."
  (palette-parse-match palette
    ((length _) length)))

(define (palette-colors palette)
  "Return the colors in PALETTE."
  (palette-parse-match palette
    ((length colors)
     (let ((bv (pointer->bytevector colors (* length 4)))
           (colors (make-vector length)))
       (let loop ((i 0))
         (if (< i length)
             (let ((offset (* i 4)))
               (vector-set! colors i
                            (make-color (u8vector-ref bv offset)
                                        (u8vector-ref bv (+ offset 1))
                                        (u8vector-ref bv (+ offset 2))
                                        (u8vector-ref bv (+ offset 3))))
               (loop (1+ i)))
             colors))))))


;;;
;;; Pixel Format
;;;

(define-wrapped-pointer-type <pixel-format>
  pixel-format?
  wrap-pixel-format unwrap-pixel-format
  (lambda (pf port)
    (format port "#<pixel-format name: ~s bpp: ~d>"
            (pixel-format-name pf)
            (pixel-format-bytes-per-pixel pf))))

(define %pixel-format-types
  (list uint32   ; format
        '*       ; palette
        uint8    ; bits per pixel
        uint8    ; bytes per pixel
        uint32   ; red mask
        uint32   ; green mask
        uint32   ; blue mask
        uint32)) ; alpha mask

(define-syntax-rule (pixel-format-parse-match pf matchers ...)
  (match (parse-c-struct (unwrap-pixel-format pf) %pixel-format-types)
    matchers ...))

(define (pixel-format-name pf)
  "Return the symbolic name of the pixel format PF."
  (pixel-format-parse-match pf
    ((format _ _ _ _ _ _ _)
     (cond
      ((= format ffi:SDL_PIXELFORMAT_INDEX1LSB) 'index1lsb)
      ((= format ffi:SDL_PIXELFORMAT_INDEX1MSB) 'index1msb)
      ((= format ffi:SDL_PIXELFORMAT_INDEX4LSB) 'index4lsb)
      ((= format ffi:SDL_PIXELFORMAT_INDEX4MSB) 'index4msb)
      ((= format ffi:SDL_PIXELFORMAT_INDEX8) 'index8)
      ((= format ffi:SDL_PIXELFORMAT_RGB332) 'rgb332)
      ((= format ffi:SDL_PIXELFORMAT_RGB444) 'rgb444)
      ((= format ffi:SDL_PIXELFORMAT_RGB555) 'rgb555)
      ((= format ffi:SDL_PIXELFORMAT_BGR555) 'bgr555)
      ((= format ffi:SDL_PIXELFORMAT_ARGB4444) 'argb4444)
      ((= format ffi:SDL_PIXELFORMAT_RGBA4444) 'rgba4444)
      ((= format ffi:SDL_PIXELFORMAT_ABGR4444) 'abgr4444)
      ((= format ffi:SDL_PIXELFORMAT_BGRA4444) 'bgra4444)
      ((= format ffi:SDL_PIXELFORMAT_ARGB1555) 'argb1555)
      ((= format ffi:SDL_PIXELFORMAT_RGBA5551) 'rgba5551)
      ((= format ffi:SDL_PIXELFORMAT_ABGR1555) 'abgr1555)
      ((= format ffi:SDL_PIXELFORMAT_BGRA5551) 'bgra5551)
      ((= format ffi:SDL_PIXELFORMAT_RGB565) 'rgb565)
      ((= format ffi:SDL_PIXELFORMAT_BGR565) 'bgr565)
      ((= format ffi:SDL_PIXELFORMAT_RGB24) 'rgb24)
      ((= format ffi:SDL_PIXELFORMAT_BGR24) 'bgr24)
      ((= format ffi:SDL_PIXELFORMAT_RGB888) 'rgb888)
      ((= format ffi:SDL_PIXELFORMAT_RGBX8888) 'rgbx8888)
      ((= format ffi:SDL_PIXELFORMAT_BGR888) 'bgr888)
      ((= format ffi:SDL_PIXELFORMAT_BGRX8888) 'bgrx8888)
      ((= format ffi:SDL_PIXELFORMAT_ARGB8888) 'argb8888)
      ((= format ffi:SDL_PIXELFORMAT_RGBA8888) 'rgba8888)
      ((= format ffi:SDL_PIXELFORMAT_ABGR8888) 'abgr8888)
      ((= format ffi:SDL_PIXELFORMAT_BGRA8888) 'bgra8888)
      ((= format ffi:SDL_PIXELFORMAT_ARGB2101010) 'argb2101010)
      ((= format ffi:SDL_PIXELFORMAT_YV12) 'yv12)
      ((= format ffi:SDL_PIXELFORMAT_IYUV) 'iyuv)
      ((= format ffi:SDL_PIXELFORMAT_YUY2) 'yuy2)
      ((= format ffi:SDL_PIXELFORMAT_UYVY) 'uyvy)
      ((= format ffi:SDL_PIXELFORMAT_YVYU) 'yvyu)))))

(define (pixel-format-palette pf)
  "Return the palette for the pixel format PF."
  (pixel-format-parse-match pf
    ((_ palette _ _ _ _ _ _)
     (if (null-pointer? palette)
         #f
         (wrap-palette palette)))))

(define (pixel-format-bits-per-pixel pf)
  "Return the number of bits per pixel for the pixel format PF."
  (pixel-format-parse-match pf
    ((_ _ bits _ _ _ _ _) bits)))

(define (pixel-format-bytes-per-pixel pf)
  "Return the number of bytes per pixel for the pixel format PF."
  (pixel-format-parse-match pf
    ((_ _ _ bytes _ _ _ _) bytes)))

(define (pixel-format-red-mask pf)
  "Return the bitmask for the red component of a pixel in the pixel
format PF."
  (pixel-format-parse-match pf
    ((_ _ _ _ red-mask _ _ _) red-mask)))

(define (pixel-format-green-mask pf)
  "Return the bitmask for the green component of a pixel in the pixel
format PF."
  (pixel-format-parse-match pf
    ((_ _ _ _ _ green-mask _ _) green-mask)))

(define (pixel-format-blue-mask pf)
  "Return the bitmask for the blue component of a pixel in the pixel
format PF."
  (pixel-format-parse-match pf
    ((_ _ _ _ _ _ blue-mask _) blue-mask)))

(define (pixel-format-alpha-mask pf)
  "Return the bitmask for the alpha component of a pixel in the pixel
format PF."
  (pixel-format-parse-match pf
    ((_ _ _ _ _ _ _ alpha-mask) alpha-mask)))


;;;
;;; Surface
;;;

(define-wrapped-pointer-type <surface>
  surface?
  wrap-surface unwrap-surface
  (lambda (surface port)
    (format port "#<surface ~x>"
            (pointer-address (unwrap-surface surface)))))

(define (make-rgb-surface width height depth)
  "Create a new SDL surface with the dimensions WIDTH and HEIGHT and
DEPTH bits per pixel."
  (wrap-surface
   (if (eq? (native-endianness) 'big)
       (ffi:sdl-create-rgb-surface 0 width height depth
                                   #xff000000
                                   #x00ff0000
                                   #x0000ff00
                                   #x000000ff)
       (ffi:sdl-create-rgb-surface 0 width height depth
                                   #x000000ff
                                   #x0000ff00
                                   #x00ff0000
                                   #xff000000))))

(define (bytevector->surface bv width height depth pitch)
  "Convert BV, a bytevector of pixel data with dimenions WIDTHxHEIGHT,
to an SDL surface.  Each pixel is DEPTH bits in size, and each row of
pixels is PITCH bytes in size."
  (wrap-surface
   (if (eq? (native-endianness) 'big)
       (ffi:sdl-create-rgb-surface-from (bytevector->pointer bv)
                                        width height depth pitch
                                        #xff000000
                                        #x00ff0000
                                        #x0000ff00
                                        #x000000ff)
       (ffi:sdl-create-rgb-surface-from (bytevector->pointer bv)
                                        width height depth pitch
                                        #x000000ff
                                        #x0000ff00
                                        #x00ff0000
                                        #xff000000))))

(define (delete-surface! surface)
  "Free the memory used by SURFACE."
  (ffi:sdl-free-surface (unwrap-surface surface)))

(define (call-with-surface surface proc)
  "Call PROC, passing it SURFACE and deleting SURFACE upon exit of
PROC."
  (dynamic-wind
    (const #t)
    (lambda ()
      (proc surface))
    (lambda ()
      (delete-surface! surface))))

;; The equivalent of the SDL_LoadBMP C macro.
(define (load-bmp file)
  "Create a new surface from the bitmap data in FILE."
  (let ((ptr (ffi:sdl-load-bmp-rw (ffi:sdl-rw-from-file (string->pointer file)
                                                        (string->pointer "rb"))
                                  1)))
    (if (null-pointer? ptr)
        (sdl-error "load-bmp" "failed to load bitmap")
        (wrap-surface ptr))))

(define %int-size (sizeof int))
(define %pointer-size (sizeof '*))

(define (pointer-int-ref pointer offset)
  (bytevector-sint-ref (pointer->bytevector pointer %int-size offset)
                       0 (native-endianness) %int-size))

;; A partial list of surface types so that we can parse the data we
;; need out of the SDL_Surface struct pointer.
(define %surface-types
  (list uint32 ; flags
        '*     ; format
        int    ; width
        int    ; height
        int    ; pitch
        '*))   ; pixels

(define-syntax-rule (surface-parse-match surface matchers ...)
  (match (parse-c-struct (unwrap-surface surface) %surface-types)
    matchers ...))

(define (surface-width surface)
  "Return the width of SURFACE in pixels."
  (surface-parse-match surface
    ((_ _ width _ _ _) width)))

(define (surface-height surface)
  "Return the height of SURFACE in pixels."
    (surface-parse-match surface
      ((_ _ _ height _ _) height)))

(define (surface-pitch surface)
  "Return the length of a row of pixels in SURFACE in bytes."
  (surface-parse-match surface
    ((_ _ _ _ pitch _) pitch)))

(define (surface-pixels surface)
  "Return a bytevector containing the raw pixel data in SURFACE."
  (surface-parse-match surface
    ((_ _ _ height pitch pixels)
     (pointer->bytevector pixels (* height pitch)))))

(define (surface-pixel-format surface)
  "Return the pixel format for SURFACE."
  (surface-parse-match surface
    ((_ format _ _ _ _)
     (wrap-pixel-format format))))

(define (symbol->sdl-pixel-format sym)
  (match sym
    ('index1lsb   ffi:SDL_PIXELFORMAT_INDEX1LSB)
    ('index1msb   ffi:SDL_PIXELFORMAT_INDEX1MSB)
    ('index4lsb   ffi:SDL_PIXELFORMAT_INDEX4LSB)
    ('index4msb   ffi:SDL_PIXELFORMAT_INDEX4MSB)
    ('index8      ffi:SDL_PIXELFORMAT_INDEX8)
    ('rgb332      ffi:SDL_PIXELFORMAT_RGB332)
    ('rgb444      ffi:SDL_PIXELFORMAT_RGB444)
    ('rgb555      ffi:SDL_PIXELFORMAT_RGB555)
    ('bgr555      ffi:SDL_PIXELFORMAT_BGR555)
    ('argb4444    ffi:SDL_PIXELFORMAT_ARGB4444)
    ('rgba4444    ffi:SDL_PIXELFORMAT_RGBA4444)
    ('abgr4444    ffi:SDL_PIXELFORMAT_ABGR4444)
    ('bgra4444    ffi:SDL_PIXELFORMAT_BGRA4444)
    ('argb1555    ffi:SDL_PIXELFORMAT_ARGB1555)
    ('rgba5551    ffi:SDL_PIXELFORMAT_RGBA5551)
    ('abgr1555    ffi:SDL_PIXELFORMAT_ABGR1555)
    ('bgra5551    ffi:SDL_PIXELFORMAT_BGRA5551)
    ('rgb565      ffi:SDL_PIXELFORMAT_RGB565)
    ('bgr565      ffi:SDL_PIXELFORMAT_BGR565)
    ('rgb24       ffi:SDL_PIXELFORMAT_RGB24)
    ('bgr24       ffi:SDL_PIXELFORMAT_BGR24)
    ('rgb888      ffi:SDL_PIXELFORMAT_RGB888)
    ('rgbx8888    ffi:SDL_PIXELFORMAT_RGBX8888)
    ('bgr888      ffi:SDL_PIXELFORMAT_BGR888)
    ('bgrx8888    ffi:SDL_PIXELFORMAT_BGRX8888)
    ('argb8888    ffi:SDL_PIXELFORMAT_ARGB8888)
    ('rgba8888    ffi:SDL_PIXELFORMAT_RGBA8888)
    ('abgr8888    ffi:SDL_PIXELFORMAT_ABGR8888)
    ('bgra8888    ffi:SDL_PIXELFORMAT_BGRA8888)
    ('argb2101010 ffi:SDL_PIXELFORMAT_ARGB2101010)
    ('yv12        ffi:SDL_PIXELFORMAT_YV12)
    ('iyuv        ffi:SDL_PIXELFORMAT_IYUV)
    ('yuy2        ffi:SDL_PIXELFORMAT_YUY2)
    ('uyvy        ffi:SDL_PIXELFORMAT_UYVY)
    ('yvyu        ffi:SDL_PIXELFORMAT_YVYU)))

(define (convert-surface-format surface format)
  "Convert the pixels in SURFACE to FORMAT, a symbol representing a
specific pixel format, and return a new surface object.

Valid format types are:

- index1lsb
- index1msb
- index4lsb
- index4msb
- index8
- rgb332
- rgb444
- rgb555
- bgr555
- argb4444
- rgba4444
- abgr4444
- bgra4444
- argb1555
- rgba5551
- abgr1555
- bgra5551
- rgb565
- bgr565
- rgb24
- bgr24
- rgb888
- rgbx8888
- bgr888
- bgrx8888
- argb8888
- rgba8888
- abgr8888
- bgra8888
- argb2101010
- yv12
- iyuv
- yuy2
- uyvy
- yvyu"
  (let ((ptr (ffi:sdl-convert-surface-format (unwrap-surface surface)
                                             (symbol->sdl-pixel-format format)
                                             0)))
    (if (null-pointer? ptr)
        (sdl-error "convert-surface-format" "failed to convert surface format")
        (wrap-surface ptr))))

(define (blit-surface src src-rect dst dst-rect)
  "Blit the rectangle SRC-RECT from the surface SRC to DST-RECT of the
surface DST."
  (unless (zero?
           (ffi:sdl-blit-surface (unwrap-surface src)
                                 (if src-rect
                                     ((@@ (sdl2 rect) unwrap-rect) src-rect)
                                     %null-pointer)
                                 (unwrap-surface dst)
                                 (if dst-rect
                                     ((@@ (sdl2 rect) unwrap-rect) dst-rect)
                                     %null-pointer)))
    (sdl-error "blit-surface" "failed to blit surface ~a to ~a" src dst)))

(define (blit-scaled src src-rect dst dst-rect)
  "Blit the rectangle SRC-RECT from the surface SRC to DST-RECT of the
surface DST, scaling the source to fit the destination."
  (unless (zero?
           (ffi:sdl-blit-surface (unwrap-surface src)
                                 (if src-rect
                                     ((@@ (sdl2 rect) unwrap-rect) src-rect)
                                     %null-pointer)
                                 (unwrap-surface dst)
                                 (if dst-rect
                                     ((@@ (sdl2 rect) unwrap-rect) dst-rect)
                                     %null-pointer)))
    (sdl-error "blit-scaled" "failed to blit surface ~a to ~a" src dst)))

(define (fill-rect dst rect color)
  "Fill RECT with COLOR in the surface DST."
  (unless (zero?
           (ffi:sdl-fill-rect (unwrap-surface dst)
                              (if rect
                                  ((@@ (sdl2 rect) unwrap-rect) rect)
                                  %null-pointer)
                              color))
    (sdl-error "fill-rect" "failed to fill rect in ~a" dst)))
