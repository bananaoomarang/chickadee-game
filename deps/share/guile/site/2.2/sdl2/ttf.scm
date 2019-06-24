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
;; Font rendering.
;;
;;; Code:

(define-module (sdl2 ttf)
  #:use-module (ice-9 format)
  #:use-module (system foreign)
  #:use-module (sdl2)
  #:use-module ((sdl2 bindings ttf) #:prefix ffi:)
  #:export (ttf-init
            ttf-quit

            font?
            load-font
            delete-font!
            font-height

            render-font-solid
            render-font-blended))

(define (ttf-init)
  "Initialize the TTF system."
  (unless (zero? (ffi:ttf-init))
    (sdl-error "ttf-init" "failed to initialize TTF library")))

(define (ttf-quit)
  "Shut down and clean up the TTF system."
  (ffi:ttf-quit))

(define-wrapped-pointer-type <font>
  font?
  wrap-font unwrap-font
  (lambda (font port)
    (format port "#<font ~x>"
            (pointer-address (unwrap-font font)))))

(define (load-font file point-size)
  "Load TTF font from FILE and return a new font object whose glyph
size is POINT-SIZE."
  (let ((ptr (ffi:ttf-open-font (string->pointer file) point-size)))
    (if (null-pointer? ptr)
        (sdl-error "load-font" "failed to load font" file)
        (wrap-font ptr))))

(define (delete-font! font)
  "Delete the memory allocated for FONT."
  (ffi:ttf-close-font (unwrap-font font)))

(define (font-height font)
  "Return the maximum height of FONT."
  (ffi:ttf-font-height (unwrap-font font)))

(define (render-font-solid font text color)
  "Render TEXT, a UTF-8 encoded string, using FONT and COLOR, the
foreground color, and return a surface containing the results."
  (let ((ptr (ffi:ttf-render-utf8-solid (unwrap-font font)
                                        (string->pointer text)
                                        ((@@ (sdl2) color->struct) color))))
    (if (null-pointer? ptr)
        (sdl-error "render-font-solid" "failed to render text")
        ((@@ (sdl2 surface) wrap-surface) ptr))))

(define (render-font-blended font text color)
  "Render TEXT, a UTF-8 encoded string, using FONT and COLOR, the
foreground color, and return a high-quality alpha-blended surface
containing the results."
  (let ((ptr (ffi:ttf-render-utf8-blended (unwrap-font font)
                                          (string->pointer text)
                                          ((@@ (sdl2) color->struct) color))))
    (if (null-pointer? ptr)
        (sdl-error "render-font-solid" "failed to render text")
        ((@@ (sdl2 surface) wrap-surface) ptr))))
