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
;; Low-level FFI bindings for SDL2_ttf.
;;
;;; Code:

(define-module (sdl2 bindings ttf)
  #:use-module (system foreign)
  #:use-module (sdl2 config)
  #:use-module (sdl2 bindings))

(define sdl-ttf-func
  (let ((lib (dynamic-link %libsdl2-ttf)))
    (lambda (return-type function-name arg-types)
      "Return a procedure for the foreign function FUNCTION-NAME in
the SDL2_ttf shared library.  That function must return a value of
RETURN-TYPE and accept arguments of ARG-TYPES."
      (pointer->procedure return-type
                          (dynamic-func function-name lib)
                          arg-types))))

(define-syntax-rule (define-foreign name return-type func-name arg-types)
  (define-public name
    (sdl-ttf-func return-type func-name arg-types)))

(define-foreign ttf-init
  int "TTF_Init" '())

(define-foreign ttf-quit
  void "TTF_Quit" '())

(define-foreign ttf-open-font
  '* "TTF_OpenFont" (list '* int))

(define-foreign ttf-close-font
  void "TTF_CloseFont" '(*))

(define-foreign ttf-font-height
  int "TTF_FontHeight" '(*))

(define-foreign ttf-render-text-solid
  '* "TTF_RenderText_Solid" (list '* '* sdl-color))

(define-foreign ttf-render-utf8-solid
  '* "TTF_RenderUTF8_Solid" (list '* '* sdl-color))

(define-foreign ttf-render-text-shaded
  '* "TTF_RenderText_Shaded" (list '* '* sdl-color sdl-color))

(define-foreign ttf-render-utf8-shaded
  '* "TTF_RenderUTF8_Shaded" (list '* '* sdl-color sdl-color))

(define-foreign ttf-render-text-blended
  '* "TTF_RenderText_Blended" (list '* '* sdl-color))

(define-foreign ttf-render-utf8-blended
  '* "TTF_RenderUTF8_Blended" (list '* '* sdl-color))
