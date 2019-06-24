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
;; Low-level FFI bindings for SDL2_image.
;;
;;; Code:

(define-module (sdl2 bindings image)
  #:use-module (system foreign)
  #:use-module (sdl2 config))

(define sdl-image-func
  (let ((lib (dynamic-link %libsdl2-image)))
    (lambda (return-type function-name arg-types)
      "Return a procedure for the foreign function FUNCTION-NAME in
the SDL2_image shared library.  That function must return a value of
RETURN-TYPE and accept arguments of ARG-TYPES."
      (pointer->procedure return-type
                          (dynamic-func function-name lib)
                          arg-types))))

(define-syntax-rule (define-foreign name return-type func-name arg-types)
  (define-public name
    (sdl-image-func return-type func-name arg-types)))

(define-public IMG_INIT_JPG  #x00000001)
(define-public IMG_INIT_PNG  #x00000002)
(define-public IMG_INIT_TIF  #x00000004)
(define-public IMG_INIT_WEBP #x00000008)

(define-foreign img-init
  int "IMG_Init" (list int))

(define-foreign img-quit
  void "IMG_Quit" '())

(define-foreign img-load
  '* "IMG_Load" '(*))

(define-foreign img-save-png
  int "IMG_SavePNG" '(* *))
