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
;; Image loading.
;;
;;; Code:

(define-module (sdl2 image)
  #:use-module (system foreign)
  #:use-module (sdl2)
  #:use-module ((sdl2 bindings image) #:prefix ffi:)
  #:export (image-init
            image-quit
            load-image
            save-png))

(define (image-init)
  "Initialize dynamically loaded image libraries."
  (ffi:img-init (logior ffi:IMG_INIT_JPG
                        ffi:IMG_INIT_PNG
                        ffi:IMG_INIT_TIF
                        ffi:IMG_INIT_WEBP))
  *unspecified*)

(define (image-quit)
  "Clean up dynamically loaded image libraries."
  (ffi:img-quit))

(define (load-image file)
  "Load the image in FILE and return an SDL surface."
  (let ((ptr (ffi:img-load (string->pointer file))))
    (if (null-pointer? ptr)
        (sdl-error "load-image" "failed to load image: ~a" file)
        ((@@ (sdl2 surface) wrap-surface) ptr))))

(define (save-png surface file)
  "Save SURFACE to FILE as a PNG formatted image."
  (unless (zero? (ffi:img-save-png ((@@ (sdl2 surface) unwrap-surface) surface)
                                   (string->pointer file)))
    (sdl-error "save-png" "failed to save surface as PNG: ~a" file)))
