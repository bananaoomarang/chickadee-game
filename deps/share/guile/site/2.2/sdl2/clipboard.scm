;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2017 David Thompson <davet@gnu.org>
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
;; OS clipboard manipulation.
;;
;;; Code:

(define-module (sdl2 clipboard)
  #:use-module (sdl2)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:use-module (system foreign)
  #:export (clipboard-text?
            clipboard-text
            set-clipboard-text!))

(define (clipboard-text?)
  "Return #t if there is text in the OS clipboard."
  (= 1 (ffi:sdl-has-clipboard-text)))

(define (clipboard-text)
  "Return a string containing the text in the OS clipboard."
  (let ((ret (ffi:sdl-get-clipboard-text)))
    (if (null-pointer? ret)
        (sdl-error "clipboard-text" "failed to get clipboard text")
        (pointer->string ret))))

(define (set-clipboard-text! str)
  "Put the text in STR into the OS clipboard."
  (unless (zero? (ffi:sdl-set-clipboard-text (string->pointer str)))
    (sdl-error "set-clipboard-text!" "failed to set clipboard text")))
