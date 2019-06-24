;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2016 David Thompson <davet@gnu.org>
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
;; Unicode text input.
;;
;;; Code:

(define-module (sdl2 input text)
  #:use-module (system foreign)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:export (start-text-input
            stop-text-input
            text-input-active?))

(define (start-text-input)
  "Enable text input events."
  (ffi:sdl-start-text-input))

(define (stop-text-input)
  "Disable text input events."
  (ffi:sdl-stop-text-input))

(define (text-input-active?)
  "Return #t if text input events are enabled."
  (= (ffi:sdl-is-text-input-active) 1))
