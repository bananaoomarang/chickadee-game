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
;; SDL initialization and error handling.
;;
;;; Code:

(define-module (sdl2)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:export (sdl-error-string
            sdl-error
            sdl-version
            sdl-init
            sdl-quit
            sdl-ticks

            <color>
            make-color
            color?
            color-r
            color-g
            color-b
            color-a))

(define %default-init-flags
  '(timer audio video joystick haptic game-controller events))

(define (sdl-error-string)
  "Return the current SDL error string."
  (pointer->string (ffi:sdl-get-error)))

(define (sdl-error func message . args)
  (apply throw 'sdl-error func (string-append message ": ~A")
         (append args (list (sdl-error-string)))))

(define (sdl-version)
  "Return a three element list containing the major, minor, and patch
version of the linked SDL library."
  (let ((bv (make-u8vector 3)))
    (ffi:sdl-get-version (bytevector->pointer bv))
    (u8vector->list bv)))

(define* (sdl-init #:optional (subsystems %default-init-flags))
  "Initialize the SDL library.  This must be called before using any
other SDL procedure.

SUBSYSTEMS is an optional list of symbols that specifies the
subsystems to initialize.  All subsystems are initialized by default.
The possible flags are 'timer', 'audio', 'video', 'haptic',
'game-controller', and 'events'."
  (let ((flags (apply logior
                      (map (match-lambda
                            ('timer ffi:SDL_INIT_TIMER)
                            ('audio ffi:SDL_INIT_AUDIO)
                            ('video ffi:SDL_INIT_VIDEO)
                            ('haptic ffi:SDL_INIT_HAPTIC)
                            ('game-controller ffi:SDL_INIT_GAMECONTROLLER)
                            ('joystick ffi:SDL_INIT_JOYSTICK)
                            ('events ffi:SDL_INIT_EVENTS))
                           subsystems))))
    (unless (zero? (ffi:sdl-init flags))
      (sdl-error "sdl-init" "failed to initialize subsystems ~S"
                 subsystems))))

(define (sdl-quit)
  "Quit all activated SDL subsystems.  This procedure should be called
upon all exit conditions."
  (ffi:sdl-quit))

(define (sdl-ticks)
  "Return the number of milliseconds since the SDL library was
initialized."
  (ffi:sdl-get-ticks))

;; SDL_Color
(define-record-type <color>
  (make-color r g b a)
  color?
  (r color-r)
  (g color-g)
  (b color-b)
  (a color-a))

(define (color->struct color)
  "Convert COLOR into a foreign struct."
  (match color
    (($ <color> r g b a)
     (make-c-struct ffi:sdl-color (list r g b a)))))
