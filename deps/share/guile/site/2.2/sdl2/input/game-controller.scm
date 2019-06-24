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
;; Joystick input.
;;
;;; Code:

(define-module (sdl2 input game-controller)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:use-module (sdl2)
  #:export (load-game-controller-mappings!
            open-game-controller
            close-game-controller
            game-controller?
            game-controller-attached?
            game-controller-joystick
            game-controller-name
            game-controller-axis
            game-controller-button-pressed?
            game-controller-index?))

(define-wrapped-pointer-type <game-controller>
  game-controller?
  wrap-game-controller unwrap-game-controller
  (lambda (game-controller port)
    (format port "#<game-controller ~a>"
            (game-controller-name game-controller))))

(define-record-type <game-controller>
  (make-game-controller pointer joystick)
  game-controller?
  (pointer unwrap-game-controller)
  (joystick %game-controller-joystick))

(set-record-type-printer! <game-controller>
                          (lambda (game-controller port)
                            (format port "#<game-controller ~s>"
                                    (game-controller-name game-controller))))

(define wrap-joystick (@@ (sdl2 input joystick) wrap-joystick))

(define (load-game-controller-mappings! file)
  "Load game controller mappings from FILE and return the number of
mappings added this way.

See
https://raw.github.com/gabomdq/SDL_GameControllerDB/master/gamecontrollerdb.txt
for a community maintained controller mapping file."
  (let ((count (ffi:sdl-game-controller-add-mappings-from-rw
                (ffi:sdl-rw-from-file (string->pointer file)
                                      (string->pointer "rb"))
                1)))
    (if (= count -1)
        (sdl-error "load-game-controller-mappings!"
                   (string-append "failed to load game controller mappings from file "
                                  file))
        count)))

(define (open-game-controller joystick-index)
  "Return a game controller object for the physical joystick device
associated with ."
  (let ((ptr (ffi:sdl-game-controller-open joystick-index)))
    (if (null-pointer? ptr)
        (sdl-error "open-game-controller" "failed to open game controller")
        (let ((joystick (wrap-joystick
                         (ffi:sdl-game-controller-get-joystick ptr))))
         (make-game-controller ptr joystick)))))

(define (close-game-controller controller)
  "Close CONTROLLER."
  (ffi:sdl-game-controller-close (unwrap-game-controller controller)))

(define (game-controller-joystick controller)
  "Return the underlying joystick object associated with CONTROLLER."
  (%game-controller-joystick controller))

(define (game-controller-attached? controller)
  "Return #t if CONTROLLER is currently in use."
  (= (ffi:sdl-game-controller-get-attached (unwrap-game-controller controller)) 1))

(define (game-controller-name controller)
  "Return the human readable name for CONTROLLER."
  (pointer->string (ffi:sdl-game-controller-name (unwrap-game-controller controller))))

(define (axis-symbol->int axis)
  (match axis
    ('left-x ffi:SDL_CONTROLLER_AXIS_LEFTX)
    ('left-y ffi:SDL_CONTROLLER_AXIS_LEFTY)
    ('right-x ffi:SDL_CONTROLLER_AXIS_RIGHTX)
    ('right-y ffi:SDL_CONTROLLER_AXIS_RIGHTY)
    ('trigger-left ffi:SDL_CONTROLLER_AXIS_TRIGGERLEFT)
    ('trigger-right ffi:SDL_CONTROLLER_AXIS_TRIGGERRIGHT)))

(define (game-controller-axis controller axis)
  "Return a number in the range [-32768, 32767] representing the
current state of AXIS on CONTROLLER.

AXIS may be one of the following symbols:
- left-x
- left-y
- right-x
- right-y
- trigger-left
- trigger-right"
  (ffi:sdl-game-controller-get-axis (unwrap-game-controller controller)
                                    (axis-symbol->int axis)))

(define (button-symbol->int button)
  (match button
    ('a ffi:SDL_CONTROLLER_BUTTON_A)
    ('b ffi:SDL_CONTROLLER_BUTTON_B)
    ('x ffi:SDL_CONTROLLER_BUTTON_X)
    ('y ffi:SDL_CONTROLLER_BUTTON_Y)
    ('back ffi:SDL_CONTROLLER_BUTTON_BACK)
    ('guide ffi:SDL_CONTROLLER_BUTTON_GUIDE)
    ('start ffi:SDL_CONTROLLER_BUTTON_START)
    ('left-stick ffi:SDL_CONTROLLER_BUTTON_LEFTSTICK)
    ('right-stick ffi:SDL_CONTROLLER_BUTTON_RIGHTSTICK)
    ('left-shoulder ffi:SDL_CONTROLLER_BUTTON_LEFTSHOULDER)
    ('right-shoulder ffi:SDL_CONTROLLER_BUTTON_RIGHTSHOULDER)
    ('dpad-up ffi:SDL_CONTROLLER_BUTTON_DPAD_UP)
    ('dpad-down ffi:SDL_CONTROLLER_BUTTON_DPAD_DOWN)
    ('dpad-left ffi:SDL_CONTROLLER_BUTTON_DPAD_LEFT)
    ('dpad-right ffi:SDL_CONTROLLER_BUTTON_DPAD_RIGHT)))

(define (game-controller-button-pressed? controller button)
  "Return #t if BUTTON is pressed on CONTROLLER.

BUTTON may be one of the following symbols:
- a
- b
- x
- y
- back
- guide
- start
- left-stick
- right-stick
- left-shoulder
- right-shoulder
- dpad-up
- dpad-down
- dpad-left
- dpad-right"
  (= (ffi:sdl-game-controller-get-button (unwrap-game-controller controller)
                                         (button-symbol->int button))
     1))

(define (game-controller-index? joystick-index)
  "Return #t if JOYSTICK-INDEX is a valid game controller index."
  (= (ffi:sdl-is-game-controller joystick-index) 1))
