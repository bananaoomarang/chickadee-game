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

(define-module (sdl2 input joystick)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:use-module (sdl2)
  #:export (num-joysticks
            open-joystick
            close-joystick
            joystick?
            joystick-instance-id
            joystick-power-level
            joystick-num-axes
            joystick-num-balls
            joystick-num-buttons
            joystick-num-hats))

(define (num-joysticks)
  "Return the current number of connected joystick devices."
  (ffi:sdl-num-joysticks))

(define-wrapped-pointer-type <joystick>
  joystick?
  wrap-joystick unwrap-joystick
  (lambda (joystick port)
    (format port "#<joystick id: ~d>"
            (joystick-instance-id joystick))))

(define (open-joystick device-index)
  "Return a joystick object for the physical joystick device
associated with DEVICE-INDEX."
  (let ((ptr (ffi:sdl-joystick-open device-index)))
    (if (null-pointer? ptr)
        (sdl-error "open-joystick" "failed to open joystick")
        (wrap-joystick ptr))))

(define (close-joystick joystick)
  "Close JOYSTICK."
  (ffi:sdl-joystick-close (unwrap-joystick joystick)))

(define (joystick-instance-id joystick)
  "Return the instance id of JOYSTICK."
  (ffi:sdl-joystick-instance-id (unwrap-joystick joystick)))

(define (joystick-attached? joystick)
  "Return #t if JOYSTICK has been opened."
  (= (ffi:sdl-joystick-get-attached (unwrap-joystick joystick)) 1))

(define (joystick-power-level joystick)
  "Return the symbolic battery power level for JOYSTICK, either
'unknown', 'empty', 'low', 'medium', 'full', or 'wired'."
  (let ((level (ffi:sdl-joystick-current-power-level
                (unwrap-joystick joystick))))
    (cond
     ((= level ffi:SDL_JOYSTICK_POWER_UNKNOWN) 'unknown)
     ((= level ffi:SDL_JOYSTICK_POWER_EMPTY) 'empty)
     ((= level ffi:SDL_JOYSTICK_POWER_LOW) 'low)
     ((= level ffi:SDL_JOYSTICK_POWER_MEDIUM) 'medium)
     ((= level ffi:SDL_JOYSTICK_POWER_FULL) 'full)
     ((= level ffi:SDL_JOYSTICK_POWER_WIRED) 'wired))))

(define (joystick-num-axes joystick)
  "Return the number of axes for JOYSTICK."
  (ffi:sdl-joystick-num-axes (unwrap-joystick joystick)))

(define (joystick-num-balls joystick)
  "Return the number of balls for JOYSTICK."
  (ffi:sdl-joystick-num-balls (unwrap-joystick joystick)))

(define (joystick-num-buttons joystick)
  "Return the number of buttons for JOYSTICK."
  (ffi:sdl-joystick-num-buttons (unwrap-joystick joystick)))

(define (joystick-num-hats joystick)
  "Return the number of hats for JOYSTICK."
  (ffi:sdl-joystick-num-hats (unwrap-joystick joystick)))

(define (joystick-axis joystick axis-index)
  "Return a number in the range [-32768, 32767] representing the
current state of AXIS-INDEX on JOYSTICK.  On most joysticks, use 0 to
query the X axis and 1 to query the Y axis."
  (ffi:sdl-joystick-get-axis (unwrap-joystick joystick) axis-index))

(define (joystick-button-pressed? joystick button-index)
  "Return #t if BUTTON-INDEX is pressed on JOYSTICK.  Button indices
start from 0."
  (= (ffi:sdl-joystick-get-button (unwrap-joystick joystick) button-index) 1))

;; TODO: joystick-hat and joystick-ball
