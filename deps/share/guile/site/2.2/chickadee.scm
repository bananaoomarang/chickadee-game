;;; Chickadee Game Toolkit
;;; Copyright © 2018 David Thompson <davet@gnu.org>
;;;
;;; Chickadee is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; Chickadee is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Simple SDL + OpenGL game loop implementation.
;;
;;; Code:

(define-module (chickadee)
  #:use-module (sdl2)
  #:use-module (sdl2 events)
  #:use-module ((sdl2 input game-controller) #:prefix sdl2:)
  #:use-module (sdl2 input joystick)
  #:use-module ((sdl2 input keyboard) #:prefix sdl2:)
  #:use-module ((sdl2 input mouse) #:prefix sdl2:)
  #:use-module (sdl2 input text)
  #:use-module (sdl2 mixer)
  #:use-module (sdl2 video)
  #:use-module (chickadee config)
  #:use-module (chickadee game-loop)
  #:use-module (chickadee math matrix)
  #:use-module (chickadee render)
  #:use-module (chickadee render color)
  #:use-module (chickadee render gl)
  #:use-module (chickadee render gpu)
  #:use-module (chickadee render viewport)
  #:use-module (chickadee utils)
  #:export (current-window
            controller-button-pressed?
            controller-axis
            controller-name
            key-pressed?
            key-released?
            mouse-x
            mouse-y
            mouse-button-pressed?
            mouse-button-released?
            run-game)
  #:re-export (abort-game))

(define (key-pressed? key)
  "Return #t if KEY is currently being pressed."
  (sdl2:key-pressed? key))

(define (key-released? key)
  "Return #t if KEY is not currently being pressed."
  (sdl2:key-released? key))

(define (mouse-x)
  "Return the current X coordinate of the mouse cursor."
  (sdl2:mouse-x))

(define (mouse-y)
  "Return the current Y coordinate of the mouse cursor."
  (sdl2:mouse-y))

(define (mouse-button-pressed? button)
  "Return #t if BUTTON is currently being pressed."
  (sdl2:mouse-button-pressed? button))

(define (mouse-button-released? button)
  "Return #t if BUTTON is not currently being pressed."
  (sdl2:mouse-button-released? button))

(define *controllers* (make-hash-table))

(define (lookup-controller joystick-id)
  (hashv-ref *controllers* joystick-id))

(define (add-controller joystick-index)
  (let ((controller (sdl2:open-game-controller joystick-index)))
    (hashv-set! *controllers*
                (joystick-instance-id
                 (sdl2:game-controller-joystick controller))
                controller)
    controller))

(define (remove-controller joystick-id)
  (hashv-remove! *controllers* joystick-id))

(define (controller-button-pressed? controller button)
  "Return #t if BUTTON is currently being pressed on CONTROLLER."
  (sdl2:game-controller-button-pressed? controller button))

(define (controller-axis controller axis)
  "Return a floating point value in the range [-1, 1] corresponding to
how much AXIS is being pushed on CONTROLLER.  0 is returned if AXIS is
not being pushed at all."
  (/ (sdl2:game-controller-axis controller axis)
     32768.0))

(define controller-name
  ;; Memoize to avoid repeated allocation of strings via
  ;; pointer->string.
  (memoize
   (lambda (controller)
     (sdl2:game-controller-name controller))))

(define current-window (make-parameter #f))

(define* (run-game #:key
                   (window-title "Chickadee!")
                   (window-width 640)
                   (window-height 480)
                   window-fullscreen?
                   (update-hz 60)
                   (load (const #t))
                   (update (const #t))
                   (draw (const #t))
                   (quit abort-game)
                   (key-press (const #t))
                   (key-release (const #t))
                   (text-input (const #t))
                   (mouse-press (const #t))
                   (mouse-release (const #t))
                   (mouse-move (const #t))
                   (controller-add (const #t))
                   (controller-remove (const #t))
                   (controller-press (const #t))
                   (controller-release (const #t))
                   (controller-move (const #t))
                   error)
  (sdl-init)
  (false-if-exception (mixer-init))
  (open-audio)
  (start-text-input)
  (let* ((window (make-window #:opengl? #t
                              #:title window-title
                              #:size (list window-width window-height)
                              #:fullscreen? window-fullscreen?))
         (gl-context (make-gl-context window))
         (default-viewport (make-viewport 0 0 window-width window-height))
         (default-projection (orthographic-projection 0 window-width
                                                      window-height 0
                                                      0 1)))
    (define (invert-y y)
      ;; SDL's origin is the top-left, but our origin is the bottom
      ;; left so we need to invert Y coordinates that SDL gives us.
      (- window-height y))
    (define (input-sdl)
      (define (process-event event)
        (cond
         ((quit-event? event)
          (quit))
         ((keyboard-down-event? event)
          (key-press (keyboard-event-key event)
                     (keyboard-event-scancode event)
                     (keyboard-event-modifiers event)
                     (keyboard-event-repeat? event)))
         ((keyboard-up-event? event)
          (key-release (keyboard-event-key event)
                       (keyboard-event-scancode event)
                       (keyboard-event-modifiers event)))
         ((text-input-event? event)
          (text-input (text-input-event-text event)))
         ((mouse-button-down-event? event)
          (mouse-press (mouse-button-event-button event)
                       (mouse-button-event-clicks event)
                       (mouse-button-event-x event)
                       (invert-y (mouse-button-event-y event))))
         ((mouse-button-up-event? event)
          (mouse-release (mouse-button-event-button event)
                         (mouse-button-event-x event)
                         (invert-y (mouse-button-event-y event))))
         ((mouse-motion-event? event)
          (mouse-move (mouse-motion-event-x event)
                      (invert-y (mouse-motion-event-y event))
                      (mouse-motion-event-x-rel event)
                      (- (mouse-motion-event-y-rel event))
                      (mouse-motion-event-buttons event)))
         ((and (controller-device-event? event)
               (eq? (controller-device-event-action event) 'added))
          (controller-add (add-controller
                           (controller-device-event-which event))))
         ((and (controller-device-event? event)
               (eq? (controller-device-event-action event) 'removed))
          (let ((controller (lookup-controller
                             (controller-device-event-which event))))
            (controller-remove controller)
            (remove-controller (controller-device-event-which event))
            (sdl2:close-game-controller controller)))
         ((controller-button-down-event? event)
          (controller-press (lookup-controller
                             (controller-button-event-which event))
                            (controller-button-event-button event)))
         ((controller-button-up-event? event)
          (controller-release (lookup-controller
                               (controller-button-event-which event))
                              (controller-button-event-button event)))
         ((controller-axis-event? event)
          (controller-move (lookup-controller
                            (controller-axis-event-which event))
                           (controller-axis-event-axis event)
                           (/ (controller-axis-event-value event) 32768.0)))))
      ;; Process all pending events.
      (let loop ((event (poll-event)))
        (when event
          (process-event event)
          (loop (poll-event)))))
    (define (update-sdl dt)
      (input-sdl)
      (update dt)
      ;; Free any GPU resources that have been GC'd.
      (gpu-reap!))
    (define (render-sdl-opengl alpha)
      ;; Switch to the null viewport to ensure that
      ;; the default viewport will be re-applied and
      ;; clear the screen.
      (gpu-state-set! *viewport-state* null-viewport)
      (with-viewport default-viewport
        (with-projection default-projection
          (draw alpha)))
      (swap-gl-window window))
    (dynamic-wind
      (const #t)
      (lambda ()
        (parameterize ((current-window window))
          ;; Attempt to activate vsync, if possible.  Some systems do
          ;; not support setting the OpenGL swap interval.
          (catch #t
            (lambda ()
              (set-gl-swap-interval! 'vsync))
            (lambda args
              (display "warning: could not enable vsync\n"
                       (current-error-port))))
          (load)
          (sdl2:load-game-controller-mappings!
           (scope-datadir "gamecontrollerdb.txt"))
          (run-game* #:update update-sdl
                     #:render render-sdl-opengl
                     #:error error
                     #:time sdl-ticks
                     #:update-hz update-hz)))
      (lambda ()
        (delete-gl-context! gl-context)
        (close-window! window)))))
