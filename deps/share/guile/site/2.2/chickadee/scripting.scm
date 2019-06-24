;;; Chickadee Game Toolkit
;;; Copyright © 2017 David Thompson <davet@gnu.org>
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

(define-module (chickadee scripting)
  #:use-module (chickadee math)
  #:use-module (chickadee math easings)
  #:use-module (chickadee scripting agenda)
  #:use-module (chickadee scripting channel)
  #:use-module (chickadee scripting script)
  #:export (forever
            repeat
            sleep
            tween)
  #:replace (sleep))

;; Export public bindings from other modules.
(eval-when (eval load compile)
  (begin
    (define %public-modules
      '(agenda channel script))
    (for-each (let ((i (module-public-interface (current-module))))
                (lambda (m)
                  (module-use! i (resolve-interface
                                  `(chickadee scripting ,m)))))
              %public-modules)))

(define-syntax-rule (forever body ...)
  "Evaluate BODY in an endless loop."
  (while #t body ...))

(define-syntax-rule (repeat n body ...)
  "Evaluate BODY N times."
  (let loop ((i 0))
    (when (< i n)
      body ...
      (loop (+ i 1)))))

(define (sleep duration)
  "Wait DURATION before resuming the current script."
  ;; Capture the current agenda before suspending the script so that
  ;; we schedule the continuation in the right place.
  (let ((agenda (current-agenda)))
    (yield
     (lambda (cont)
       (with-agenda agenda
         (schedule-after duration cont))))))

(define* (tween duration start end proc #:key
                (step 1)
                (ease smoothstep)
                (interpolate lerp))
  "Transition a value from START to END over DURATION, sending each
succesive value to PROC.  STEP controls the amount of time between
each update of the animation.

The EASE procedure controls the rate at which the animation advances.
The smoothstep easing function is used by default.

The INTERPOLATE procedure computes the values in between START and
END.  By default, linear interpolation is used."
  (let loop ((t 0))
    (if (>= t duration)
        (proc end)
        (let ((alpha (ease (/ t duration))))
          (proc (interpolate start end alpha))
          (sleep step)
          (loop (+ t step))))))
