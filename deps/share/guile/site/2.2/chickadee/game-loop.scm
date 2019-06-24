;;; Chickadee Game Toolkit
;;; Copyright © 2016, 2018 David Thompson <davet@gnu.org>
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

(define-module (chickadee game-loop)
  #:export (run-game*
            abort-game))


;;;
;;; Error handling
;;;

(define (call-with-error-handling handler thunk)
  (if handler
      (let ((stack #f))
        (catch #t
          (lambda ()
            (thunk)
            #f)
          (lambda (key . args)
            (handler stack key args)
            #t)
          (lambda (key . args)
            (set! stack (make-stack #t 3)))))
      (begin
        (thunk)
        #f)))

(define-syntax-rule (with-error-handling handler body ...)
  (call-with-error-handling handler (lambda () body ...)))

(define (default-error-handler stack key args)
  (apply throw key args))


;;;
;;; Game loop kernel
;;;

(define game-loop-prompt-tag (make-prompt-tag 'game-loop))

(define (abort-game)
  (abort-to-prompt game-loop-prompt-tag #f))

(define* (run-game* #:key update render time error
                    (update-hz 60))
  (let ((timestep (round (/ 1000 update-hz))))
    (call-with-prompt game-loop-prompt-tag
      (lambda ()
        ;; Catch SIGINT and kill the loop.
        (sigaction SIGINT
          (lambda (signum)
            (abort-game)))
        ;; A simple analogy is that we are filling up a bucket
        ;; with water.  When the bucket fills up to a marked
        ;; line, we dump it out.  Our water is time, and each
        ;; time we dump the bucket we update the game.  Updating
        ;; the game on a fixed timestep like this yields a
        ;; stable simulation.
        (let loop ((previous-time (time))
                   (buffer 0))
          (let* ((current-time (time))
                 (delta (- current-time previous-time)))
            (let update-loop ((buffer (+ buffer delta)))
              (if (>= buffer timestep)
                  ;; Short-circuit the update loop if an error
                  ;; occurred, and reset the current time to now in
                  ;; order to discard the undefined amount of time
                  ;; that was spent handling the error.
                  (if (with-error-handling error (update timestep))
                      (loop (time) 0)
                      (begin
                        (usleep 1)
                        (update-loop (- buffer timestep))))
                  (begin
                    ;; We render upon every iteration of the loop, and
                    ;; thus rendering is decoupled from updating.
                    ;; It's possible to render multiple times before
                    ;; an update is performed.
                    (if (with-error-handling error (render (/ buffer timestep)))
                        (loop (time) 0)
                        (loop current-time buffer))))))))
      (lambda (cont callback)
        #f))))
