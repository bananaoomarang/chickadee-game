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

(define-module (chickadee scripting agenda)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (chickadee heap)
  #:export (make-agenda
            agenda?
            current-agenda
            with-agenda
            agenda-time
            update-agenda
            clear-agenda
            reset-agenda
            schedule-at
            schedule-after
            schedule-every
            at
            after
            every))

(define-record-type <agenda>
  (%make-agenda time queue)
  agenda?
  (time %agenda-time set-agenda-time!)
  (queue agenda-queue))

(define (task< a b)
  (< (car a) (car b)))

(define (make-agenda)
  "Return a new task scheduler."
  (%make-agenda 0 (make-heap task<)))

(define (schedule agenda time thunk)
  (when (<= time (%agenda-time agenda))
    (error "cannot schedule in the past" time))
  (heap-insert! (agenda-queue agenda) (cons time thunk)))

(define (%agenda-clear! agenda)
  (heap-clear! (agenda-queue agenda))
  (set-agenda-time! agenda 0)
  *unspecified*)

(define (%update-agenda agenda dt)
  (let ((queue (agenda-queue agenda))
        (time (+ (%agenda-time agenda) dt)))
    (set-agenda-time! agenda time)
    (let loop ()
      (when (not (heap-empty? queue))
        (match (heap-min queue)
          ((task-time . thunk)
           (when (<= task-time time)
             (heap-remove! queue)
             (thunk)
             (loop))))))))

(define current-agenda (make-parameter (make-agenda)))

(define-syntax-rule (with-agenda agenda body ...)
  (parameterize ((current-agenda agenda))
    body ...))

(define (agenda-time)
  "Return the current agenda time."
  (%agenda-time (current-agenda)))

(define (clear-agenda)
  "Remove all scheduled tasks from the current agenda."
  (%agenda-clear! (current-agenda)))

(define (reset-agenda)
  "Remove all scheduled tasks from the current agenda and reset time
to 0."
  (%agenda-clear! (current-agenda))
  (set-agenda-time! (current-agenda) 0))

(define (update-agenda dt)
  "Advance the current agenda by DT."
  (%update-agenda (current-agenda) dt))

(define (schedule-at time thunk)
  "Schedule THUNK to be run at TIME."
  (schedule (current-agenda) time thunk))

(define (schedule-after delay thunk)
  "Schedule THUNK to be run after DELAY."
  (schedule (current-agenda) (+ (agenda-time) delay) thunk))

(define* (schedule-every interval thunk #:optional n)
  "Schedule THUNK to run every INTERVAL amount of time.  Repeat this N
times, or indefinitely if not specified."
  (schedule-after interval
                  (lambda ()
                    (cond
                     ((not n)
                      (thunk)
                      (schedule-every interval thunk))
                     ((> n 0)
                      (thunk)
                      (schedule-every interval thunk (- n 1)))))))

(define-syntax-rule (at time body ...)
  (schedule-at time (lambda () body ...)))

(define-syntax-rule (after delay body ...)
  (schedule-after delay (lambda () body ...)))

(define-syntax every
  (syntax-rules ()
    ((_ (interval n) body ...)
     (schedule-every interval (lambda () body ...) n))
    ((_ interval body ...)
     (schedule-every interval (lambda () body ...)))))
