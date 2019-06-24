;;; Chickadee Game Toolkit
;;; Copyright © 2017, 2018 David Thompson <davet@gnu.org>
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

(define-module (chickadee scripting channel)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chickadee queue)
  #:use-module (chickadee scripting script)
  #:export (make-channel
            channel?
            channel-get!
            channel-get
            channel-put!
            channel-put))

;; A very simplified notion of channels compared to guile-fibers.  In
;; our case, everything is cooperative and on the same thread, so we
;; have less to worry about.
(define-record-type <channel>
  (%make-channel get-queue put-queue)
  channel?
  (get-queue channel-get-queue)
  (put-queue channel-put-queue))

(define (display-channel channel port)
  (display "<channel>" port))

(set-record-type-printer! <channel> display-channel)

(define (make-channel)
  "Return a new channel."
  (%make-channel (make-queue) (make-queue)))

(define (maybe-deliver channel)
  (let ((getq (channel-get-queue channel))
        (putq (channel-put-queue channel)))
    (if (and (not (queue-empty? getq))
             (not (queue-empty? putq)))
        (match (dequeue! putq)
          ((data . put-cont)
           (let ((get-cont (dequeue! getq)))
             (get-cont data)
             (put-cont)))))))

(define (channel-get! channel proc)
  "Asynchronously retrieve a value from CHANNEL and call PROC with
that value."
  (enqueue! (channel-get-queue channel) proc)
  (maybe-deliver channel))

(define (channel-get channel)
  "Retrieve a value from CHANNEL.  The current script suspends until a
value is available."
  (yield
   (lambda (cont)
     (channel-get! channel cont))))

(define noop (lambda () #t))

(define* (channel-put! channel data #:optional (thunk noop))
  "Asynchronously send DATA to CHANNEL and call THUNK after it has
been received."
  (enqueue! (channel-put-queue channel) (cons data thunk))
  (maybe-deliver channel))

(define (channel-put channel data)
  "Send DATA to CHANNEL.  The current script suspends until another
script is available to retrieve the value."
  (yield
   (lambda (cont)
     (channel-put! channel data cont))))
