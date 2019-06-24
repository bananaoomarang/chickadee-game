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

(define-module (chickadee scripting script)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (script?
            script-cancelled?
            script-running?
            script-complete?
            spawn-script
            script
            cancel-script
            yield)
  #:replace (yield))

(define-record-type <script>
  (make-script status children)
  script?
  (status script-status set-script-status!)
  (children script-children set-script-children!))

(define current-script (make-parameter #f))

(define (display-script script port)
  (format port "<script status: ~a>" (script-status script)))

(set-record-type-printer! <script> display-script)

(define (script-cancelled? script)
  "Return #t if SCRIPT has been cancelled."
  (eq? 'cancelled (script-status script)))

(define (script-running? script)
  "Return #t if SCRIPT has not yet terminated or been cancelled."
  (eq? 'running (script-status script)))

(define (script-complete? script)
  "Return #t if SCRIPT has terminated."
  (eq? 'complete (script-status script)))

(define (cancel-script script)
  "Prevent further execution of SCRIPT."
  (set-script-status! script 'cancelled)
  (for-each cancel-script (script-children script))
  *unspecified*)

(define script-prompt (make-prompt-tag 'script))

(define (spawn-script thunk)
  "Apply THUNK as a script."
  (let ((script (make-script 'running '())))
    (define (handler cont callback . args)
      (define (resume . args)
        ;; Call the continuation that resumes the script, unless the
        ;; script has been cancelled in the meanwhile.
        (unless (script-cancelled? script)
          (call-with-prompt script-prompt
            (lambda () (apply cont args))
            handler)))
      (when (procedure? callback)
        (apply callback resume args)))
    (define task
      (let ((dynamic-state (current-dynamic-state)))
        (lambda ()
          (with-dynamic-state
           dynamic-state
           (lambda ()
             (current-script script)
             (thunk)
             (set-script-status! script 'complete))))))
    ;; Register child script with parent.  Cancelling the parent will
    ;; cause all children to be cancelled as well.
    (when (script? (current-script))
      (set-script-children! (current-script)
                            (cons script (script-children (current-script)))))
    ;; Start the script.
    (call-with-prompt script-prompt task handler)
    script))

(define-syntax-rule (script body ...)
  "Evaluate BODY in a script."
  (spawn-script (lambda () body ...)))

(define (yield handler)
  "Suspend the current script and pass its continuation to the
procedure HANDLER."
  (abort-to-prompt script-prompt handler))
