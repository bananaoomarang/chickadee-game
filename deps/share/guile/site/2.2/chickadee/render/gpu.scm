;;; Chickadee Game Toolkit
;;; Copyright © 2016 David Thompson <davet@gnu.org>
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

(define-module (chickadee render gpu)
  #:use-module (oop goops)
  #:use-module (srfi srfi-9)
  #:export (make-gpu-state
            gpu-state-ref
            gpu-state-set!

            gpu-finalize
            gpu-guard
            gpu-reap!))


;;;
;;; GPU state
;;;

(define-record-type <gpu-state>
  (make-gpu-state bind value)
  gpu-state?
  (bind gpu-state-bind)
  (value gpu-state-ref %gpu-state-set!))

(define (gpu-state-set! state new-value)
  (unless (eq? new-value (gpu-state-ref state))
    ((gpu-state-bind state) new-value)
    (%gpu-state-set! state new-value)))

;;;
;;; GPU finalizers
;;;

(define-generic gpu-finalize)

(define *gpu-guardian* (make-guardian))

(define (gpu-guard obj)
  "Protect OBJ for the garbage collector until OBJ has been deleted
from the GPU's memory."
  (*gpu-guardian* obj)
  obj)

(define (gpu-reap!)
  "Delete all GPU objects that are no longer being referenced."
  (let loop ((obj (*gpu-guardian*)))
    (when obj
      (gpu-finalize obj)
      (loop (*gpu-guardian*)))))
