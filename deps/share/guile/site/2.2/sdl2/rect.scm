;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2018 David Thompson <davet@gnu.org>
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
;; SDL surface manipulation.
;;
;;; Code:

(define-module (sdl2 rect)
  #:use-module (rnrs bytevectors)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:export (make-rect
            rect?
            rect-x
            rect-y
            rect-width
            rect-height))

(define-record-type <rect>
  (%make-rect bv ptr)
  rect?
  (bv rect-bv)
  (ptr unwrap-rect))

(define (make-rect x y width height)
  (let ((bv (s32vector x y width height)))
    (%make-rect bv (bytevector->pointer bv))))

(define (rect-x rect)
  (s32vector-ref (rect-bv rect) 0))

(define (rect-y rect)
  (s32vector-ref (rect-bv rect) 1))

(define (rect-width rect)
  (s32vector-ref (rect-bv rect) 2))

(define (rect-height rect)
  (s32vector-ref (rect-bv rect) 3))
