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

(define-module (chickadee render blend)
  #:use-module (ice-9 match)
  #:use-module (gl)
  #:use-module (chickadee render gl)
  #:use-module (chickadee render gpu)
  #:export (*blend-mode-state*
            *depth-test-state*))

(define (apply-blend-mode blend-mode)
  (if blend-mode
      (begin
        (gl-enable (enable-cap blend))
        (match blend-mode
          ('alpha
           (gl-blend-equation (blend-equation-mode-ext func-add-ext))
           (gl-blend-func (blending-factor-src src-alpha)
                          (blending-factor-dest one-minus-src-alpha)))
          ('multiply
           (gl-blend-equation (blend-equation-mode-ext func-add-ext))
           (gl-blend-func (blending-factor-src dst-color)
                          (blending-factor-dest zero)))
          ('subtract
           (gl-blend-equation
            (blend-equation-mode-ext func-reverse-subtract-ext))
           (gl-blend-func (blending-factor-src one)
                          (blending-factor-dest zero)))
          ('add
           (gl-blend-equation (blend-equation-mode-ext func-add-ext))
           (gl-blend-func (blending-factor-src one)
                          (blending-factor-dest one)))
          ('lighten
           (gl-blend-equation (blend-equation-mode-ext max-ext))
           (gl-blend-func (blending-factor-src one)
                          (blending-factor-dest zero)))
          ('darken
           (gl-blend-equation (blend-equation-mode-ext min-ext))
           (gl-blend-func (blending-factor-src one)
                          (blending-factor-dest zero)))
          ('screen
           (gl-blend-equation (blend-equation-mode-ext func-add-ext))
           (gl-blend-func (blending-factor-src one)
                          (blending-factor-dest one-minus-src-color)))
          ('replace
           (gl-blend-equation (blend-equation-mode-ext func-add-ext))
           (gl-blend-func (blending-factor-src one)
                          (blending-factor-dest zero)))))
      (gl-disable (enable-cap blend))))

(define *blend-mode-state* (make-gpu-state apply-blend-mode 'replace))

(define (apply-depth-test depth-test?)
  (if depth-test?
      (gl-enable (enable-cap depth-test))
      (gl-disable (enable-cap depth-test))))

(define *depth-test-state* (make-gpu-state apply-depth-test #f))
