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

;;; Commentary:
;;
;; High-level rendering API.
;;
;;; Code:

(define-module (chickadee render)
  #:use-module (chickadee math matrix)
  #:use-module (chickadee render gpu)
  #:use-module (chickadee render blend)
  #:use-module (chickadee render framebuffer)
  #:use-module (chickadee render shader)
  #:use-module (chickadee render texture)
  #:use-module (chickadee render buffer)
  #:use-module (chickadee render viewport)
  #:export (current-viewport
            current-framebuffer
            current-blend-mode
            current-depth-test
            current-texture
            current-projection
            with-viewport
            with-framebuffer
            with-blend-mode
            with-depth-test
            with-texture
            with-projection
            gpu-apply
            gpu-apply*
            gpu-apply/instanced*
            gpu-apply/instanced))

(define *current-viewport* null-viewport)
(define *current-framebuffer* null-framebuffer)
(define *current-blend-mode* 'replace)
(define *current-depth-test* #f)
(define *current-projection* (make-identity-matrix4))
(define *current-textures* (make-vector 32 null-texture))

(define (current-viewport)
  *current-viewport*)

(define (current-framebuffer)
  *current-framebuffer*)

(define (current-blend-mode)
  *current-blend-mode*)

(define (current-depth-test)
  *current-depth-test*)

(define (current-texture i)
  (vector-ref *current-textures* i))

(define (current-projection)
  *current-projection*)

(define-syntax-rule (with (name value) body ...)
  (let ((prev name))
    (dynamic-wind
      (lambda () (set! name value))
      (lambda () body ...)
      (lambda () (set! name prev)))))

(define-syntax-rule (with-viewport viewport body ...)
  (with (*current-viewport* viewport) body ...))

(define-syntax-rule (with-framebuffer framebuffer body ...)
  (with (*current-framebuffer* framebuffer)
        ;; As a convenience, initialize the viewport and projection
        ;; matrix as well so that the user doesn't have to explicitly
        ;; make a viewport and/or projection matrix unless they
        ;; actually want to do fancy viewport manipulations.
        (with-viewport (framebuffer-viewport framebuffer)
          (with-projection (framebuffer-projection framebuffer)
            body ...))))

(define-syntax-rule (with-blend-mode blend-mode body ...)
  (with (*current-blend-mode* blend-mode) body ...))

(define-syntax-rule (with-depth-test depth-test body ...)
  (with (*current-depth-test* depth-test) body ...))

(define-syntax-rule (with-texture n texture body ...)
  (let ((prev (vector-ref *current-textures* n)))
    (dynamic-wind
      (lambda () (vector-set! *current-textures* n texture))
      (lambda () body ...)
      (lambda () (vector-set! *current-textures* n prev)))))

(define-syntax-rule (with-shader shader body ...)
  (with (*current-shader* shader)
        (initialize-uniforms)
        body ...))

(define-syntax-rule (with-vertex-array vertex-array body ...)
  (with (*current-vertex-array* vertex-array) body ...))

(define-syntax-rule (with-projection matrix body ...)
  (with (*current-projection* matrix) body ...))

;; (define (initialize-uniforms)
;;   (hash-for-each (lambda (name uniform)
;;                    (unless (hash-get-handle *current-uniforms* name)
;;                      (hash-set! *current-uniforms* name
;;                                 (uniform-default-value uniform))))
;;                  (shader-uniforms *current-shader*)))

;; (define-syntax uniform-let
;;   (syntax-rules ()
;;     ((_ () body ...) (begin body ...))
;;     ((_ ((name value) . rest) body ...)
;;      (let ((uniform (shader-uniform (current-shader) name))
;;            (prev (hash-ref *current-uniforms* name)))
;;        (if uniform
;;            (dynamic-wind
;;              (lambda ()
;;                (hash-set! *current-uniforms* name value))
;;              (lambda ()
;;                (uniform-let rest body ...))
;;              (lambda ()
;;                (hash-set! *current-uniforms* name prev)))
;;            (error "no such uniform: " name))))))

;; (define (uniform-ref name)
;;   (uniform-value (shader-uniform (current-shader) name)))

(define (keyword->string kw)
  (symbol->string (keyword->symbol kw)))

(define-syntax uniform-apply
  (lambda (x)
    (syntax-case x ()
      ((_ shader ()) (datum->syntax x #t))
      ((_ shader (name value . rest))
       (with-syntax ((sname (datum->syntax x (keyword->string
                                              (syntax->datum #'name)))))
         #'(begin
             (set-uniform-value! (shader-uniform shader sname) value)
             (uniform-apply shader rest)))))))

(define-syntax-rule (gpu-prepare shader vertex-array uniforms)
  (begin
    ;; It's important that the framebuffer is set before setting the
    ;; viewport because applying a new viewport will clear the current
    ;; framebuffer.
    (gpu-state-set! *framebuffer-state* (current-framebuffer))
    (gpu-state-set! *viewport-state* (current-viewport))
    (gpu-state-set! *blend-mode-state* (current-blend-mode))
    (gpu-state-set! *depth-test-state* (current-depth-test))
    (gpu-state-set! *shader-state* shader)
    (let loop ((i 0))
      (when (< i 32)
        (texture-set! i (current-texture i))
        (loop (1+ i))))
    (uniform-apply shader uniforms)
    (hash-for-each (lambda (name uniform)
                     (when (eq? 'sampler-2d (uniform-type uniform))
                       (set-uniform-value! uniform (uniform-value uniform))))
                   (shader-uniforms shader))))

(define-syntax-rule (gpu-apply* shader vertex-array count . uniforms)
  (begin
    (gpu-prepare shader vertex-array uniforms)
    (render-vertices vertex-array count)))

(define-syntax-rule (gpu-apply shader vertex-array uniforms ...)
  (gpu-apply* shader vertex-array #f uniforms ...))

(define-syntax-rule (gpu-apply/instanced* shader vertex-array count instances .
                                          uniforms)
  (begin
    (gpu-prepare shader vertex-array uniforms)
    (render-vertices/instanced vertex-array instances count)))

(define-syntax-rule (gpu-apply/instanced shader vertex-array instances
                                         uniforms ...)
  (gpu-apply/instanced* shader vertex-array #f instances uniforms ...))
