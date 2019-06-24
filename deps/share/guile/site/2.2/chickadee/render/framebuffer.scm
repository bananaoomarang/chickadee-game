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

;;; Commentary:
;;
;; Render to texture.
;;
;;; Code:

(define-module (chickadee render framebuffer)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (gl)
  #:use-module (gl enums)
  #:use-module (chickadee math matrix)
  #:use-module (chickadee render gl)
  #:use-module (chickadee render gpu)
  #:use-module ((chickadee render texture) #:select (make-texture null-texture))
  #:use-module (chickadee render viewport)
  #:export (make-framebuffer
            framebuffer?
            framebuffer-texture
            framebuffer-viewport
            framebuffer-projection
            null-framebuffer
            apply-framebuffer
            *framebuffer-state*))

(define (generate-framebuffer)
  "Generate a new OpenGL framebuffer object."
  (let ((bv (u32vector 1)))
    (gl-gen-framebuffers 1 (bytevector->pointer bv))
    (u32vector-ref bv 0)))

(define (generate-renderbuffer)
  "Generate a new OpenGL renderbuffer object."
  (let ((bv (u32vector 1)))
    (gl-gen-renderbuffers 1 (bytevector->pointer bv))
    (u32vector-ref bv 0)))

(define-record-type <framebuffer>
  (%make-framebuffer id renderbuffer-id texture viewport projection)
  framebuffer?
  (id framebuffer-id)
  (renderbuffer-id framebuffer-renderbuffer-id)
  (texture framebuffer-texture)
  (viewport framebuffer-viewport)
  (projection framebuffer-projection))

(define null-framebuffer
  (%make-framebuffer 0 0 null-texture null-viewport (make-identity-matrix4)))

(define <<framebuffer>> (class-of null-framebuffer))

(define (free-framebuffer framebuffer)
  (gl-delete-renderbuffers 1
                           (bytevector->pointer
                            (u32vector
                             (framebuffer-renderbuffer-id framebuffer))))
  (gl-delete-framebuffers 1
                          (bytevector->pointer
                            (u32vector
                             (framebuffer-id framebuffer)))))

(define-method (gpu-finalize (framebuffer <<framebuffer>>))
  (free-framebuffer framebuffer))

(define (apply-framebuffer framebuffer)
  (gl-bind-framebuffer (version-3-0 framebuffer)
                       (framebuffer-id framebuffer)))

(define *framebuffer-state*
  (make-gpu-state apply-framebuffer null-framebuffer))

(define make-framebuffer
  (let ((draw-buffers (u32vector (version-3-0 color-attachment0))))
    (lambda* (width height #:key (min-filter 'linear) (mag-filter 'linear)
                    (wrap-s 'repeat) (wrap-t 'repeat))
      "Create a new framebuffer that renders to a texture with
dimensions WIDTH x HEIGHT."
      (let* ((framebuffer-id (generate-framebuffer))
             (renderbuffer-id (generate-renderbuffer))
             (texture (make-texture #f width height
                                    #:flip? #t
                                    #:min-filter min-filter
                                    #:mag-filter mag-filter
                                    #:wrap-s wrap-s
                                    #:wrap-t wrap-t))
             ;; It is convenient to make a default viewport and
             ;; projection matrix for the framebuffer so that the
             ;; rendering engine can set it whenever it changes to
             ;; this framebuffer, saving users the trouble of having
             ;; to this tedious task themselves.
             (viewport (make-viewport 0 0 width height))
             (projection (orthographic-projection 0 width height 0 0 1))
             (framebuffer (%make-framebuffer framebuffer-id
                                             renderbuffer-id
                                             texture
                                             viewport
                                             projection)))
        (gpu-state-set! *framebuffer-state* framebuffer)
        ;; Setup depth buffer.
        (gl-bind-renderbuffer (version-3-0 renderbuffer)
                              renderbuffer-id)
        (gl-renderbuffer-storage (version-3-0 renderbuffer)
                                 (pixel-format depth-component)
                                 width
                                 height)
        (gl-framebuffer-renderbuffer (version-3-0 framebuffer)
                                     (arb-framebuffer-object depth-attachment)
                                     (version-3-0 renderbuffer)
                                     renderbuffer-id)
        ;; Setup framebuffer.
        (gl-framebuffer-texture-2d (version-3-0 framebuffer)
                                   (version-3-0 color-attachment0)
                                   (texture-target texture-2d)
                                   ((@@ (chickadee render texture) texture-id)
                                    texture)
                                   0)
        (gl-draw-buffers 1 (bytevector->pointer draw-buffers))
        ;; Check for errors.
        (unless (= (gl-check-framebuffer-status (version-3-0 framebuffer))
                   (version-3-0 framebuffer-complete))
          (error "Framebuffer cannot be created"))
        framebuffer))))
