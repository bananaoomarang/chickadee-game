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
;; Custom wrappers over low level OpenGL commands that aren't part of
;; guile-opengl.
;;
;;; Code:

(define-module (chickadee render gl)
  #:use-module (srfi srfi-4)
  #:use-module ((system foreign) #:select (bytevector->pointer))
  #:use-module (gl)
  #:use-module ((gl low-level) #:renamer (symbol-prefix-proc '%))
  #:use-module (gl enums)
  #:use-module (gl runtime)
  #:use-module (gl types))

(re-export (%glClearColor . gl-clear-color)
           (%glScissor . gl-scissor)
           (%glBlendFunc . gl-blend-func)
           (%glBlendEquation . gl-blend-equation))

;;;
;;; 3.8.1 Texture Image Specification
;;;

(re-export (%glTexImage3D . gl-texture-image-3d)
           (%glTexImage2D . gl-texture-image-2d)
           (%glTexImage1D . gl-texture-image-1d))

;;;
;;; 3.8.2 Alternate Texture Image Specification Commands
;;;

(re-export (%glCopyTexImage2D . gl-copy-texture-image-2d)
           (%glCopyTexImage1D . gl-copy-texture-image-1d)
           (%glCopyTexSubImage3D . gl-copy-texture-sub-image-3d)
           (%glCopyTexSubImage2D . gl-copy-texture-sub-image-2d)
           (%glCopyTexSubImage1D . gl-copy-texture-sub-image-1d)
           (%glTexSubImage3D . gl-texture-sub-image-3d)
           (%glTexSubImage2D . gl-texture-sub-image-2d)
           (%glTexSubImage1D . gl-texture-sub-image-1d))

;;;
;;; 3.8.3 Compressed Texture Images
;;;

(re-export (%glCompressedTexImage1D . gl-compressed-texture-image-1d)
           (%glCompressedTexImage2D . gl-compressed-texture-image-2d)
           (%glCompressedTexImage3D . gl-compressed-texture-image-3d)
           (%glCompressedTexSubImage1D . gl-compressed-texture-sub-image-1d)
           (%glCompressedTexSubImage2D . gl-compressed-texture-sub-image-2d)
           (%glCompressedTexSubImage3D . gl-compressed-texture-sub-image-3d))

;;;
;;; 3.8.4 Texture Parameters
;;;

(re-export  (%glTexParameteri . gl-texture-parameter)
            (%glBindTexture . gl-bind-texture))

;;;
;;; Instancing extension
;;;

(define-gl-procedure (glDrawArraysInstanced (mode GLenum)
                                            (first GLint)
                                            (count GLsizei)
                                            (primcount GLsizei)
                                            -> void)
  "Draw multiple instances of a set of arrays.")

(define-gl-procedure (glDrawElementsInstanced (mode GLenum)
                                              (count GLsizei)
                                              (type GLenum)
                                              (indices void-*)
                                              (primcount GLsizei)
                                              -> void)
  "Draw multiple instances of a set of elements.")

(define-gl-procedure (glVertexAttribDivisor (index GLuint)
                                            (divisor GLuint)
                                            -> void)
  "Modify the rate at which generic vertex attributes advance during
instanced rendering.")

(export (glDrawArraysInstanced . gl-draw-arrays-instanced)
        (glDrawElementsInstanced . gl-draw-elements-instanced)
        (glVertexAttribDivisor . gl-vertex-attrib-divisor))

;;;
;;; VBOs
;;;

(re-export (%glGenBuffers . gl-gen-buffers)
           (%glDeleteBuffers . gl-delete-buffers)
           (%glBufferData . gl-buffer-data)
           (%glBufferSubData . gl-buffer-sub-data)
           (%glMapBuffer . gl-map-buffer)
           (%glUnmapBuffer . gl-unmap-buffer))

;;;
;;; VAOs
;;;

(define-gl-procedure (glGenVertexArrays (n GLsizei)
                                        (arrays GLuint-*)
                                        -> void)
  "Generate N vertex arrays.")

(define-gl-procedure (glDeleteVertexArrays (n GLsizei)
                                           (arrays GLuint-*)
                                           -> void)
  "Delete vertex array objects.")

(define-gl-procedure (glBindVertexArray (array GLuint)
                                        -> void)
  "Bind vertex array object ARRAY.")

(define-gl-procedure (glEnableVertexAttribArray (index GLuint)
                                                -> void)
  "Enable or disable a generic vertex attribute array.")

(define-gl-procedure (glVertexAttribPointer (index GLuint)
                                            (size GLint)
                                            (type GLenum)
                                            (normalized GLboolean)
                                            (stride GLsizei)
                                            (pointer GLvoid-*)
                                            -> void)
  "Define an array of generic vertex attribute data.")

(define-gl-procedure (glDrawElements (mode GLenum)
                                     (count GLsizei)
                                     (type GLenum)
                                     (indices GLvoid-*)
                                     -> void)
  "Render primitives from array data.")

(export (glGenVertexArrays . gl-gen-vertex-arrays)
        (glDeleteVertexArrays . gl-delete-vertex-arrays)
        (glBindVertexArray . gl-bind-vertex-array)
        (glEnableVertexAttribArray . gl-enable-vertex-attrib-array)
        (glVertexAttribPointer . gl-vertex-attrib-pointer)
        (glDrawElements . gl-draw-elements))

(define-syntax-rule (with-gl-client-state state body ...)
  (begin
    (gl-enable-client-state state)
    body ...
    (gl-disable-client-state state)))

(export with-gl-client-state)

;;;
;;; Framebuffers
;;;

(define-gl-procedure (glGenFramebuffers (n GLsizei)
                                        (ids GLuint-*)
                                        -> void)
  "Generate framebuffer object names.")

(define-gl-procedure (glDeleteFramebuffers (n GLsizei)
                                           (framebuffers GLuint-*)
                                           -> void)
  "Delete framebuffer objects.")

(define-gl-procedure (glBindFramebuffer (target GLenum)
                                        (framebuffer GLuint)
                                        -> void)
  "Bind a framebuffer to a framebuffer target.")

(define-gl-procedure (glFramebufferTexture2D (target GLenum)
                                             (attachment GLenum)
                                             (textarget GLenum)
                                             (texture GLuint)
                                             (level GLint)
                                             -> void)
  "Attach a level of a texture object as a logical buffer to the
currently bound framebuffer object.")

(define-gl-procedure (glCheckFramebufferStatus (target GLenum)
                                               -> GLenum)
  "Return the framebuffer completeness status of a framebuffer
object.")

(define-gl-procedure (glGenRenderbuffers (n GLsizei)
                                         (ids GLuint-*)
                                         -> void)
  "Generate renderbuffer object names.")

(define-gl-procedure (glDeleteRenderbuffers (n GLsizei)
                                            (renderbuffers GLuint-*)
                                            -> void)
  "Delete renderbuffer objects.")

(define-gl-procedure (glBindRenderbuffer (target GLenum)
                                         (renderbuffer GLuint)
                                         -> void)
  "Bind a named renderbuffer object.")

(define-gl-procedure (glRenderbufferStorage (target GLenum)
                                            (internalformat GLenum)
                                            (width GLsizei)
                                            (height GLsizei)
                                            -> void)
  "Create and initialize a renderbuffer object's data store.")

(define-gl-procedure (glFramebufferRenderbuffer (target GLenum)
                                                (attachment GLenum)
                                                (renderbuffertarget GLenum)
                                                (renderbuffer GLuint)
                                                -> void)
  "Attach a renderbuffer object to a framebuffer object.")

(export (glGenFramebuffers . gl-gen-framebuffers)
        (glDeleteFramebuffers . gl-delete-framebuffers)
        (glBindFramebuffer . gl-bind-framebuffer)
        (glFramebufferTexture2D . gl-framebuffer-texture-2d)
        (glCheckFramebufferStatus . gl-check-framebuffer-status)
        (glGenRenderbuffers . gl-gen-renderbuffers)
        (glDeleteRenderbuffers . gl-delete-renderbuffers)
        (glBindRenderbuffer . gl-bind-renderbuffer)
        (glRenderbufferStorage . gl-renderbuffer-storage)
        (glFramebufferRenderbuffer . gl-framebuffer-renderbuffer))

(re-export (%glDrawBuffers . gl-draw-buffers))


;;;
;;; Shaders
;;;

(define-gl-procedure (glUniform1ui (location GLint)
                                   (v0 GLuint)
                                   -> void)
  "Specify the value of a uniform variable for the current program object")

(export (glUniform1ui . gl-uniform1ui))

(re-export (%glUseProgram . gl-use-program)
           (%glDeleteProgram .  gl-delete-program)
           (%glDetachShader .  gl-detach-shader)
           (%glLinkProgram .  gl-link-program)
           (%glBindAttribLocation .  gl-bind-attrib-location)
           (%glAttachShader .  gl-attach-shader)
           (%glGetAttribLocation .  gl-get-attrib-location)
           (%glGetUniformLocation .  gl-get-uniform-location)
           (%glCreateProgram .  gl-create-program)
           (%glGetProgramInfoLog .  gl-get-program-info-log)
           (%glGetProgramiv .  gl-get-programiv)
           (%glDeleteProgram .  gl-delete-program)
           (%glDeleteShader .  gl-delete-shader)
           (%glGetShaderiv .  gl-get-shaderiv)
           (%glGetShaderInfoLog .  gl-get-shader-info-log)
           (%glCompileShader .  gl-compile-shader)
           (%glShaderSource .  gl-shader-source)
           (%glCreateShader .  gl-create-shader)
           (%glGetActiveUniform . gl-get-active-uniform)
           (%glGetActiveAttrib . gl-get-active-attrib)
           (%glUniform1i .  gl-uniform1i)
           (%glUniform2i .  gl-uniform2i)
           (%glUniform3i .  gl-uniform3i)
           (%glUniform4i .  gl-uniform4i)
           (%glUniform1f . gl-uniform1f)
           (%glUniform2f .  gl-uniform2f)
           (%glUniform2fv .  gl-uniform2fv)
           (%glUniform3f . gl-uniform3f)
           (%glUniform3fv . gl-uniform3fv)
           (%glUniform4f .  gl-uniform4f)
           (%glUniformMatrix4fv . gl-uniform-matrix4fv)
           (%glUniform4f .  gl-uniform4f))

(re-export (%glPointSize . gl-point-size))
