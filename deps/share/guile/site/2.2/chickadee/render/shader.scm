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

(define-module (chickadee render shader)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (gl)
  #:use-module (chickadee math matrix)
  #:use-module (chickadee math vector)
  #:use-module (chickadee render color)
  #:use-module (chickadee render gl)
  #:use-module (chickadee render gpu)
  #:use-module (chickadee render texture)
  #:export (make-shader
            shader?
            null-shader
            load-shader
            strings->shader
            shader-uniform
            shader-uniforms
            shader-attributes
            uniform?
            uniform-name
            uniform-type
            uniform-value
            uniform-default-value
            set-uniform-value!
            attribute?
            attribute-name
            attribute-location
            attribute-type
            *shader-state*))

(define-record-type <shader>
  (%make-shader id attributes uniforms)
  shader?
  (id shader-id)
  (attributes shader-attributes)
  (uniforms shader-uniforms))

(define-record-type <uniform>
  (make-uniform name location type value setter)
  uniform?
  (name uniform-name)
  (location uniform-location)
  (type uniform-type)
  (value uniform-value %set-uniform-value!)
  (setter uniform-setter))

(define-record-type <attribute>
  (make-attribute name location type)
  attribute?
  (name attribute-name)
  (location attribute-location)
  (type attribute-type))

(define null-shader (%make-shader 0 (make-hash-table) (make-hash-table)))

(define <<shader>> (class-of null-shader))

(define-method (gpu-finalize (shader <<shader>>))
  (gl-delete-program (shader-id shader)))

(define (apply-shader shader)
  (gl-use-program (shader-id shader)))

(define *shader-state* (make-gpu-state apply-shader null-shader))

(define (shader-compiled? id)
  (let ((status (make-u32vector 1)))
    (gl-get-shaderiv id (version-2-0 compile-status)
                     (bytevector->pointer status))
    (= (u32vector-ref status 0) 1)))

(define (shader-linked? id)
  (let ((status (make-u32vector 1)))
    (gl-get-programiv id (version-2-0 link-status)
                      (bytevector->pointer status))
    (= (u32vector-ref status 0) 1)))

(define (info-log length-proc log-proc id)
  (let ((log-length-bv (make-u32vector 1)))
    (length-proc id (version-2-0 info-log-length)
                 (bytevector->pointer log-length-bv))
    (u32vector-ref log-length-bv 0)
    ;; Add one byte to account for the null string terminator.
    (let* ((log-length (u32vector-ref log-length-bv 0))
           (log (make-u8vector (1+ log-length))))
      (log-proc id log-length %null-pointer (bytevector->pointer log))
      (utf8->string log))))

(define (compilation-error id)
  (info-log gl-get-shaderiv gl-get-shader-info-log id))

(define (linking-error id)
  (info-log gl-get-programiv gl-get-program-info-log id))

(define (uniform-count id)
  (let ((bv (make-u32vector 1)))
    (gl-get-programiv id
                      (arb-shader-objects active-uniforms)
                      (bytevector->pointer bv))
    (u32vector-ref bv 0)))

(define (utf8->string* bv length)
  (let ((bv* (make-bytevector length)))
    (bytevector-copy! bv 0 bv* 0 length)
    (utf8->string bv*)))

(define (set-boolean-uniform! location bool)
  (gl-uniform1i location (if bool 1 0)))

(define (set-integer-uniform! location n)
  (gl-uniform1i location n))

(define (set-unsigned-integer-uniform! location n)
  (gl-uniform1ui location n))

(define (set-float-uniform! location n)
  (gl-uniform1f location n))

(define (set-float-vector2-uniform! location v)
  (gl-uniform2fv location 1 (vec2->pointer v)))

(define (set-float-vector3-uniform! location v)
  (gl-uniform3fv location 1 (vec3->pointer v)))

(define (set-float-vector4-uniform! location v)
  (if (color? v)
      (gl-uniform4f location
                    (color-r v)
                    (color-g v)
                    (color-b v)
                    (color-a v))
      #f
      ;; (gl-uniform4f location (vx v) (vy v) (vz v) (vw v))
      ))

;; (define (set-integer-vector2-uniform! location v)
;;   (gl-uniform2i location (vx v) (vy v)))

;; (define (set-integer-vector3-uniform! location v)
;;   (gl-uniform3i location (vx v) (vy v) (vz v)))

;; (define (set-integer-vector4-uniform! location v)
;;   (gl-uniform4i location (vx v) (vy v) (vz v) (vw v)))

(define (set-float-matrix4-uniform! location m)
  (gl-uniform-matrix4fv location 1 #f
                        ((@@ (chickadee math matrix) matrix4-ptr) m)))

(define (set-sampler-2d-uniform! location texture-unit)
  (gl-uniform1i location texture-unit))

(define (gl-type->symbol type)
  (cond
   ((= type (version-2-0 bool)) 'bool)
   ((= type (data-type int)) 'int)
   ((= type (data-type unsigned-int)) 'unsigned-int)
   ((= type (data-type float)) 'float)
   ((= type (version-2-0 float-vec2)) 'float-vec2)
   ((= type (version-2-0 float-vec3)) 'float-vec3)
   ((= type (version-2-0 float-vec4)) 'float-vec4)
   ;; ((= type (version-2-0 int-vec2)) 'int-vec2)
   ;; ((= type (version-2-0 int-vec3)) 'int-vec3)
   ;; ((= type (version-2-0 int-vec4)) 'int-vec4)
   ((= type (version-2-0 float-mat4)) 'mat4)
   ((= type (version-2-0 sampler-2d)) 'sampler-2d)
   (else
    (error "unsupported OpenGL type" type))))

(define %default-mat4 (make-identity-matrix4))

(define (default-uniform-value type)
  (match type
    ('bool #f)
    ('int 0)
    ('unsigned-int 0)
    ('float 0.0)
    ('float-vec2 #v(0.0 0.0))
    ('float-vec3 #v(0.0 0.0 0.0))
    ('float-vec4 (make-color 0.0 0.0 0.0 0.0))
    ;; ('int-vec2 (vector2 0 0))
    ;; ('int-vec3 (vector3 0 0 0))
    ;; ('int-vec4 (vector4 0 0 0 0))
    ('mat4 %default-mat4)))

(define (uniform-setter-for-type type)
  ;; TODO: Handle more data types, notably matrices.
  (match type
    ('bool set-boolean-uniform!)
    ('int set-integer-uniform!)
    ('unsigned-int set-unsigned-integer-uniform!)
    ('float set-float-uniform!)
    ('float-vec2 set-float-vector2-uniform!)
    ('float-vec3 set-float-vector3-uniform!)
    ('float-vec4 set-float-vector4-uniform!)
    ;; ('int-vec2 set-integer-vector2-uniform!)
    ;; ('int-vec3 set-integer-vector3-uniform!)
    ;; ('int-vec4 set-integer-vector4-uniform!)
    ('mat4 set-float-matrix4-uniform!)
    ('sampler-2d set-sampler-2d-uniform!)))

(define (extract-uniforms id)
  (let ((total (uniform-count id))
        (table (make-hash-table)))
    (let loop ((i 0)
               (texture-unit 0))
      (unless (= i total)
        (let ((length-bv (make-u32vector 1))
              (size-bv (make-u32vector 1))
              (type-bv (make-u32vector 1))
              (name-bv (make-bytevector 255)))
          (gl-get-active-uniform id i
                                 (bytevector-length name-bv)
                                 (bytevector->pointer length-bv)
                                 (bytevector->pointer size-bv)
                                 (bytevector->pointer type-bv)
                                 (bytevector->pointer name-bv))
          (let* ((length (u32vector-ref length-bv 0))
                 (name (utf8->string* name-bv length))
                 (location (gl-get-uniform-location id name))
                 (size (u32vector-ref size-bv 0))
                 (type (gl-type->symbol (u32vector-ref type-bv 0)))
                 (sampler? (eq? type 'sampler-2d))
                 (default (if sampler?
                              texture-unit
                              (default-uniform-value type)))
                 (setter (uniform-setter-for-type type)))
            ;; TODO: Handle uniform arrays.
            (unless (= size 1)
              (error "unsupported uniform size" name size))

            (hash-set! table
                       name
                       (make-uniform name location type default setter))
            (loop (1+ i)
                  (if sampler?
                      (1+ texture-unit)
                      texture-unit))))))
    table))

(define (attribute-count id)
  (let ((bv (make-u32vector 1)))
    (gl-get-programiv id
                      (arb-shader-objects active-attributes)
                      (bytevector->pointer bv))
    (u32vector-ref bv 0)))

(define (extract-attributes id)
  (let ((total (attribute-count id))
        (table (make-hash-table)))
    (let loop ((i 0))
      (unless (= i total)
        (let ((length-bv (make-u32vector 1))
              (size-bv (make-u32vector 1))
              (type-bv (make-u32vector 1))
              (name-bv (make-bytevector 255)))
          (gl-get-active-attrib id i
                                (bytevector-length name-bv)
                                (bytevector->pointer length-bv)
                                (bytevector->pointer size-bv)
                                (bytevector->pointer type-bv)
                                (bytevector->pointer name-bv))
          (let* ((length (u32vector-ref length-bv 0))
                 (name (utf8->string* name-bv length))
                 (size (u32vector-ref size-bv 0))
                 (type (gl-type->symbol (u32vector-ref type-bv 0)))
                 (location (gl-get-attrib-location id name)))
            (unless (= size 1)
              (error "unsupported attribute size" name size))

            (hash-set! table name (make-attribute name location type))))
        (loop (1+ i))))
    table))

(define (make-shader vertex-port fragment-port)
  "Read GLSL source from VERTEX-PORT and FRAGMENT-PORT and compile
them into a GPU shader program."
  (define (make-shader-stage type port)
    (let ((id (gl-create-shader type))
          (source (get-bytevector-all port)))
      (gl-shader-source id 1
                        (bytevector->pointer
                         (u64vector
                          (pointer-address (bytevector->pointer source))))
                        (bytevector->pointer
                         (u32vector (bytevector-length source))))
      (gl-compile-shader id)
      (unless (shader-compiled? id)
        (let ((error-log (compilation-error id)))
          (gl-delete-shader id) ; clean up GPU resource.
          (error "failed to compile shader" error-log)))
      id))

  (let ((vertex-id (make-shader-stage (version-2-0 vertex-shader)
                                      vertex-port))
        (fragment-id (make-shader-stage (version-2-0 fragment-shader)
                                        fragment-port))
        (id (gl-create-program)))
    (gl-attach-shader id vertex-id)
    (gl-attach-shader id fragment-id)
    (gl-link-program id)
    (unless (shader-linked? id)
      (let ((error-log (linking-error id)))
        (gl-delete-program id)
        (error "failed to link shader" error-log)))
    (gl-delete-shader vertex-id)
    (gl-delete-shader fragment-id)
    (gpu-guard (%make-shader id (extract-attributes id) (extract-uniforms id)))))

(define (load-shader vertex-source-file fragment-source-file)
  "Compile the GLSL source code within VERTEX-SOURCE-FILE and
FRAGMENT-SOURCE-FILE into a GPU shader program."
  (call-with-input-file vertex-source-file
    (lambda (vertex-port)
      (call-with-input-file fragment-source-file
        (lambda (fragment-port)
          (make-shader vertex-port fragment-port))))))

(define (strings->shader vertex-source fragment-source)
  "Compile VERTEX-SOURCE, the GLSL code for the vertex shader,
and FRAGMENT-SOURCE, the GLSL code for the fragment shader, into a GPU
shader program."
  (call-with-input-string vertex-source
    (lambda (vertex-port)
      (call-with-input-string fragment-source
        (lambda (fragment-port)
          (make-shader vertex-port fragment-port))))))

(define (shader-uniform shader name)
  "Return the metadata for the uniform NAME in SHADER."
  (let ((uniform (hash-ref (shader-uniforms shader) name)))
    (or uniform (error "no such uniform" name))))

(define (set-uniform-value! uniform x)
  "Change the value of UNIFORM to X.  This procedure assumes that the
shader where UNIFORM is defined is currently bound in the OpenGL
context.  The behavior of this procedure under any other circumstance
is undefined."
  ((uniform-setter uniform) (uniform-location uniform) x)
  (%set-uniform-value! uniform x))

(define (uniform-default-value uniform)
  "Return the default value of UNIFORM."
  (default-uniform-value (uniform-type uniform)))
