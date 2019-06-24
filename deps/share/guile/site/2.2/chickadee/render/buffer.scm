;;; Chickadee Game Toolkit
;;; Copyright © 2016, 2017, 2019 David Thompson <davet@gnu.org>
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
;; GPU data buffers.
;;
;;; Code:

(define-module (chickadee render buffer)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (gl)
  #:use-module (system foreign)
  #:use-module (chickadee math matrix)
  #:use-module (chickadee math vector)
  #:use-module (chickadee render gl)
  #:use-module (chickadee render gpu)
  #:export (make-buffer
            make-streaming-buffer
            buffer?
            index-buffer?
            buffer-mapped?
            buffer-name
            buffer-length
            buffer-stride
            buffer-target
            buffer-usage
            buffer-data
            null-buffer
            map-buffer!
            unmap-buffer!
            with-mapped-buffer
            *buffer-state*
            make-buffer-view
            make-streaming-buffer-view
            buffer-view?
            buffer-view->buffer
            buffer-view->vector
            buffer-view-name
            buffer-view-offset
            buffer-view-component-type
            buffer-view-normalized?
            buffer-view-count
            buffer-view-type
            buffer-view-max
            buffer-view-min
            buffer-view-sparse
            buffer-view-data
            buffer-view-divisor
            map-buffer-view!
            unmap-buffer-view!
            with-mapped-buffer-view
            make-vertex-array
            vertex-array?
            vertex-array-indices
            vertex-array-attributes
            vertex-array-mode
            null-vertex-array
            *vertex-array-state*
            render-vertices
            render-vertices/instanced))

;;;
;;; Buffers
;;;

(define-record-type <buffer>
  (%make-buffer id name length stride target usage data)
  buffer?
  (id buffer-id)
  (name buffer-name)
  (length buffer-length)
  (stride buffer-stride)
  (target buffer-target)
  (usage buffer-usage)
  (data buffer-data set-buffer-data!))

(set-record-type-printer! <buffer>
                          (lambda (buffer port)
                            (format port
                                    "#<buffer id: ~d name: ~s usage: ~s target: ~s length: ~d stride: ~s>"
                                    (buffer-id buffer)
                                    (buffer-name buffer)
                                    (buffer-usage buffer)
                                    (buffer-target buffer)
                                    (buffer-length buffer)
                                    (buffer-stride buffer))))

(define null-buffer
  (%make-buffer 0 "null" 0 0 'vertex 'static #f))

(define <<buffer>> (class-of null-buffer))

(define (free-buffer buffer)
  (gl-delete-buffers 1 (u32vector (buffer-id buffer))))

(define-method (gpu-finalize (buffer <<buffer>>))
  (free-buffer buffer))

(define (apply-buffer buffer)
  (gl-bind-buffer (buffer-target-gl buffer)
                  (buffer-id buffer)))

(define *buffer-state*
  (make-gpu-state apply-buffer null-buffer))

(define (generate-buffer-gl)
  (let ((bv (u32vector 1)))
    (gl-gen-buffers 1 (bytevector->pointer bv))
    (u32vector-ref bv 0)))

(define (index-buffer? buffer)
  "Return #t if VIEW is an index buffer view."
  (eq? (buffer-target buffer) 'index))

(define (buffer-usage-gl buffer)
  (match (buffer-usage buffer)
    ('static (arb-vertex-buffer-object static-draw-arb))
    ('stream (arb-vertex-buffer-object stream-draw-arb))))

(define (buffer-target-gl buffer)
  (if (index-buffer? buffer)
      (arb-vertex-buffer-object element-array-buffer-arb)
      (arb-vertex-buffer-object array-buffer-arb)))

(define* (make-buffer data #:key
                      (name "anonymous")
                      (length (bytevector-length data))
                      (offset 0)
                      (stride 0)
                      (target 'vertex)
                      (usage 'static))
  "Upload DATA, a bytevector, to the GPU.  By default, the entire
bytevector is uploaded.  A subset of the data may be uploaded by
specifying the OFFSET, the index of the first byte to be uploaded, and
LENGTH, the number of bytes to upload.

If DATA is #f, allocate LENGTH bytes of fresh GPU memory instead.

TARGET and USAGE are hints that tell the GPU how the buffer is
intended to be used.

TARGET may be:
- vertex: Vertex attribute data.
- index: Index buffer data.

USAGE may be:
- static: The buffer data will not be modified after creation.
- stream: The buffer data will be modified frequently.

NAME is simply an arbitrary string for debugging purposes that is
never sent to the GPU."
  ;; Weird bugs will occur when creating a new vertex buffer while a
  ;; vertex array is bound.
  (gpu-state-set! *vertex-array-state* null-vertex-array)
  (let ((buffer (gpu-guard
                 (%make-buffer (generate-buffer-gl)
                               name
                               length
                               stride
                               target
                               usage
                               #f))))
    (gpu-state-set! *buffer-state* buffer)
    (gl-buffer-data (buffer-target-gl buffer)
                    length
                    (if data
                        (bytevector->pointer data offset)
                        %null-pointer)
                    (buffer-usage-gl buffer))
    (gpu-state-set! *buffer-state* null-buffer)
    buffer))

(define* (make-streaming-buffer length #:key
                                (name "anonymous")
                                (target 'vertex))
  "Return a new vertex buffer of LENGTH bytes, named NAME, suitable
for streaming data to the GPU every frame."
  (make-buffer #f #:usage 'stream #:length length #:name name #:target target))

(define (buffer-mapped? buffer)
  "Return #t if buffer data has been mapped from GPU."
  (if (buffer-data buffer) #t #f))

(define* (map-buffer! buffer #:optional (mode 'read-write))
  "Map the memory space for BUFFER from the GPU to the CPU, allowing
the vertex buffer to be updated with new vertex data.  The
'unmap-buffer!' procedure must be called to submit the new
vertex buffer data back to the GPU."
  (unless (buffer-mapped? buffer) ;; Don't map a buffer that is already mapped!
    (let ((target (buffer-target-gl buffer))
          (length (buffer-length buffer)))
      (gpu-state-set! *buffer-state* buffer)
      (when (eq? (buffer-usage buffer) 'stream)
        ;; Orphan the buffer to avoid implicit synchronization.
        ;; See: https://www.opengl.org/wiki/Buffer_Object_Streaming#Buffer_re-specification
        (gl-buffer-data target length %null-pointer (buffer-usage-gl buffer)))
      (let ((ptr (gl-map-buffer target (match mode
                                         ('read-write (version-1-5 read-write))
                                         ('read-only (version-1-5 read-only))
                                         ('write-only (version-1-5 write-only))))))
        (set-buffer-data! buffer (pointer->bytevector ptr length))))))

(define (unmap-buffer! buffer)
  "Return the mapped vertex buffer data for BUFFER to the GPU."
  (gpu-state-set! *buffer-state* buffer)
  (gl-unmap-buffer (buffer-target-gl buffer))
  (set-buffer-data! buffer #f))

(define-syntax-rule (with-mapped-buffer buffer body ...)
  (dynamic-wind
    (lambda ()
      (map-buffer! buffer))
    (lambda () body ...)
    (lambda ()
      (unmap-buffer! buffer))))


;;;
;;; Buffer Views
;;;

(define-record-type <buffer-view>
  (%make-buffer-view name buffer offset component-type
                     normalized? length type max min sparse divisor)
  buffer-view?
  (name buffer-view-name)
  (buffer buffer-view->buffer)
  (offset buffer-view-offset)
  (component-type buffer-view-component-type)
  (normalized? buffer-view-normalized?)
  (length buffer-view-length)
  (type buffer-view-type)
  (max buffer-view-max)
  (min buffer-view-min)
  (sparse buffer-view-sparse)
  (divisor buffer-view-divisor)) ; for instanced rendering

(define (buffer-view-stride buffer-view)
  (or (buffer-stride (buffer-view->buffer buffer-view))
      (* (type-size (buffer-view-type buffer-view))
         (component-type-size (buffer-view-component-type buffer-view)))))

(define (num-elements byte-length byte-offset type component-type)
  (inexact->exact
   (floor
    (/ (- byte-length byte-offset)
       (* (component-type-size component-type)
          (type-size type))))))


(define* (make-buffer-view #:key
                           (name "anonymous")
                           buffer
                           type
                           component-type
                           normalized?
                           (offset 0)
                           (length (num-elements (buffer-length buffer)
                                                 offset
                                                 type
                                                 component-type))
                           max
                           min
                           sparse
                           divisor)
  "Return a new typed buffer view for BUFFER starting at byte index
OFFSET of LENGTH elements, where each element is of TYPE and composed
of COMPONENT-TYPE values.

Valid values for TYPE are:
- scalar: single number
- vec2: 2D vector
- vec3: 3D vector
- vec4: 4D vector
- mat2: 2x2 matrix
- mat3: 3x3 matrix
- mat4: 4x4 matrix

Valid values for COMPONENT-TYPE are:

- byte
- unsigned-byte
- short
- unsigned-short
- int
- unsigned-int
- float
- double

DIVISOR is only needed for instanced rendering applications and
represents how many instances each vertex element applies to.  A
divisor of 0 means that a single element is used for every instance
and is used for the data being instanced.  A divisor of 1 means that
each element is used for 1 instance.  A divisor of 2 means that each
element is used for 2 instances, and so on."
  (%make-buffer-view name buffer offset component-type
                     normalized? length type max min sparse divisor))

(define (type-size type)
  (match type
    ('scalar 1)
    ('vec2 2)
    ('vec3 3)
    ((or 'vec4 'mat2) 4)
    ('mat3 9)
    ('mat4 16)))

(define (component-type-size component-type)
  (match component-type
    ('byte 1)
    ('unsigned-byte 1)
    ('short 2)
    ('unsigned-short 2)
    ('int 4)
    ('unsigned-int 4)
    ('float 4)
    ('double 8)))

(define* (make-streaming-buffer-view type component-type length #:key
                                     (name "anonymous")
                                     (target 'vertex)
                                     data
                                     divisor)
  "Return a new typed buffer to hold LENGTH elements of TYPE whose
components are comprised of COMPONENT-TYPE values.  The underlying
untyped buffer is configured for GPU streaming.  Optonally, a NAME can
be specified for the buffer.  If the buffer will be used for instanced
rendering, the DIVISOR argument must be used to specify the rate at
which attributes advance when rendering multiple instances."
  (let* ((buffer-length
          (* length (type-size type) (component-type-size component-type)))
         (buffer (if data
                     (make-buffer data
                                  #:name name
                                  #:length buffer-length
                                  #:usage 'stream
                                  #:target target)
                     (make-streaming-buffer buffer-length
                                            #:name name
                                            #:target target))))
    (make-buffer-view #:name name
                      #:buffer buffer
                      #:type type
                      #:component-type component-type
                      #:length length
                      #:divisor divisor)))

(define (display-buffer-view buffer-view port)
  (format port "#<buffer-view name: ~s buffer: ~a type: ~s component-type: ~s length: ~d offset: ~d>"
          (buffer-view-name buffer-view)
          (buffer-view->buffer buffer-view)
          (buffer-view-type buffer-view)
          (buffer-view-component-type buffer-view)
          (buffer-view-length buffer-view)
          (buffer-view-offset buffer-view)))

(set-record-type-printer! <buffer-view> display-buffer-view)

(define (buffer-view-type-size buffer-view)
  (type-size (buffer-view-type buffer-view)))

(define (buffer-view-data buffer-view)
  (buffer-data (buffer-view->buffer buffer-view)))

(define (buffer-view-type-gl buffer-view)
  (match (buffer-view-component-type buffer-view)
    ('byte (data-type byte))
    ('unsigned-byte (data-type unsigned-byte))
    ('short (data-type short))
    ('unsigned-short (data-type unsigned-short))
    ('int (data-type int))
    ('unsigned-int (data-type unsigned-int))
    ('float (data-type float))
    ('double (data-type double))))

(define (map-buffer-view! buffer-view)
  (map-buffer! (buffer-view->buffer buffer-view)))

(define (unmap-buffer-view! buffer-view)
  (unmap-buffer! (buffer-view->buffer buffer-view)))

(define-syntax-rule (with-mapped-buffer-view buffer-view body ...)
  (with-mapped-buffer (buffer-view->buffer buffer-view) body ...))

(define* (apply-buffer-view buffer-view #:optional attribute-index)
  (gpu-state-set! *buffer-state* (buffer-view->buffer buffer-view))
  ;; If there is no attribute-index, we assume this is being bound for
  ;; use as an index buffer.
  (when attribute-index
    (gl-enable-vertex-attrib-array attribute-index)
    (gl-vertex-attrib-pointer attribute-index
                              (buffer-view-type-size buffer-view)
                              (buffer-view-type-gl buffer-view)
                              (buffer-view-normalized? buffer-view)
                              (buffer-view-stride buffer-view)
                              (make-pointer (buffer-view-offset buffer-view)))
    (let ((divisor (buffer-view-divisor buffer-view)))
      (when divisor
        (gl-vertex-attrib-divisor attribute-index divisor)))))

;; TODO: Handle 4-byte alignment rule for the types that need it.
(define (buffer-view->vector buffer-view)
  (define (component-parser type)
    (match type
      ('byte bytevector-s8-ref)
      ('unsigned-byte bytevector-u8-ref)
      ('short
       (lambda (bv i)
         (bytevector-s16-ref bv i (native-endianness))))
      ('unsigned-short
       (lambda (bv i)
         (bytevector-u16-ref bv i (native-endianness))))
      ('unsigned-int
       (lambda (bv i)
         (bytevector-u32-ref bv i (native-endianness))))
      ('float bytevector-ieee-single-native-ref)))
  (define (element-parser type component-type)
    (let ((parse-component (component-parser component-type))
          (component-type-size (component-type-size component-type)))
      (match type
        ('scalar parse-component)
        ('vec2
         (lambda (bv i)
           (vec2 (parse-component bv i)
                 (parse-component bv (+ i component-type-size)))))
        ('vec3
         (lambda (bv i)
           (vec3 (parse-component bv i)
                 (parse-component bv (+ i component-type-size))
                 (parse-component bv (+ i (* component-type-size 2))))))
        ;; TODO: Use a proper vec4 type when it exists.
        ('vec4
         (lambda (bv i)
           (vector (parse-component bv i)
                   (parse-component bv (+ i component-type-size))
                   (parse-component bv (+ i (* component-type-size 2)))
                   (parse-component bv (+ i (* component-type-size 3))))))
        ;; TODO: Use proper matrix2 type when it exists.
        ('mat2
         (lambda (bv i)
           (vector (vector (parse-component bv i)
                           (parse-component bv (+ i component-type-size)))
                   (vector (parse-component bv (+ i (* component-type-size 2)))
                           (parse-component bv (+ i (* component-type-size 3)))))))
        ;; TODO: Use proper matrix3 type when it exists.
        ('mat3
         (lambda (bv i)
           (vector (vector (parse-component bv i)
                           (parse-component bv (+ i component-type-size))
                           (parse-component bv (+ i (* component-type-size 2))))
                   (vector (parse-component bv (+ i (* component-type-size 3)))
                           (parse-component bv (+ i (* component-type-size 4)))
                           (parse-component bv (+ i (* component-type-size 5)))))))
        ('mat4
         (lambda (bv i)
           (make-matrix4 (parse-component bv i)
                         (parse-component bv (+ i component-type-size))
                         (parse-component bv (+ i (* component-type-size 2)))
                         (parse-component bv (+ i (* component-type-size 3)))
                         (parse-component bv (+ i (* component-type-size 4)))
                         (parse-component bv (+ i (* component-type-size 5)))
                         (parse-component bv (+ i (* component-type-size 6)))
                         (parse-component bv (+ i (* component-type-size 7)))
                         (parse-component bv (+ i (* component-type-size 8)))
                         (parse-component bv (+ i (* component-type-size 9)))
                         (parse-component bv (+ i (* component-type-size 10)))
                         (parse-component bv (+ i (* component-type-size 11)))
                         (parse-component bv (+ i (* component-type-size 12)))
                         (parse-component bv (+ i (* component-type-size 13)))
                         (parse-component bv (+ i (* component-type-size 14)))
                         (parse-component bv (+ i (* component-type-size 15)))))))))
  (with-mapped-buffer-view buffer-view
    (let* ((data (buffer-view-data buffer-view))
           (length (buffer-view-length buffer-view))
           (offset (buffer-view-offset buffer-view))
           (stride (buffer-view-stride buffer-view))
           (type (buffer-view-type buffer-view))
           (component-type (buffer-view-component-type buffer-view))
           (type-byte-size (* (type-size type)
                              (component-type-size component-type)))
           (v (make-vector length))
           (parse-element (element-parser type component-type)))
      (let loop ((i 0))
        (when (< i length)
          (let ((byte-index (+ (* i stride) offset)))
            (vector-set! v i (parse-element data byte-index)))
          (loop (+ i 1))))
      v)))


;;;
;;; Vertex Arrays
;;;

(define-record-type <vertex-array>
  (%make-vertex-array id indices attributes mode)
  vertex-array?
  (id vertex-array-id)
  (indices vertex-array-indices)
  (attributes vertex-array-attributes)
  (mode vertex-array-mode))

(set-record-type-printer! <vertex-array>
                          (lambda (array port)
                            (format port
                                    "#<vertex-array indices: ~a attributes: ~a mode: ~s>"
                                    (vertex-array-indices array)
                                    (vertex-array-attributes array)
                                    (vertex-array-mode array))))

(define null-vertex-array (%make-vertex-array 0 #f '() 'triangles))

(define <<vertex-array>> (class-of null-vertex-array))

(define (generate-vertex-array)
  (let ((bv (u32vector 1)))
    (gl-gen-vertex-arrays 1 (bytevector->pointer bv))
    (u32vector-ref bv 0)))

(define (free-vertex-array va)
  (gl-delete-vertex-arrays 1 (u32vector (vertex-array-id va))))

(define-method (gpu-finalize (va <<vertex-array>>))
  (free-vertex-array va))

(define (apply-vertex-array va)
  (gl-bind-vertex-array (vertex-array-id va)))

(define *vertex-array-state*
  (make-gpu-state apply-vertex-array null-vertex-array))

(define* (make-vertex-array #:key indices attributes (mode 'triangles))
  "Return a new vertex array using the index data within the typed
buffer INDICES and the vertex attribute data within ATTRIBUTES, an
alist mapping shader attribute indices to typed buffers containing
vertex data.

By default, the vertex array is interpreted as containing a series of
triangles.  If another primtive type is desired, the MODE keyword
argument may be overridden.  The following values are supported:

- points
- lines
- line-loop
- line-strip
- triangles
- triangle-strip
- triangle-fan"
  (let ((array (gpu-guard
                (%make-vertex-array (generate-vertex-array)
                                    indices
                                    attributes
                                    mode))))
    (gpu-state-set! *vertex-array-state* array)
    (for-each (match-lambda
                ((index . buffer-view)
                 (apply-buffer-view buffer-view index)))
              attributes)
    (apply-buffer-view indices)
    (gpu-state-set! *vertex-array-state* null-vertex-array)
    array))

(define (vertex-array-mode-gl array)
  (match (vertex-array-mode array)
    ('points (begin-mode points))
    ('lines (begin-mode lines))
    ('line-loop (begin-mode line-loop))
    ('line-strip (begin-mode line-strip))
    ('triangles (begin-mode triangles))
    ('triangle-strip (begin-mode triangle-strip))
    ('triangle-fan (begin-mode triangle-fan))))

(define* (render-vertices array #:optional count)
  (gpu-state-set! *vertex-array-state* array)
  (let ((indices (vertex-array-indices array)))
    (gl-draw-elements (vertex-array-mode-gl array)
                      (or count
                          (buffer-view-length indices))
                      (buffer-view-type-gl indices)
                      %null-pointer)))

(define* (render-vertices/instanced array instances #:optional count)
  (gpu-state-set! *vertex-array-state* array)
  (let ((indices (vertex-array-indices array)))
    (gl-draw-elements-instanced (vertex-array-mode-gl array)
                                (or count
                                    (buffer-view-length indices))
                                (buffer-view-type-gl indices)
                                %null-pointer
                                instances)))
