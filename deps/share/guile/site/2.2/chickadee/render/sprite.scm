;;; Chickadee Game Toolkit
;;; Copyright © 2016, 2019 David Thompson <davet@gnu.org>
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

(define-module (chickadee render sprite)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (chickadee math matrix)
  #:use-module (chickadee math rect)
  #:use-module (chickadee math vector)
  #:use-module (chickadee render)
  #:use-module (chickadee render color)
  #:use-module (chickadee render shader)
  #:use-module (chickadee render texture)
  #:use-module (chickadee render buffer)
  #:export (draw-sprite*
            draw-sprite

            make-sprite-batch
            sprite-batch?
            sprite-batch-texture
            set-sprite-batch-texture!
            sprite-batch-clear!
            sprite-batch-add*
            sprite-batch-add!
            draw-sprite-batch*
            draw-sprite-batch

            with-batched-sprites
            draw-nine-patch*
            draw-nine-patch))

(define unbatched-sprite-shader
  (delay
    (strings->shader
     "
#version 130

in vec2 position;
in vec2 tex;
out vec2 fragTex;
uniform mat4 mvp;

void main(void) {
    fragTex = tex;
    gl_Position = mvp * vec4(position.xy, 0.0, 1.0);
}
"
     "
#version 130

in vec2 fragTex;
uniform sampler2D colorTexture;
uniform vec4 tint;

void main (void) {
    gl_FragColor = texture2D(colorTexture, fragTex) * tint;
}
")))

(define draw-sprite*
  (let* ((stride 16)            ; 4 f32s, 2 for vertex, 2 for texcoord
         (buffer (delay
                   (make-buffer #f
                                #:name "unbatched sprite buffer"
                                #:length (* stride 4)
                                #:stride stride
                                #:usage 'stream)))
         (pos (delay
                (make-buffer-view #:name "unbatched sprite vertices"
                                  #:buffer (force buffer)
                                  #:type 'vec2
                                  #:component-type 'float
                                  #:length 4)))
         (tex (delay
                (make-buffer-view #:name "unbatched sprite texcoords"
                                  #:buffer (force buffer)
                                  #:type 'vec2
                                  #:component-type 'float
                                  #:length 4
                                  #:offset 8)))
         (indices
          (delay
            (make-buffer-view #:name "unbatched sprite indices"
                              #:type 'scalar
                              #:component-type 'unsigned-int
                              #:buffer (make-buffer (u32vector 0 3 2 0 2 1)
                                                    #:target 'index))))
         (vertex-array
          (delay
            (make-vertex-array #:indices (force indices)
                               #:attributes
                               `((0 . ,(force pos))
                                 (1 . ,(force tex))))))
         (mvp (make-null-matrix4)))
    (lambda* (texture
              rect
              matrix
              #:key
              (tint white)
              (blend-mode 'alpha)
              (texcoords (texture-gl-tex-rect texture)))
      (with-mapped-buffer-view (force pos)
        (let* ((x1 (rect-x rect))
               (y1 (rect-y rect))
               (x2 (+ x1 (rect-width rect)))
               (y2 (+ y1 (rect-height rect)))
               (s1 (rect-x texcoords))
               (t1 (rect-y texcoords))
               (s2 (+ (rect-x texcoords) (rect-width texcoords)))
               (t2 (+ (rect-y texcoords) (rect-height texcoords)))
               (bv (buffer-view-data (force pos))))
          ;; Texture origin is at the top-left, so we need to flip the Y
          ;; coordinate relative to the vertices.
          (f32vector-set! bv 0 x1)
          (f32vector-set! bv 1 y1)
          (f32vector-set! bv 2 s1)
          (f32vector-set! bv 3 t2)
          (f32vector-set! bv 4 x2)
          (f32vector-set! bv 5 y1)
          (f32vector-set! bv 6 s2)
          (f32vector-set! bv 7 t2)
          (f32vector-set! bv 8 x2)
          (f32vector-set! bv 9 y2)
          (f32vector-set! bv 10 s2)
          (f32vector-set! bv 11 t1)
          (f32vector-set! bv 12 x1)
          (f32vector-set! bv 13 y2)
          (f32vector-set! bv 14 s1)
          (f32vector-set! bv 15 t1)))
      (with-blend-mode blend-mode
        (with-texture 0 texture
          (gpu-apply (force unbatched-sprite-shader) (force vertex-array)
                     #:tint tint
                     #:mvp (if matrix
                               (begin
                                 (matrix4-mult! mvp matrix
                                                (current-projection))
                                 mvp)
                               (current-projection))))))))

(define %null-vec2 (vec2 0.0 0.0))
(define %default-scale (vec2 1.0 1.0))

(define draw-sprite
  (let ((matrix (make-null-matrix4)))
    (lambda* (texture
              position
              #:key
              (tint white)
              (origin %null-vec2)
              (scale %default-scale)
              (rotation 0.0)
              (blend-mode 'alpha)
              (rect (texture-gl-rect texture)))
      "Draw TEXTURE at POSITION.

Optionally, other transformations may be applied to the sprite.
ROTATION specifies the angle to rotate the sprite, in radians.  SCALE
specifies the scaling factor as a 2D vector.  All transformations are
applied relative to ORIGIN, a 2D vector.

TINT specifies the color to multiply against all the sprite's pixels.
By default white is used, which does no tinting at all.

By default, alpha blending is used but can be changed by specifying
BLEND-MODE."
      (matrix4-2d-transform! matrix
                             #:origin origin
                             #:position position
                             #:rotation rotation
                             #:scale scale)
      (draw-sprite* texture rect matrix
                    #:tint tint
                    #:blend-mode blend-mode))))


;;;
;;; Sprite Batches
;;;

(define-record-type <sprite-batch>
  (%make-sprite-batch texture size capacity vertex-buffer vertex-array)
  sprite-batch?
  (texture sprite-batch-texture set-sprite-batch-texture!)
  (size sprite-batch-size set-sprite-batch-size!)
  (capacity sprite-batch-capacity set-sprite-batch-capacity!)
  (vertex-buffer sprite-batch-vertex-buffer set-sprite-batch-vertex-buffer!)
  (vertex-array sprite-batch-vertex-array set-sprite-batch-vertex-array!))

(define (init-sprite-batch batch capacity)
  (let* ((index-data (let ((bv (make-u32vector (* capacity 6))))
                       (let loop ((i 0))
                         (when (< i capacity)
                               (let ((index-offset (* i 6))
                                     (vertex-offset (* i 4)))
                                 (u32vector-set! bv index-offset vertex-offset)
                                 (u32vector-set! bv (+ index-offset 1) (+ vertex-offset 3))
                                 (u32vector-set! bv (+ index-offset 2) (+ vertex-offset 2))
                                 (u32vector-set! bv (+ index-offset 3) vertex-offset)
                                 (u32vector-set! bv (+ index-offset 4) (+ vertex-offset 2))
                                 (u32vector-set! bv (+ index-offset 5) (+ vertex-offset 1))
                                 (loop (+ i 1)))))
                       bv))
         (index-buffer (make-buffer index-data
                                    #:name "indices"
                                    #:target 'index))
         (indices (make-buffer-view #:name "indices"
                                    #:buffer index-buffer
                                    #:type 'scalar
                                    #:component-type 'unsigned-int))
         (stride 32) ; 8 f32s, 2 for vertex, 2 for texcoord, 4 for tint color
         (buffer (make-buffer #f
                              #:name "sprite batch buffer"
                              #:length (* capacity stride 4)
                              #:stride stride
                              #:usage 'stream))
         (pos (make-buffer-view #:name "sprite batch vertices"
                                #:buffer buffer
                                #:type 'vec2
                                #:component-type 'float
                                #:length (* capacity 4)))
         (tex (make-buffer-view #:name "sprite batch texture coordinates"
                                #:buffer buffer
                                #:type 'vec2
                                #:component-type 'float
                                #:length (* capacity 4)
                                #:offset 8))
         (tint (make-buffer-view #:name "sprite batch tint colors"
                                 #:buffer buffer
                                 #:type 'vec4
                                 #:component-type 'float
                                 #:length (* capacity 4)
                                 #:offset 16))
         (va (make-vertex-array #:indices indices
                                #:attributes `((0 . ,pos)
                                               (1 . ,tex)
                                               (2 . ,tint)))))
    (set-sprite-batch-capacity! batch capacity)
    (set-sprite-batch-vertex-buffer! batch buffer)
    (set-sprite-batch-vertex-array! batch va)))

(define* (make-sprite-batch texture #:key (capacity 256))
  "Make a sprite batch that can hold CAPACITY sprites."
  (let ((batch (%make-sprite-batch texture 0 0 #f #f)))
    (init-sprite-batch batch capacity)
    batch))

(define (sprite-batch-full? batch)
  (= (sprite-batch-capacity batch) (sprite-batch-size batch)))

(define (double-sprite-batch-size! batch)
  (let* ((old-verts (sprite-batch-vertex-buffer batch))
         (old-vertex-data (buffer-data old-verts)))
    (unmap-buffer! old-verts)
    (init-sprite-batch batch (* (sprite-batch-capacity batch) 2))
    (let ((new-verts (sprite-batch-vertex-buffer batch)))
      (map-buffer! new-verts 'write-only)
      (bytevector-copy! old-vertex-data 0
                        (buffer-data new-verts) 0
                        (bytevector-length old-vertex-data)))))

(define (sprite-batch-clear! batch)
  "Reset BATCH to size 0."
  (set-sprite-batch-size! batch 0))

(define (sprite-batch-flush! batch)
  "Submit the contents of BATCH to the GPU."
  (unmap-buffer! (sprite-batch-vertex-buffer batch)))

(define* (sprite-batch-add* batch rect matrix
                            #:key
                            (tint white)
                            texture-region)
  "Add RECT, transformed by MATRIX, to BATCH.  To render a subsection
of the batch's texture, a texture object whose parent is the batch
texture may be specified via the TEXTURE-REGION argument."
  ;; Expand the buffers when necessary.
  (when (sprite-batch-full? batch)
        (double-sprite-batch-size! batch))
  (map-buffer! (sprite-batch-vertex-buffer batch) 'write-only)
  (let* ((size (sprite-batch-size batch))
         (vertices (buffer-data (sprite-batch-vertex-buffer batch)))
         (offset (* size 32)) ; each sprite is 32 floats in size
         (minx (rect-x rect))
         (miny (rect-y rect))
         (maxx (+ minx (rect-width rect)))
         (maxy (+ miny (rect-height rect)))
         (x1 (transform-x matrix minx miny))
         (y1 (transform-y matrix minx miny))
         (x2 (transform-x matrix maxx miny))
         (y2 (transform-y matrix maxx miny))
         (x3 (transform-x matrix maxx maxy))
         (y3 (transform-y matrix maxx maxy))
         (x4 (transform-x matrix minx maxy))
         (y4 (transform-y matrix minx maxy))
         (texcoords (texture-gl-tex-rect
                     (or texture-region
                         (sprite-batch-texture batch))))
         (s1 (rect-x texcoords))
         (t1 (rect-y texcoords))
         (s2 (+ (rect-x texcoords) (rect-width texcoords)))
         (t2 (+ (rect-y texcoords) (rect-height texcoords))))
    ;; Add vertices.
    ;; Bottom-left
    (f32vector-set! vertices offset x1)
    (f32vector-set! vertices (+ offset 1) y1)
    ;; Bottom-right
    (f32vector-set! vertices (+ offset 8) x2)
    (f32vector-set! vertices (+ offset 9) y2)
    ;; Top-right
    (f32vector-set! vertices (+ offset 16) x3)
    (f32vector-set! vertices (+ offset 17) y3)
    ;; Top-left
    (f32vector-set! vertices (+ offset 24) x4)
    (f32vector-set! vertices (+ offset 25) y4)
    ;; Add texture coordinates.
    ;; Bottom-left
    (f32vector-set! vertices (+ offset 2) s1)
    (f32vector-set! vertices (+ offset 3) t2)
    ;; Bottom-right
    (f32vector-set! vertices (+ offset 10) s2)
    (f32vector-set! vertices (+ offset 11) t2)
    ;; Top-right
    (f32vector-set! vertices (+ offset 18) s2)
    (f32vector-set! vertices (+ offset 19) t1)
    ;; Top-left
    (f32vector-set! vertices (+ offset 26) s1)
    (f32vector-set! vertices (+ offset 27) t1)
    ;; Add tint.
    (let ((bv ((@@ (chickadee render color) unwrap-color) tint))
          (byte-offset (* offset 4)))
      (bytevector-copy! bv 0 vertices (+ byte-offset 16) 16)
      (bytevector-copy! bv 0 vertices (+ byte-offset 48) 16)
      (bytevector-copy! bv 0 vertices (+ byte-offset 80) 16)
      (bytevector-copy! bv 0 vertices (+ byte-offset 112) 16))
    (set-sprite-batch-size! batch (1+ size))))

(define sprite-batch-add!
  (let ((matrix (make-null-matrix4)))
    (lambda* (batch
              position
              #:key
              (origin %null-vec2)
              (scale %default-scale)
              (rotation 0.0)
              (tint white)
              texture-region)
      "Add sprite to BATCH at POSITION.  To render a subsection of the
batch's texture, a texture object whose parent is the batch texture
may be specified via the TEXTURE-REGION argument."
      (let ((rect (texture-gl-rect
                   (or texture-region (sprite-batch-texture batch)))))
        (matrix4-2d-transform! matrix
                               #:origin origin
                               #:position position
                               #:rotation rotation
                               #:scale scale)
        (sprite-batch-add* batch rect matrix
                           #:tint tint
                           #:texture-region texture-region)))))


(define batched-sprite-shader
  (delay
    (strings->shader
     "
#version 130

in vec2 position;
in vec2 tex;
in vec4 tint;
out vec2 fragTex;
out vec4 fragTint;
uniform mat4 mvp;

void main(void) {
    fragTex = tex;
    fragTint = tint;
    gl_Position = mvp * vec4(position.xy, 0.0, 1.0);
}
"
     "
#version 130

in vec2 fragTex;
in vec4 fragTint;
uniform sampler2D colorTexture;

void main (void) {
    gl_FragColor = texture2D(colorTexture, fragTex) * fragTint;
}
")))

(define draw-sprite-batch*
  (let ((mvp (make-null-matrix4)))
    (lambda* (batch matrix #:key (blend-mode 'alpha))
      "Render the contents of BATCH."
      (sprite-batch-flush! batch)
      (matrix4-mult! mvp matrix (current-projection))
      (with-blend-mode blend-mode
        (with-texture 0 (sprite-batch-texture batch)
          (gpu-apply* (force batched-sprite-shader)
                      (sprite-batch-vertex-array batch)
                      (* (sprite-batch-size batch) 6)
                      #:mvp mvp))))))

(define draw-sprite-batch
  (let ((matrix (make-null-matrix4)))
    (lambda* (batch
              #:key
              (position %null-vec2)
              (origin %null-vec2)
              (scale %default-scale)
              (rotation 0.0)
              (blend-mode 'alpha))
      "Render the contents of BATCH."
      (matrix4-2d-transform! matrix
                             #:origin origin
                             #:position position
                             #:rotation rotation
                             #:scale scale)
      (draw-sprite-batch* batch matrix #:blend-mode blend-mode))))


;;;
;;; Nine Patches
;;;

(define draw-nine-patch*
  (let ((%rect (make-rect 0.0 0.0 0.0 0.0))
        (texcoords (make-rect 0.0 0.0 0.0 0.0)))
    (lambda* (texture
              rect
              matrix
              #:key
              (margin 0.0)
              (top-margin margin)
              (bottom-margin margin)
              (left-margin margin)
              (right-margin margin)
              (blend-mode 'alpha)
              (tint white))
      (let* ((x (rect-x rect))
             (y (rect-y rect))
             (w (rect-width rect))
             (h (rect-height rect))
             (border-x1 x)
             (border-y1 y)
             (border-x2 (+ x w))
             (border-y2 (+ y h))
             (fill-x1 (+ border-x1 left-margin))
             (fill-y1 (+ border-y1 bottom-margin))
             (fill-x2 (- border-x2 right-margin))
             (fill-y2 (- border-y2 top-margin))
             (prect (texture-gl-rect texture))
             (trect (texture-gl-tex-rect texture))
             (tw (rect-width prect))
             (th (rect-height prect))
             (border-s1 (rect-x trect))
             (border-t1 (rect-y trect))
             (border-s2 (+ (rect-x trect) (rect-width trect)))
             (border-t2 (+ (rect-y trect) (rect-height trect)))
             (fill-s1 (+ border-s1 (/ left-margin tw)))
             (fill-t1 (+ border-t1 (/ top-margin th)))
             (fill-s2 (- border-s2 (/ right-margin tw)))
             (fill-t2 (- border-t2 (/ bottom-margin th))))
        (define (draw-piece x1 y1 x2 y2 s1 t1 s2 t2)
          (set-rect-x! %rect x1)
          (set-rect-y! %rect y1)
          (set-rect-width! %rect (- x2 x1))
          (set-rect-height! %rect (- y2 y1))
          (set-rect-x! texcoords s1)
          (set-rect-y! texcoords t1)
          (set-rect-width! texcoords (- s2 s1))
          (set-rect-height! texcoords (- t2 t1))
          (draw-sprite* texture %rect matrix
                        #:texcoords texcoords
                        #:blend-mode blend-mode
                        #:tint tint))
        ;; bottom-left
        (draw-piece border-x1 border-y1 fill-x1 fill-y1
                    border-s1 fill-t2 fill-s1 border-t2)
        ;; bottom-center
        (draw-piece fill-x1 border-y1 fill-x2 fill-y1
                    fill-s1 fill-t2 fill-s2 border-t2)
        ;; bottom-right
        (draw-piece fill-x2 border-y1 border-x2 fill-y1
                    fill-s2 fill-t2 border-s2 border-t2)
        ;; center-left
        (draw-piece border-x1 fill-y1 fill-x1 fill-y2
                    border-s1 fill-t2 fill-s1 fill-t1)
        ;; center
        (draw-piece fill-x1 fill-y1 fill-x2 fill-y2
                    fill-s1 fill-t2 fill-s2 fill-t1)
        ;; center-right
        (draw-piece fill-x2 fill-y1 border-x2 fill-y2
                    fill-s2 fill-t2 border-s2 fill-t1)
        ;; top-left
        (draw-piece border-x1 fill-y2 fill-x1 border-y2
                    border-s1 border-t1 fill-s1 fill-t1)
        ;; top-center
        (draw-piece fill-x1 fill-y2 fill-x2 border-y2
                    fill-s1 border-t1 fill-s2 fill-t1)
        ;; top-right
        (draw-piece fill-x2 fill-y2 border-x2 border-y2
                    fill-s2 border-t1 border-s2 fill-t1)))))

(define draw-nine-patch
  (let ((position (vec2 0.0 0.0))
        (%rect (make-rect 0.0 0.0 0.0 0.0))
        (matrix (make-null-matrix4)))
    (lambda* (texture
              rect
              #:key
              (margin 0.0)
              (top-margin margin) (bottom-margin margin)
              (left-margin margin) (right-margin margin)
              (origin %null-vec2)
              (rotation 0.0)
              (scale %default-scale)
              (blend-mode 'alpha)
              (tint white))
      "Draw a \"nine patch\" sprite.  A nine patch sprite renders
TEXTURE on the rectangular area RECT whose stretchable areas are
defined by the given margin measurements.  The corners are never
stretched, the left and right edges may be stretched vertically, the
top and bottom edges may be stretched horizontally, and the center may
be stretched in both directions.  This rendering technique is
particularly well suited for resizable windows and buttons in
graphical user interfaces.

MARGIN specifies the margin size for all sides of the nine patch.  To
make margins of differing sizes, the TOP-MARGIN, BOTTOM-MARGIN,
LEFT-MARGIN, and RIGHT-MARGIN arguments may be used."
      (set-rect-x! %rect 0.0)
      (set-rect-y! %rect 0.0)
      (set-rect-width! %rect (rect-width rect))
      (set-rect-height! %rect (rect-height rect))
      (set-vec2-x! position (rect-x rect))
      (set-vec2-y! position (rect-y rect))
      (matrix4-2d-transform! matrix
                             #:origin origin
                             #:position position
                             #:rotation rotation
                             #:scale scale)
      (draw-nine-patch* texture %rect matrix
                        #:top-margin top-margin
                        #:bottom-margin bottom-margin
                        #:left-margin left-margin
                        #:right-margin right-margin
                        #:blend-mode blend-mode
                        #:tint tint))))
