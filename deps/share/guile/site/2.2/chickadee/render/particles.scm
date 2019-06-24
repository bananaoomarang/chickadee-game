;;; Chickadee Game Toolkit
;;; Copyright © 2018 David Thompson <davet@gnu.org>
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

(define-module (chickadee render particles)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:use-module (chickadee math)
  #:use-module (chickadee math matrix)
  #:use-module (chickadee math rect)
  #:use-module (chickadee math vector)
  #:use-module (chickadee render)
  #:use-module (chickadee render buffer)
  #:use-module (chickadee render color)
  #:use-module (chickadee render shader)
  #:use-module (chickadee render texture)
  #:export (make-particle-emitter
            particle-emitter?
            particle-emitter-spawn-area
            particle-emitter-rate
            particle-emitter-life
            particle-emitter-done?
            make-particles
            particles?
            particles-capacity
            particles-size
            particles-texture
            particles-blend-mode
            particles-color
            particles-spawn-area
            add-particle-emitter
            remove-particle-emitter
            update-particles
            draw-particles*
            draw-particles))

(define-record-type <particle-emitter>
  (%make-particle-emitter spawn-area rate life)
  particle-emitter?
  (spawn-area particle-emitter-spawn-area)
  (rate particle-emitter-rate)
  (life particle-emitter-life set-particle-emitter-life!))

(define* (make-particle-emitter spawn-area rate #:optional duration)
  "Return a new particle emitter that spawns RATE particles per frame
within SPAWN-AREA (a rectangle or 2D vector) for DURATION frames.  If
DURATION is not specified, the emitter will spawn particles
indefinitely."
  (%make-particle-emitter spawn-area rate duration))

(define (update-particle-emitter emitter)
  "Advance the lifecycle of EMITTER."
  (let ((life (particle-emitter-life emitter)))
    (when life
      (set-particle-emitter-life! emitter (- life 1)))))

(define (particle-emitter-done? emitter)
  "Return #t if EMITTER has finished emitting particles."
  (let ((life (particle-emitter-life emitter)))
    (and life (<= life 0))))

(define-record-type <particles>
  (%make-particles capacity size buffer shader vertex-array
                   texture animation-rows animation-columns
                   speed-range acceleration-range direction-range
                   blend-mode start-color end-color lifetime
                   sort emitters)
  particles?
  (capacity particles-capacity)
  (size particles-size set-particles-size!)
  (buffer particles-buffer)
  (shader particles-shader)
  (vertex-array particles-vertex-array)
  (texture particles-texture set-particles-texture!)
  (animation-rows particles-animation-rows)
  (animation-columns particles-animation-columns)
  (speed-range particles-speed-range set-particles-speed-range!)
  (acceleration-range particles-acceleration-range
                      set-particles-acceleration-range!)
  (direction-range particles-direction-range set-particles-direction-range!)
  (blend-mode particles-blend-mode set-particles-blend-mode!)
  (start-color particles-start-color set-particles-start-color!)
  (end-color particles-end-color set-particles-end-color!)
  (lifetime particles-lifetime set-particles-lifetime!)
  (sort particles-sort set-particles-sort!)
  (emitters particles-emitters set-particles-emitters!))

(define (add-particle-emitter particles emitter)
  "Add EMITTER to PARTICLES."
  (set-particles-emitters! particles
                           (cons emitter (particles-emitters particles))))

(define (remove-particle-emitter particles emitter)
  "Remove EMITTER from PARTICLES."
  (set-particles-emitters! particles
                           (delete emitter (particles-emitters particles))))

(define (make-particles-shader)
  (strings->shader
   "
#version 130

in vec2 position;
in vec2 tex;
in vec2 offset;
in float life;
out vec2 frag_tex;
out float t;
uniform mat4 mvp;
uniform int lifetime;
uniform int animationRows;
uniform int animationColumns;

void main(void) {
    t = life / lifetime;
    int numTiles = animationRows * animationColumns;
    int tile = int(numTiles * (1.0 - t));
    float tx = float(tile % animationColumns) / animationColumns;
    float ty = float(tile / animationColumns) / animationRows;
    float tw = 1.0 / animationColumns;
    float th = 1.0 / animationRows;
    frag_tex = vec2(tx, ty) + tex * vec2(tw, th);
    gl_Position = mvp * vec4(position.xy + offset, 0.0, 1.0);
}
"
   "
#version 130

in vec2 frag_tex;
in float t;
uniform sampler2D color_texture;
uniform vec4 startColor;
uniform vec4 endColor;

void main (void) {
    gl_FragColor = mix(endColor, startColor, t) * texture2D(color_texture, frag_tex);
}
"))

(define (make-particles-vertex-array capacity width height texture buffer)
  (let* ((indices (make-buffer-view #:type 'scalar
                                    #:component-type 'unsigned-int
                                    #:divisor 0
                                    #:buffer (make-buffer
                                              (u32vector 0 3 2 0 2 1)
                                              #:target 'index)))
         (verts (make-buffer-view #:type 'vec2
                                  #:component-type 'float
                                  #:divisor 0
                                  #:buffer (make-buffer
                                            ;; TODO: use the texture
                                            ;; size in pixels.
                                            (let ((hw (/ width 2.0))
                                                  (hh (/ height 2.0)))
                                              (f32vector (- hw) (- hh)
                                                         hw (- hh)
                                                         hw hh
                                                         (- hw) hh))
                                            #:target 'vertex)))
         (tex (make-buffer-view #:type 'vec2
                                #:component-type 'float
                                #:divisor 0
                                #:buffer (make-buffer
                                          (let ((tex (texture-gl-tex-rect
                                                      texture)))
                                            (f32vector 0 0
                                                       1 0
                                                       1 1
                                                       0 1))
                                          #:target 'vertex)))
         (pos (make-buffer-view #:name "particle position buffer"
                                #:buffer buffer
                                #:type 'vec2
                                #:component-type 'float
                                #:length capacity
                                #:divisor 1))
         (life (make-buffer-view #:name "particle life remaining buffer"
                                 #:buffer buffer
                                 #:type 'scalar
                                 #:component-type 'int
                                 #:offset 24
                                 #:length capacity
                                 #:divisor 1)))
    (make-vertex-array #:indices indices
                       #:attributes `((0 . ,verts)
                                      (1 . ,tex)
                                      (2 . ,pos)
                                      (3 . ,life)))))

(define* (make-particles capacity #:key
                         (blend-mode 'alpha)
                         (start-color white)
                         (end-color (make-color 0.0 0.0 0.0 0.0))
                         (texture null-texture)
                         (animation-rows 1)
                         (animation-columns 1)
                         (width (if (texture-null? texture)
                                    8.0
                                    (inexact->exact
                                     (floor
                                      (/ (texture-width texture)
                                         animation-columns)))))
                         (height (if (texture-null? texture)
                                     8.0
                                     (inexact->exact
                                      (floor
                                       (/ (texture-height texture)
                                          animation-rows)))))
                         (speed-range (vec2 0.1 1.0))
                         (acceleration-range (vec2 0.0 0.1))
                         (direction-range (vec2 0.0 (* 2 pi)))
                         (lifetime 30)
                         sort)
  "Return a new particle system that may contain up to CAPACITY
particles.  Achieving the desired particle effect involves tweaking
the following keyword arguments as needed:

- BLEND-MODE: Pixel blending mode.  'alpha' by default.

- START-COLOR: The tint color of the particle at the beginning of its
life.  White by default.

- END-COLOR: The tint color of the particle at the end of of its life.
Completely transparent by default for a fade-out effect.  The color in
the middle of a particle's life will be an interpolation of
START-COLOR and END-COLOR.

- TEXTURE: The texture applied to the particles.  The texture may be
subdivided into many animation frames.

- ANIMATION-ROWS: How many animation frame rows there are in the
texture.  Default is 1.

- ANIMATION-COLUMNS: How many animation frame columns there are in the
texture.  Default is 1.

- WIDTH: The width of each particle.  By default, the width of an
animation frame (in pixels) is used.

- HEIGHT: The height of each particle.  By default, the height of an
animation frame (in pixels) is used.

- SPEED-RANGE: A 2D vector containing the min and max particle speed.
Each particle will have a speed chosen at random from this range.  By
default, speed ranges from 0.1 to 1.0.

- ACCELERATION-RANGE: A 2D vector containing the min and max particle
acceleration.  Each particle will have an acceleration chosen at
random from this range.  By default, acceleration ranges from 0.0 to
0.1.

- DIRECTION-RANGE: A 2D vector containing the min and max particle
direction as an angle in radians.  Each particle will have a direction
chosen at random from this range.  By default, the range covers all
possible angles.

- LIFETIME: How long each particle lives, measured in updates. 30 by
default.

- SORT: 'youngest' if youngest particle should be drawn last or
'oldest' for the reverse.  By default, no sorting is applied at all."
  (let* ((stride (+ (* 4 2) ; position - 2x f32
                    (* 4 2) ; velocity - 2x f32
                    (* 4 2) ; acceleration - 2x f32
                    4)) ; life remaining - 1x s32
                 (buffer (make-buffer #f
                                      #:name "packed particle data"
                                      ;; One extra element to use as
                                      ;; swap space for sorting
                                      ;; particles.
                                      #:length (* stride (+ capacity 1))
                                      #:stride stride
                                      #:usage 'stream)))
    (%make-particles capacity
                     0
                     buffer
                     (make-particles-shader)
                     (make-particles-vertex-array capacity
                                                  width
                                                  height
                                                  texture
                                                  buffer)
                     texture
                     animation-rows
                     animation-columns
                     speed-range
                     acceleration-range
                     direction-range
                     blend-mode
                     start-color
                     end-color
                     lifetime
                     sort
                     '())))

(define (update-particles particles)
  "Advance the simulation of PARTICLES."
  (let* ((buffer (particles-buffer particles))
         (va (particles-vertex-array particles))
         (pos (assq-ref (vertex-array-attributes va) 2))
         (speed-range (particles-speed-range particles))
         (acceleration-range (particles-acceleration-range particles))
         (direction-range (particles-direction-range particles))
         (sort (particles-sort particles))
         (lifetime (particles-lifetime particles))
         (float-ref bytevector-ieee-single-native-ref)
         (float-set! bytevector-ieee-single-native-set!)
         (int-ref bytevector-s32-native-ref)
         (int-set! bytevector-s32-native-set!)
         (y-offset 4)
         (dx-offset 8)
         (dy-offset 12)
         (ddx-offset 16)
         (ddy-offset 20)
         (life-offset 24))
    (with-mapped-buffer buffer
      (let* ((bv (buffer-data buffer))
             (stride (buffer-stride buffer))
             (current-size (particles-size particles)))
        ;; Remove particles in batches since often a bunch of
        ;; contiguous particles die at the same time.
        (define (kill-range start end len)
          (when start
            (bytevector-copy! bv len
                              bv start
                              (- end start))))
        ;; Update existing particles, removing dead ones.
        (let loop ((i 0)
                   (len (* current-size stride))
                   (kill-start #f))
          (if (< i len)
              (let ((life (- (int-ref bv (+ i life-offset)) 1)))
                (cond
                 ((<= life 0)
                  (loop (+ i stride) (- len stride) (or kill-start i)))
                 (kill-start
                  (kill-range kill-start i len)
                  (loop kill-start len #f))
                 (else
                  (let ((x (float-ref bv i))
                        (y (float-ref bv (+ i y-offset)))
                        (dx (float-ref bv (+ i dx-offset)))
                        (dy (float-ref bv (+ i dy-offset)))
                        (ddx (float-ref bv (+ i ddx-offset)))
                        (ddy (float-ref bv (+ i ddy-offset))))
                    (int-set! bv (+ i life-offset) life)
                    (float-set! bv i (+ x dx))
                    (float-set! bv (+ i y-offset) (+ y dy))
                    (float-set! bv (+ i dx-offset) (+ dx ddx))
                    (float-set! bv (+ i dy-offset) (+ dy ddy))
                    (loop (+ i stride) len #f)))))
              (if kill-start
                  (begin
                    (kill-range kill-start len len)
                    (loop kill-start len #f))
                  (set-particles-size! particles (/ len stride)))))
        ;; Add particles from each active emitter and then remove
        ;; emitters that have completed.
        (let ((sx (vec2-x speed-range))
              (sy (vec2-y speed-range))
              (ax (vec2-x acceleration-range))
              (ay (vec2-y acceleration-range))
              (dx (vec2-x direction-range))
              (dy (vec2-y direction-range))
              (emitters (particles-emitters particles))
              (len (- (bytevector-length bv) stride)))
          (define (emit emitter any-done?)
            (let* ((size (particles-size particles))
                   (spawn-area (particle-emitter-spawn-area emitter))
                   (rate (particle-emitter-rate emitter))
                   (rx (rect-x spawn-area))
                   (ry (rect-y spawn-area))
                   (rw (rect-width spawn-area))
                   (rh (rect-height spawn-area))
                   (start (* size stride))
                   (end (min (+ start (* rate stride)) len)))
              (let loop ((i start))
                (if (< i end)
                    (let* ((speed (+ (* (random:uniform) (- sy sx)) sx))
                           (accel (+ (* (random:uniform) (- ay ax)) ax))
                           (dir (+ (* (random:uniform) (- dy dx)) dx))
                           (dir-x (cos dir))
                           (dir-y (sin dir)))
                      (float-set! bv i (+ rx (* (random:uniform) rw)))
                      (float-set! bv (+ i y-offset)
                                  (+ ry (* (random:uniform) rh)))
                      (float-set! bv (+ i dx-offset) (* dir-x speed))
                      (float-set! bv (+ i dy-offset) (* dir-y speed))
                      (float-set! bv (+ i ddx-offset) (* dir-x accel))
                      (float-set! bv (+ i ddy-offset) (* dir-y accel))
                      (int-set! bv (+ i life-offset) lifetime)
                      (loop (+ i stride)))
                    (begin
                      (set-particles-size! particles (/ end stride))
                      (update-particle-emitter emitter)
                      (or any-done? (particle-emitter-done? emitter)))))))
          (when (fold emit #f emitters)
            (set-particles-emitters! particles
                                     (remove particle-emitter-done? emitters))))
        ;; Sort particles.
        (when sort
          (let ((compare (cond
                          ((eq? sort 'young)
                           (lambda (i j)
                             (< (int-ref bv (+ i life-offset))
                                (int-ref bv (+ j life-offset)))))
                          ((eq? sort 'old)
                           (lambda (i j)
                             (> (int-ref bv (+ i life-offset))
                                (int-ref bv (+ j life-offset)))))
                          (else
                           (error "unknown particle sorting method" sort))))
                (tmp (* (particles-capacity particles) stride)))
            (define (swap i j)
              (bytevector-copy! bv i bv tmp stride)
              (bytevector-copy! bv j bv i stride)
              (bytevector-copy! bv tmp bv j stride))
            ;; In the benchmarks I've done, insertion sort has
            ;; performed much better than quicksort here.  The number
            ;; of comparisons and swaps is much fewer.
            (define (sort start end)
              (let outer ((i (+ start stride)))
                (when (< i end)
                  (let inner ((j i))
                    (when (and (> j start)
                               (compare j (- j stride)))
                      (swap (- j stride) j)
                      (inner (- j stride))))
                  (outer (+ i stride)))))
            (sort 0 (* (particles-size particles) stride))))))))

(define draw-particles*
  (let ((mvp (make-null-matrix4)))
    (lambda (particles matrix)
      "Render PARTICLES with MATRIX applied."
      (let ((size (particles-size particles))
            (va (particles-vertex-array particles)))
        (with-blend-mode (particles-blend-mode particles)
          (with-texture 0 (particles-texture particles)
            (gpu-apply/instanced (particles-shader particles)
                                 va
                                 size
                                 #:mvp (if matrix
                                           (begin
                                             (matrix4-mult! mvp matrix
                                                            (current-projection))
                                             mvp)
                                           (current-projection))
                                 #:startColor (particles-start-color particles)
                                 #:endColor (particles-end-color particles)
                                 #:lifetime (particles-lifetime particles)
                                 #:animationRows
                                 (particles-animation-rows particles)
                                 #:animationColumns
                                 (particles-animation-columns particles))))))))

(define (draw-particles particles)
  "Render PARTICLES."
  (draw-particles* particles #f))
