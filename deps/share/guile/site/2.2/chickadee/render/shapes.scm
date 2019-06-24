;;; Chickadee Game Toolkit
;;; Copyright © 2016, 2018 David Thompson <davet@gnu.org>
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

;;; Commentary
;;
;; Polylines as described in
;; http://jcgt.org/published/0002/02/08/paper.pdf
;;
;;; Code:

(define-module (chickadee render shapes)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-4)
  #:use-module (chickadee math bezier)
  #:use-module (chickadee math matrix)
  #:use-module (chickadee math rect)
  #:use-module (chickadee math vector)
  #:use-module (chickadee render)
  #:use-module (chickadee render color)
  #:use-module (chickadee render shader)
  #:use-module (chickadee render buffer)
  #:export (draw-filled-rect
            draw-line
            draw-bezier-curve
            draw-bezier-path))

;; TODO: Make a generic polygon renderer, include batching, etc.
(define draw-filled-rect
  (let* ((vertex-buffer
          (delay
            (make-streaming-buffer-view 'vec2 'float 4
                                        #:name "rect-buffer-view")))
         (index-buffer
          (delay
            (make-buffer-view #:type 'scalar
                              #:component-type 'unsigned-int
                              #:buffer (make-buffer (u32vector 0 3 2 0 2 1)
                                                    #:target 'index))))
         (vertex-array
          (delay
            (make-vertex-array #:indices (force index-buffer)
                               #:attributes `((0 . ,(force vertex-buffer))))))
         (default-shader
           (delay
             (strings->shader
              "
#version 130

in vec2 position;
uniform mat4 mvp;

void main(void) {
    gl_Position = mvp * vec4(position.xy, 0.0, 1.0);
}
"
              "
#version 130

in vec2 frag_tex;
uniform vec4 color;

void main (void) {
    gl_FragColor = color;
}
")))
         (mvp (make-null-matrix4)))
    (lambda* (region
              color
              #:key
              (blend-mode 'alpha)
              (shader (force default-shader))
              matrix)
      (let* ((x1 (rect-x region))
             (y1 (rect-y region))
             (x2 (+ x1 (rect-width region)))
             (y2 (+ y1 (rect-height region))))
        (with-mapped-buffer-view (force vertex-buffer)
          (let ((bv (buffer-view-data (force vertex-buffer))))
            (f32vector-set! bv 0 x1)
            (f32vector-set! bv 1 y1)
            (f32vector-set! bv 2 x2)
            (f32vector-set! bv 3 y1)
            (f32vector-set! bv 4 x2)
            (f32vector-set! bv 5 y2)
            (f32vector-set! bv 6 x1)
            (f32vector-set! bv 7 y2)))
        (with-blend-mode blend-mode
          (gpu-apply shader (force vertex-array)
                     #:mvp (if matrix
                               (begin
                                 (matrix4-mult! mvp matrix
                                                (current-projection))
                                 mvp)
                               (current-projection))
                     #:color color))))))

(define draw-line
  (let* ((mvp (make-null-matrix4))
         (vertex-buffer
          (delay
            (make-streaming-buffer-view 'vec2 'float 4
                                        #:name "line-buffer-view")))
         (texcoord-buffer
          (delay
            (make-streaming-buffer-view 'vec2 'float 4
                                        #:name "line-buffer-view")))
         (index-buffer
          (delay
            (make-buffer-view #:type 'scalar
                              #:component-type 'unsigned-int
                              #:buffer (make-buffer (u32vector 0 3 2 0 2 1)
                                                    #:target 'index))))
         (vertex-array
          (delay
            (make-vertex-array #:indices (force index-buffer)
                               #:attributes `((0 . ,(force vertex-buffer))
                                              (1 . ,(force texcoord-buffer))))))
         (default-shader
           (delay
             (strings->shader
              "
#version 130

in vec2 position;
in vec2 tex;
out vec2 frag_tex;
uniform mat4 mvp;

void main(void) {
    frag_tex = tex;
    gl_Position = mvp * vec4(position.xy, 0.0, 1.0);
}
"
              "
#version 130

in vec2 frag_tex;
uniform vec4 color;
uniform float r;
uniform float w;
uniform float t;
uniform float l;
uniform int cap;
float infinity = 1.0 / 0.0;

void main (void) {
  float hw = w / 2.0;
  float u = frag_tex.x;
  float v = frag_tex.y;
  float dx;
  float dy;
  float d;

  if (u < 0 || u > l) {
    if (u < 0) {
      dx = abs(u);
    } else {
      dx = u - l;
    }
    dy = abs(v);

    switch (cap) {
    // none
    case 0:
      d = infinity;
      break;
    // butt
    case 1:
      d = max(dx + w / 2 - 2 * r, dy);
      break;
    // square
    case 2:
      d = max(dx, dy);
      break;
    // round
    case 3:
      d = sqrt(dx * dx + dy * dy);
      break;
    // triangle out
    case 4:
      d = dx + dy;
      break;
    // triangle in
    case 5:
      d = max(dy, w / 2 - r + dx - dy);
      break;
    }
  } else {
    d = abs(v);
  }

  if (d <= hw) {
    gl_FragColor = color;
  } else {
    gl_FragColor = vec4(color.rgb, color.a * (1.0 - ((d - hw) / r)));
  }
}
"))))
    (lambda* (start end #:key
                    (thickness 0.5)
                    (feather 1.0)
                    (cap 'round)
                    (color white)
                    (shader (force default-shader))
                    matrix)
      "Draw a line segment from START to END. The line will be
THICKNESS pixels thick with an antialiased border FEATHER pixels wide.
The line will be colored COLOR. CAP specifies the type of end cap that
should be used to terminate the lines, either 'none', 'butt',
'square', 'round', 'triangle-in', or 'triangle-out'.  Advanced users
may use SHADER to override the built-in line segment shader."
      (let* ((x1 (vec2-x start))
             (y1 (vec2-y start))
             (x2 (vec2-x end))
             (y2 (vec2-y end))
             (dx (- x2 x1))
             (dy (- y2 y1))
             (length (sqrt (+ (expt dx 2) (expt dy 2))))
             (padding (/ (ceiling (+ thickness (* feather 2.5))) 2.0))
             (nx (/ dx length))
             (ny (/ dy length))
             (xpad (* nx padding))
             (ypad (* ny padding))
             ;; start left
             (vx1 (+ (- x1 xpad) ypad))
             (vy1 (+ (- y1 ypad) (- xpad)))
             (s1 (- padding))
             (t1 padding)
             ;; start right
             (vx2 (+ (- x1 xpad) (- ypad)))
             (vy2 (+ (- y1 ypad) xpad))
             (s2 (- padding))
             (t2 (- padding))
             ;; end left
             (vx3 (+ x2 xpad (- ypad)))
             (vy3 (+ y2 ypad xpad))
             (s3 (+ length padding))
             (t3 (- padding))
             ;; end right
             (vx4 (+ (+ x2 xpad) ypad))
             (vy4 (+ (+ y2 ypad) (- xpad)))
             (s4 (+ length padding))
             (t4 padding))
        (with-mapped-buffer-view (force vertex-buffer)
          (let ((bv (buffer-view-data (force vertex-buffer))))
            (f32vector-set! bv 0 vx1)
            (f32vector-set! bv 1 vy1)
            (f32vector-set! bv 2 vx2)
            (f32vector-set! bv 3 vy2)
            (f32vector-set! bv 4 vx3)
            (f32vector-set! bv 5 vy3)
            (f32vector-set! bv 6 vx4)
            (f32vector-set! bv 7 vy4)))
        (with-mapped-buffer-view (force texcoord-buffer)
          (let ((bv (buffer-view-data (force texcoord-buffer))))
            (f32vector-set! bv 0 s1)
            (f32vector-set! bv 1 t1)
            (f32vector-set! bv 2 s2)
            (f32vector-set! bv 3 t2)
            (f32vector-set! bv 4 s3)
            (f32vector-set! bv 5 t3)
            (f32vector-set! bv 6 s4)
            (f32vector-set! bv 7 t4)))
        (with-blend-mode 'alpha
          (gpu-apply shader (force vertex-array)
                     #:mvp (if matrix
                               (begin
                                 (matrix4-mult! mvp matrix
                                                (current-projection))
                                 mvp)
                               (current-projection))
                     #:color color
                     #:w thickness
                     #:r feather
                     #:l length
                     #:cap (match cap
                             ('none 0)
                             ('butt 1)
                             ('square 2)
                             ('round 3)
                             ('triangle-out 4)
                             ('triangle-in 5))))))))

;; XXX: This is going to be hopelessly slow until I implement batching
;; for lines and shapes.
(define draw-bezier-curve
  (let ((start #v(0.0 0.0))
        (end #v(0.0 0.0))
        (tmp #f)
        (rect (make-rect 0.0 0.0 0.0 0.0)))
    (lambda* (bezier #:key
                     (segments 32)
                     control-points?
                     tangents?
                     (control-point-size 8.0)
                     (color white)
                     (control-point-color yellow)
                     (tangent-color yellow)
                     (thickness 0.5)
                     (feather 1.0)
                     matrix)
      "Draw the curve defined by BEZIER using a resolution of n SEGMENTS."
      (define (draw-segment start end color)
        (draw-line start end
                   #:thickness thickness
                   #:feather feather
                   #:cap 'none
                   #:color color))
      (define (draw-control-point p)
        (let ((hs (/ control-point-size 2.0)))
          (set-rect-x! rect (- (vec2-x p) hs))
          (set-rect-y! rect (- (vec2-y p) hs))
          (set-rect-width! rect control-point-size)
          (set-rect-height! rect control-point-size)
          (draw-filled-rect rect control-point-color #:matrix matrix)))
      (bezier-curve-point-at! start bezier 0.0)
      (let loop ((i 1))
        (when (<= i segments)
          (bezier-curve-point-at! end bezier (exact->inexact (/ i segments)))
          (draw-segment start end color)
          ;; Make the previous end point is now the new start point
          ;; for the next iteration.
          (set! tmp start)
          (set! start end)
          (set! end tmp)
          (loop (+ i 1))))
      (when tangents?
        (draw-segment (bezier-curve-p0 bezier)
                      (bezier-curve-p1 bezier)
                      tangent-color)
        (draw-segment (bezier-curve-p3 bezier)
                      (bezier-curve-p2 bezier)
                      tangent-color))
      (when control-points?
        (draw-control-point (bezier-curve-p0 bezier))
        (draw-control-point (bezier-curve-p1 bezier))
        (draw-control-point (bezier-curve-p2 bezier))
        (draw-control-point (bezier-curve-p3 bezier))))))

(define* (draw-bezier-path path #:key
                           (segments 32)
                           control-points?
                           tangents?
                           (control-point-size 8.0)
                           (color white)
                           (control-point-color yellow)
                           (tangent-color yellow)
                           (thickness 0.5)
                           (feather 1.0)
                           matrix)
  (for-each (lambda (bezier)
              (draw-bezier-curve bezier
                                 #:segments segments
                                 #:control-points? control-points?
                                 #:tangents? tangents?
                                 #:control-point-size control-point-size
                                 #:color color
                                 #:control-point-color control-point-color
                                 #:tangent-color tangent-color
                                 #:thickness 0.5
                                 #:feather feather
                                 #:matrix matrix))
            path))
