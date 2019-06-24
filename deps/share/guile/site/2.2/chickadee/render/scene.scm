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
;; 3D scene graph with physically based rendering.
;;
;;; Code:

(define-module (chickadee render scene)
  #:use-module (chickadee config)
  #:use-module (chickadee math matrix)
  #:use-module (chickadee math quaternion)
  #:use-module (chickadee math vector)
  #:use-module (chickadee render)
  #:use-module (chickadee render color)
  #:use-module (chickadee render buffer)
  #:use-module (chickadee render texture)
  #:use-module (chickadee render shader)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-material
            default-material
            material?
            material-name
            material-base-color-factor
            material-base-color-texture
            material-metallic-factor
            material-roughness-factor
            material-metallic-roughness-texture
            material-normal-factor
            material-normal-texture
            material-occlusion-facgor
            material-occlusion-texture
            material-emissive-factor
            material-emissive-texture
            material-alpha-mode
            material-alpha-cutoff
            material-double-sided?
            pbr-shader
            make-primitive
            primitive?
            primitive-vertex-array
            primitive-material
            primitive-targets
            make-mesh
            mesh?
            mesh-name
            mesh-primitives
            mesh-weights
            make-scene-node
            scene-node?
            scene-node-name
            scene-node-children
            scene-node-camera
            scene-node-skin
            scene-node-matrix
            scene-node-mesh
            scene-node-rotation
            scene-node-scale
            scene-node-translation
            scene-node-weights
            make-scene
            scene?
            scene-name
            scene-nodes
            draw-scene))

(define-record-type <material>
  (%make-material name base-color-factor base-color-texture
                  metallic-factor roughness-factor metallic-roughness-texture
                  normal-factor normal-texture
                  occlusion-factor occlusion-texture
                  emissive-texture emissive-factor
                  alpha-mode alpha-cutoff
                  double-sided?)
  material?
  (name material-name)
  (base-color-factor material-base-color-factor)
  (base-color-texture material-base-color-texture)
  (metallic-factor material-metallic-factor)
  (roughness-factor material-roughness-factor)
  (metallic-roughness-texture material-metallic-roughness-texture)
  (normal-factor material-normal-factor)
  (normal-texture material-normal-texture)
  (occlusion-factor material-occlusion-factor)
  (occlusion-texture material-occlusion-texture)
  (emissive-factor material-emissive-factor)
  (emissive-texture material-emissive-texture)
  (alpha-mode material-alpha-mode)
  (alpha-cutoff material-alpha-cutoff)
  (double-sided? material-double-sided?))

(define* (make-material #:key
                        (name "anonymous")
                        (base-color-factor #v(1.0 1.0 1.0))
                        (base-color-texture null-texture)
                        (metallic-factor 1.0)
                        (roughness-factor 1.0)
                        (metallic-roughness-texture null-texture)
                        (normal-factor #v(1.0 1.0 1.0))
                        (normal-texture null-texture)
                        (occlusion-factor #v(1.0 1.0 1.0))
                        (occlusion-texture null-texture)
                        (emissive-factor #v(1.0 1.0 1.0))
                        (emissive-texture null-texture)
                        (alpha-mode 'opaque)
                        (alpha-cutoff 0.5)
                        double-sided?)
  (%make-material name base-color-factor base-color-texture
                  metallic-factor roughness-factor metallic-roughness-texture
                  normal-factor normal-texture
                  occlusion-factor occlusion-texture
                  emissive-texture emissive-factor
                  alpha-mode alpha-cutoff double-sided?))

(define default-material (make-material))

(define pbr-shader
  (let ((shader
         (delay
           (load-shader (scope-datadir "shaders/pbr/pbr-vert.glsl")
                        (scope-datadir "shaders/pbr/pbr-frag.glsl")))))
    (lambda ()
      (force shader))))

(define-record-type <primitive>
  (%make-primitive vertex-array material targets matrix)
  primitive?
  (vertex-array primitive-vertex-array)
  (material primitive-material)
  (targets primitive-targets)
  (matrix primitive-matrix))

(define (display-primitive primitive port)
  (format port "#<primitive material: ~s targets: ~s>"
          (primitive-material primitive)
          (primitive-targets primitive)))

(set-record-type-printer! <primitive> display-primitive)

(define* (make-primitive #:key
                         vertex-array
                         (material default-material)
                         targets)
  (%make-primitive vertex-array material targets (make-identity-matrix4)))

(define-record-type <mesh>
  (%make-mesh name primitives weights)
  mesh?
  (name mesh-name)
  (primitives mesh-primitives)
  (weights mesh-weights))

(define (display-mesh mesh port)
  (format port "#<mesh name: ~s>" (mesh-name mesh)))

(set-record-type-printer! <mesh> display-mesh)

(define* (make-mesh #:key
                    (name "anonymous")
                    primitives
                    weights)
  (%make-mesh name primitives weights))

(define-record-type <scene-node>
  (%make-scene-node name children camera skin matrix world-matrix
                    mesh rotation scale translation weights)
  node?
  (name scene-node-name)
  (children scene-node-children)
  (camera scene-node-camera)
  (skin scene-node-skin)
  (matrix scene-node-matrix)
  (world-matrix scene-node-world-matrix)
  (mesh scene-node-mesh)
  (rotation scene-node-rotation)
  (scale scene-node-scale)
  (translation scene-node-translation)
  (weights scene-node-weights))

(define (display-scene-node scene port)
  (format port "#<scene-node name: ~s>" (scene-node-name scene)))

(set-record-type-printer! <scene-node> display-scene-node)

(define* (make-scene-node #:key
                          (name "anonymous")
                          (children #())
                          camera
                          skin
                          (matrix (make-identity-matrix4))
                          mesh
                          (rotation (quaternion 0.0 0.0 0.0 1.0))
                          (scale (vec3 1.0 1.0 1.0))
                          (translation (vec3 0.0 0.0 0.0))
                          weights)
  (%make-scene-node name children camera skin matrix (make-identity-matrix4)
                    mesh rotation scale translation weights))

(define-record-type <scene>
  (%make-scene name nodes)
  scene?
  (name scene-name)
  (nodes scene-nodes))

(define* (make-scene #:key (name "anonymous") (nodes '()))
  (%make-scene name nodes))

(define (draw-primitive primitive matrix)
  ;; TODO: Actually use physically based rendering.
  (let ((mvp (primitive-matrix primitive))
        (material (primitive-material primitive)))
    (matrix4-mult! mvp matrix (current-projection))
    (with-texture 0 (material-base-color-texture material)
      (gpu-apply (pbr-shader)
                 (primitive-vertex-array primitive)
                 #:mvp mvp
                 #:base_color_factor (material-base-color-factor material)))))

(define (draw-mesh mesh matrix)
  (for-each (lambda (primitive)
              (draw-primitive primitive matrix))
            (mesh-primitives mesh)))

(define (draw-scene-node node matrix)
  (let ((world-matrix (scene-node-world-matrix node)))
    (matrix4-mult! world-matrix matrix (scene-node-matrix node))
    (when (scene-node-mesh node)
      (draw-mesh (scene-node-mesh node) world-matrix))
    (for-each (lambda (node)
                (draw-scene-node node world-matrix))
              (scene-node-children node))))

(define (draw-scene scene)
  (for-each (lambda (scene)
              (draw-scene-node scene (scene-node-world-matrix scene)))
            (scene-nodes scene)))
