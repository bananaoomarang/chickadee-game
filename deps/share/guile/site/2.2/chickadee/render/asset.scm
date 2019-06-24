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
;; Implementation of the glTF 2.0 specification
;;
;;; Code:

(define-module (chickadee render asset)
  #:use-module (chickadee json)
  #:use-module (chickadee math matrix)
  #:use-module (chickadee math vector)
  #:use-module (chickadee render buffer)
  #:use-module (chickadee render color)
  #:use-module (chickadee render scene)
  #:use-module (chickadee render shader)
  #:use-module (chickadee render texture)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs base)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module ((srfi srfi-43) #:select (vector-every))
  #:export (load-asset
            asset?
            asset-copyright
            asset-generator
            asset-scenes
            asset-default-scene
            draw-asset))

(define-record-type <asset>
  (make-asset copyright generator scenes default-scene)
  asset?
  (copyright asset-copyright)
  (generator asset-generator)
  (scenes asset-scenes)
  (default-scene asset-default-scene))

(define (display-asset asset port)
  (format port "#<asset generator: ~s scene: ~s>"
          (asset-generator asset)
          (scene-name (asset-default-scene asset))))

(set-record-type-printer! <asset> display-asset)

(define (read-gltf port file)
  (define (object-ref obj key)
    (let ((value (assoc-ref obj key)))
      (unless (pair? value)
        (error "expected object for key" key value))
      value))
  (define (object-ref/optional obj key)
    (let ((value (assoc-ref obj key)))
      (unless (or (not value) (pair? value))
        (error "expected object for optional key" key value))
      value))
  (define (array-ref obj key)
    (let ((value (assoc-ref obj key)))
      (unless (vector? value)
        (error "expected array for key" key value))
      value))
  (define (array-ref/optional obj key)
    (let ((value (assoc-ref obj key)))
      (unless (or (not value) (vector? value))
        (error "expected array for optional key" key value))
      value))
  (define (string-ref obj key)
    (let ((value (assoc-ref obj key)))
      (unless (string? value)
        (error "expected string for key" key value))
      value))
  (define (string-ref/optional obj key)
    (let ((value (assoc-ref obj key)))
      (unless (or (not value) (string? value))
        (error "expected string for optional key" key value))
      value))
  (define (number-ref obj key)
    (let ((value (assoc-ref obj key)))
      (unless (number? value)
        (error "expected number for key" key value))
      value))
  (define (number-ref/optional obj key)
    (let ((value (assoc-ref obj key)))
      (unless (or (not value) (number? value))
        (error "expected number for key" key value))
      value))
  (define (boolean-ref/optional obj key)
    (let ((value (assoc-ref obj key)))
      (unless (boolean? value)
        (error "expected boolean for key" key value))
      value))
  (define (number-array-ref/optional obj key)
    (let ((value (assoc-ref obj key)))
      (unless (or (not value)
                  (and (vector? value) (vector-every number? value)))
        (error "expected numeric array for key" key value))
      value))
  (define (matrix-ref/optional obj key)
    (let ((value (assoc-ref obj key)))
      (cond
       ((not value) #f)
       ((and (vector? value)
             (= (vector-length value) 16)
             (vector-every number? value))
        ;; glTF matrices are in column-major order.
        (make-matrix4 (vector-ref value 0)
                      (vector-ref value 4)
                      (vector-ref value 8)
                      (vector-ref value 12)
                      (vector-ref value 1)
                      (vector-ref value 5)
                      (vector-ref value 9)
                      (vector-ref value 13)
                      (vector-ref value 2)
                      (vector-ref value 6)
                      (vector-ref value 10)
                      (vector-ref value 14)
                      (vector-ref value 3)
                      (vector-ref value 7)
                      (vector-ref value 11)
                      (vector-ref value 15)))
       (else
        (error "expected 4x4 matrix for key" key value)))))
  (define (assert-color v)
    (if (and (= (vector-length v) 4)
             (vector-every (lambda (x) (and (>= x 0.0) (<= x 1.0))) v))
        (make-color (vector-ref v 0)
                    (vector-ref v 1)
                    (vector-ref v 2)
                    (vector-ref v 3))
        (error "not a color vector" v)))
  (define scope-file
    (let ((gltf-root (dirname
                      (if (absolute-file-name? file)
                          file
                          (string-append (getcwd) "/" file)))))
      (lambda (file)
        (if (absolute-file-name? file)
            file
            (string-append gltf-root "/" file)))))
  (define (parse-buffer obj)
    ;; TODO: support base64 encoded buffer data as uri
    ;; TODO: support glb-stored buffers:
    ;;   https://github.com/KhronosGroup/glTF/blob/master/specification/2.0/README.md#glb-stored-buffer
    (let* ((uri (string-ref/optional obj "uri"))
           (length (number-ref obj "byteLength"))
           (name (or (string-ref/optional obj "name") "anonymous"))
           (extensions (object-ref/optional obj "extensions"))
           (extras (assoc-ref obj "extras"))
           (data (if uri
                     (call-with-input-file (scope-file uri)
                       (lambda (port)
                         (get-bytevector-n port length)))
                     (make-bytevector length))))
      data))
  (define (parse-buffer-view obj buffers)
    (let ((name (string-ref/optional obj "name"))
          (data (vector-ref buffers (number-ref obj "buffer")))
          (offset (or (number-ref/optional obj "byteOffset") 0))
          (length (number-ref obj "byteLength"))
          (stride (number-ref/optional obj "byteStride"))
          (target (match (or (number-ref/optional obj "target") 34962)
                    (34962 'vertex)
                    (34963 'index)))
          (extensions (object-ref/optional obj "extensions"))
          (extras (assoc-ref obj "extras")))
      (make-buffer data
                   #:name name
                   #:offset offset
                   #:length length
                   #:stride stride
                   #:target target)))
  (define (parse-accessor obj buffer-views)
    (define (type-length type)
      (match type
        ('scalar 1)
        ('vec2 2)
        ('vec3 3)
        ('vec4 4)
        ('mat2 4)
        ('mat3 9)
        ('mat4 16)))
    (let ((name (or (string-ref/optional obj "name") "anonymous"))
          (view (match (number-ref/optional obj "bufferView")
                  (#f #f)
                  (n (vector-ref buffer-views n))))
          (offset (or (number-ref/optional obj "byteOffset") 0))
          (component-type (match (number-ref obj "componentType")
                            (5120 'byte)
                            (5121 'unsigned-byte)
                            (5122 'short)
                            (5123 'unsigned-short)
                            (5125 'unsigned-int)
                            (5126 'float)))
          (normalized? (boolean-ref/optional obj "normalized"))
          (length (number-ref obj "count"))
          (type (match (string-ref obj "type")
                  ("SCALAR" 'scalar)
                  ("VEC2" 'vec2)
                  ("VEC3" 'vec3)
                  ("VEC4" 'vec4)
                  ("MAT2" 'mat2)
                  ("MAT3" 'mat3)
                  ("MAT4" 'mat4)))
          (max (number-array-ref/optional obj "max"))
          (min (number-array-ref/optional obj "min"))
          (sparse (object-ref/optional obj "sparse"))
          (extensions (object-ref/optional obj "extensions"))
          (extras (assoc-ref obj "extras")))
      (unless (>= length 1)
        (error "count must be greater than 0" length))
      (when (and (vector? max)
                 (not (= (vector-length max) (type-length type))))
        (error "not enough elements for max" max type))
      (when (and (vector? min)
                 (not (= (vector-length min) (type-length type))))
        (error "not enough elements for min" min type))
      (make-buffer-view #:name name
                        #:buffer view
                        #:offset offset
                        #:component-type component-type
                        #:normalized? normalized?
                        #:length length
                        #:type type
                        #:max max
                        #:min min
                        #:sparse sparse)))
  (define (texture-filter n)
    (match n
      (9728 'nearest)
      ((or #f 9729) 'linear)
      ;; TODO: Support mip-mapping
      ;; (9984 'nearest-mipmap-nearest)
      ;; (9985 'linear-mipmap-nearest)
      ;; (9986 'nearest-mipmap-linear)
      ;; (9987 'linear-mipmap-linear)
      (_ 'linear)))
  (define (texture-wrap n)
    (match n
      (10496 'clamp)
      ((or #f 10497) 'repeat)
      (33069 'clamp-to-border)
      (33071 'clamp-to-edge)))
  (define (parse-texture obj images samplers)
    (let ((image (vector-ref images (number-ref obj "source")))
          (sampler
           (vector-ref samplers (or (number-ref/optional obj "sampler") 0))))
      (load-image (scope-file (string-ref image "uri"))
                  #:min-filter (texture-filter
                                (number-ref/optional sampler "minFilter"))
                  #:mag-filter (texture-filter
                                (number-ref/optional sampler "magFilter"))
                  #:wrap-s (texture-wrap (number-ref/optional sampler "wrapS"))
                  #:wrap-t (texture-wrap (number-ref/optional sampler "wrapT")))))
  (define (parse-material obj textures)
    (let* ((name (or (string-ref/optional obj "name") "anonymous"))
           (pbrmr (or (object-ref/optional obj "pbrMetallicRoughness") '()))
           (base-color-factor
            (let ((v (or (number-array-ref/optional pbrmr "baseColorFactor")
                         #(1.0 1.0 1.0 1.0))))
              (vec3 (vector-ref v 0) (vector-ref v 1) (vector-ref v 2))))
           (base-color-texture
            (match (object-ref/optional pbrmr "baseColorTexture")
              (#f null-texture)
              (obj
               (vector-ref textures (number-ref obj "index")))))
           (metallic-factor
            (or (number-ref/optional pbrmr "metallicFactor")
                1.0))
           (roughness-factor
            (or (number-ref/optional pbrmr "roughnessFactor")
                1.0))
           (metallic-roughness-texture
            (match (object-ref/optional pbrmr "metallicRoughnessTexture")
              (#f null-texture)
              (obj
               (vector-ref textures (number-ref obj "index")))))
           (normal-factor
            (let ((v (or (array-ref/optional obj "normalFactor")
                         #(1.0 1.0 1.0))))
              (vec3 (vector-ref v 0) (vector-ref v 1) (vector-ref v 2))))
           (normal-texture
            (match (object-ref/optional obj "normalTexture")
              (#f null-texture)
              (obj (vector-ref textures (number-ref obj "index")))))
           (occlusion-factor
            (let ((v (or (array-ref/optional obj "occlusionFactor")
                         #(1.0 1.0 1.0))))
              (vec3 (vector-ref v 0) (vector-ref v 1) (vector-ref v 2))))
           (occlusion-texture
            (match (object-ref/optional obj "occlusionTexture")
              (#f null-texture)
              (obj (vector-ref textures (number-ref obj "index")))))
           (emissive-factor
            (let ((v (or (array-ref/optional obj "emissiveFactor")
                         #(1.0 1.0 1.0))))
              (vec3 (vector-ref v 0) (vector-ref v 1) (vector-ref v 2))))
           (emissive-texture
            (match (object-ref/optional obj "emissiveTexture")
              (#f null-texture)
              (obj (vector-ref textures (number-ref obj "index")))))
           (alpha-mode (match (or (string-ref/optional obj "alphaMode")
                                  "BLEND")
                         ("OPAQUE" 'opaque)
                         ("MASK" 'mask)
                         ("BLEND" 'blend)))
           (alpha-cutoff (or (number-ref/optional obj "alphaCutoff") 0.5))
           (double-sided? (boolean-ref/optional obj "doubleSided"))
           (extensions (object-ref/optional obj "extensions"))
           (extras (assoc-ref obj "extras")))
      (make-material #:name name
                     #:base-color-factor base-color-factor
                     #:base-color-texture base-color-texture
                     #:metallic-factor metallic-factor
                     #:roughness-factor roughness-factor
                     #:metallic-roughness-texture metallic-roughness-texture
                     #:normal-factor normal-factor
                     #:normal-texture normal-texture
                     #:occlusion-factor occlusion-factor
                     #:occlusion-texture occlusion-texture
                     #:emissive-factor emissive-factor
                     #:emissive-texture emissive-texture
                     #:alpha-mode alpha-mode
                     #:alpha-cutoff alpha-cutoff
                     #:double-sided? double-sided?)))
  (define (attribute-name->index name)
    (match name
      ("POSITION"
       (attribute-location
        (hash-ref (shader-attributes (pbr-shader)) "position")))
      ("NORMAL" 1)
      ("TANGENT" 2)
      ("TEXCOORD_0"
       (attribute-location
        (hash-ref (shader-attributes (pbr-shader)) "texcoord_0")))
      ("TEXCOORD_1" 4)
      ("COLOR_0" 5)
      ("JOINTS_0" 6)
      ("WEIGHTS_0" 7)))
  (define (parse-primitive obj materials accessors)
    (let ((attributes (map (match-lambda
                             ((name . n)
                              (cons (attribute-name->index name)
                                    (vector-ref accessors n))))
                           (object-ref obj "attributes")))
          (indices (match (number-ref/optional obj "indices")
                     (#f #f)
                     (n (vector-ref accessors n))))
          ;; TODO: Set a default material when none is given.
          (material (match (number-ref/optional obj "material")
                      (#f #f)
                      (n (vector-ref materials n))))
          (mode (match (or (number-ref/optional obj "mode") 4)
                  (0 'points)
                  (1 'lines)
                  (2 'line-loop)
                  (3 'line-strip)
                  (4 'triangles)
                  (5 'triangle-strip)
                  (6 'triangle-fan)))
          ;; TODO: Support morph targets.
          (targets #f))
      (make-primitive #:vertex-array
                      (make-vertex-array #:indices indices
                                         #:attributes attributes
                                         #:mode mode)
                      #:material material
                      #:targets targets)))
  (define (parse-mesh obj materials accessors)
    (let ((name (or (string-ref/optional obj "name") "anonymous"))
          (primitives
           (map (lambda (obj)
                  (parse-primitive obj materials accessors))
                (vector->list (array-ref obj "primitives"))))
          (weights (number-array-ref/optional obj "weights")))
      (make-mesh #:name name
                 #:primitives primitives
                 #:weights weights)))
  (define (parse-node obj parse-child meshes)
    ;; TODO: Parse all fields of nodes.
    (let ((name (or (string-ref/optional obj "name") "anonymous"))
          ;; TODO: Parse camera.
          (camera #f)
          ;; TODO: Parse skin.
          (skin #f)
          (matrix (or (matrix-ref/optional obj "matrix")
                      (make-identity-matrix4)))
          (mesh (match (number-ref/optional obj "mesh")
                  (#f #f)
                  (n (vector-ref meshes n))))
          ;; TODO: Parse rotation, scale, translation
          (rotation #f)
          (scale #f)
          (translation #f)
          ;; TODO: Parse weights.
          (weights #f)
          (children (map parse-child
                         (vector->list
                          (or (array-ref/optional obj "children")
                              #())))))
      (make-scene-node #:name name
                       #:children children
                       #:camera camera
                       #:skin skin
                       #:matrix matrix
                       #:mesh mesh
                       #:rotation rotation
                       #:scale scale
                       #:translation translation
                       #:weights weights)))
  (define (parse-nodes array meshes)
    (define nodes (make-vector (vector-length array) #f))
    (define (parse-node* i)
      (let ((node (vector-ref nodes i)))
        (or node
            (let ((node (parse-node (vector-ref array i)
                                    parse-node*
                                    meshes)))
              (vector-set! nodes i node)
              node))))
    (let loop ((i 0))
      (when (< i (vector-length array))
        (parse-node* i)
        (loop (+ i 1))))
    nodes)
  (define (parse-scene obj nodes)
    (let ((name (or (string-ref/optional obj "name") "anonymous"))
          (children
           (map (lambda (i) (vector-ref nodes i))
                (vector->list
                 (or (number-array-ref/optional obj "nodes")
                     #())))))
      (make-scene #:name name #:nodes children)))
  (let* ((tree (read-json port))
         (asset (object-ref tree "asset"))
         (version (string-ref asset "version"))
         (copyright (string-ref/optional asset "copyright"))
         (generator (string-ref/optional asset "generator"))
         (minimum-version (string-ref/optional asset "minVersion"))
         (extensions (object-ref/optional asset "extensions"))
         ;; TODO: Figure out how to parse extras in a user-defined way
         (extras (assoc-ref asset "extras"))
         (buffers (vector-map parse-buffer
                              (or (assoc-ref tree "buffers") #())))
         (buffer-views (vector-map (lambda (obj)
                                     (parse-buffer-view obj buffers))
                                   (or (assoc-ref tree "bufferViews") #())))
         (accessors (vector-map (lambda (obj)
                                  (parse-accessor obj buffer-views))
                                (or (assoc-ref tree "accessors") #())))
         (images (or (assoc-ref tree "images") #()))
         (samplers (or (assoc-ref tree "samplers") #(())))
         (textures (vector-map (lambda (obj)
                                 (parse-texture obj images samplers))
                               (or (assoc-ref tree "textures") #())))
         (materials (vector-map (lambda (obj)
                                  (parse-material obj textures))
                                (or (assoc-ref tree "materials") #())))
         (meshes (vector-map (lambda (obj)
                               (parse-mesh obj materials accessors))
                             (or (assoc-ref tree "meshes") #())))
         (nodes (parse-nodes (or (assoc-ref tree "nodes") #()) meshes))
         (scenes (map (lambda (obj)
                        (parse-scene obj nodes))
                      (vector->list
                       (or (assoc-ref tree "scenes") #()))))
         (default-scene (list-ref scenes
                                  (or (number-ref/optional tree "scene")
                                      0))))
    (unless (string=? version "2.0")
      (error "unsupported glTF version" version))
    (make-asset copyright generator scenes default-scene)))

(define (load-asset file)
  (call-with-input-file file (lambda (port) (read-gltf port file))))
