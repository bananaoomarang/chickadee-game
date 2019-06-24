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

;;; Commentary:
;;
;; Tiled map format parser and renderer.
;;
;;; Code:

(define-module (chickadee render tiled)
  #:use-module (chickadee math matrix)
  #:use-module (chickadee math rect)
  #:use-module (chickadee math vector)
  #:use-module (chickadee render)
  #:use-module (chickadee render color)
  #:use-module (chickadee render sprite)
  #:use-module (chickadee render texture)
  #:use-module (chickadee render viewport)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (sxml simple)
  #:use-module (sxml xpath)
  #:export (tile-map?
            tile-map-orientation
            tile-map-width
            tile-map-height
            tile-map-tile-width
            tile-map-tile-height
            tile-map-tilesets
            tile-map-layers
            tile-map-properties
            tile-map-rect
            tile-map-layer-ref
            point->tile

            animation-frame?
            animation-frame-tile
            animation-frame-duration

            tile?
            tile-id
            tile-animation
            tile-properties

            tileset?
            tileset-name
            tileset-first-gid
            tileset-size
            tileset-tile-width
            tileset-tile-height
            tileset-atlas
            tileset-tiles
            tileset-properties

            map-tile?
            map-tile-ref
            map-tile-rect

            tile-layer?
            tile-layer-name
            tile-layer-width
            tile-layer-height
            tile-layer-tiles
            tile-layer-properties

            object-layer?
            object-layer-name
            object-layer-objects
            object-layer-properties

            polygon?
            polygon-points

            map-object?
            map-object-id
            map-object-name
            map-object-type
            map-object-shape
            map-object-properties

            load-tile-map
            draw-tile-map
            draw-tile-map*))

(define-record-type <tile-map>
  (%make-tile-map orientation width height tile-width tile-height
                  tilesets layers properties rect)
  tile-map?
  (orientation tile-map-orientation)
  (width tile-map-width)
  (height tile-map-height)
  (tile-width tile-map-tile-width)
  (tile-height tile-map-tile-height)
  (tilesets tile-map-tilesets)
  (layers tile-map-layers)
  (properties tile-map-properties)
  (rect tile-map-rect))

(define-record-type <animation-frame>
  (%make-animation-frame tile duration)
  animation-frame?
  (tile animation-frame-tile)
  (duration animation-frame-duration))

(define-record-type <tile>
  (%make-tile id texture batch animation properties)
  tile?
  (id tile-id)
  (texture tile-texture)
  (batch tile-batch)
  (animation tile-animation)
  (properties tile-properties))

(define-record-type <tileset>
  (%make-tileset name first-gid size tile-width tile-height
                 atlas tiles properties batch)
  tileset?
  (name tileset-name)
  (first-gid tileset-first-gid)
  (size tileset-size)
  (tile-width tileset-tile-width)
  (tile-height tileset-tile-height)
  (atlas tileset-atlas)
  (tiles tileset-tiles)
  (properties tileset-properties)
  (batch tileset-batch))

(define-record-type <map-tile>
  (%make-map-tile tile rect)
  map-tile?
  (tile map-tile-ref)
  (rect map-tile-rect))

(define-record-type <tile-layer>
  (%make-tile-layer name width height tiles properties)
  tile-layer?
  (name tile-layer-name)
  (width tile-layer-width)
  (height tile-layer-height)
  (tiles tile-layer-tiles)
  (properties tile-layer-properties))

(define-record-type <object-layer>
  (%make-object-layer name objects properties)
  object-layer?
  (name object-layer-name)
  (objects object-layer-objects)
  (properties object-layer-properties))

;; TODO: This should probably be a generic thing that we can use
;; outside of tiled maps.
(define-record-type <polygon>
  (make-polygon points)
  polygon?
  (points polygon-points))

(define-record-type <map-object>
  (%make-map-object id name type shape properties)
  map-object?
  (id map-object-id)
  (name map-object-name)
  (type map-object-type)
  (shape map-object-shape)
  (properties map-object-properties))

(define (tile-map-layer-ref tile-map name)
  "Return the layer named NAME."
  (define (layer-name layer)
    (if (tile-layer? layer)
        (tile-layer-name layer)
        (object-layer-name layer)))
  (let ((layers (tile-map-layers tile-map)))
    (let loop ((i 0))
      (cond
       ((= i (vector-length layers))
        #f)
       ((string=? name (layer-name (vector-ref layers i)))
        (vector-ref layers i))
       (else
        (loop (+ i 1)))))))

(define (point->tile tile-map x y)
  "Translate the pixel coordinates (X, Y) into tile coordinates."
  (values (floor (/ x (tile-map-tile-width tile-map)))
          (floor (/ y (tile-map-tile-height tile-map)))))

(define (load-tile-map file-name)
  "Load the Tiled TMX formatted map in FILE-NAME."
  (define map-directory
    (if (absolute-file-name? file-name)
        (dirname file-name)
        (string-append (getcwd) "/" (dirname file-name))))
  (define (scope file-name)
    (string-append map-directory "/" file-name))
  (define* (attr node name #:optional (parse identity))
    (let ((result ((sxpath `(@ ,name *text*)) node)))
      (if (null? result)
          #f
          (parse (car result)))))
  (define (parse-color-channel s start)
    (/ (string->number (substring s start (+ start 2)) 16) 255.0))
  (define (parse-property node)
    (let ((name (attr node 'name string->symbol))
          (type (or (attr node 'type string->symbol) 'string))
          (value (attr node 'value)))
      (cons name
            (match type
              ((or 'string 'file) value)
              ('bool (not (string=? value "false")))
              ((or 'int 'float) (string->number value))
              ('color
               (make-color (parse-color-channel value 3)
                           (parse-color-channel value 5)
                           (parse-color-channel value 7)
                           (parse-color-channel value 1)))
              (_ (error "unsupported property type" type))))))
  (define (parse-image node)
    (let ((source (attr node 'source)))
      (load-image (scope source))))
  (define (parse-frame node)
    (let ((tile-id (attr node 'tileid string->number))
          (duration (attr node 'duration string->number)))
      ;; TODO: lookup actual tile in tileset
      (%make-animation-frame tile-id duration)))
  (define (parse-tile node rows columns atlas batch)
    (let ((id (attr node 'id string->number))
          (animation (map parse-frame ((sxpath '(animation frame)) node)))
          (properties (map parse-property
                           ((sxpath '(properties property)) node))))
      (%make-tile id (texture-atlas-ref atlas id) batch animation properties)))
  (define (parse-tiles nodes size columns atlas batch)
    (let ((table (make-hash-table))
          (tiles (make-vector size))
          (rows (/ size columns)))
      (for-each (lambda (node)
                  (let ((tile (parse-tile node rows columns atlas batch)))
                    (hash-set! table (tile-id tile) tile)))
                nodes)
      (let loop ((i 0))
        (when (< i size)
          (let ((tile
                 (or (hash-ref table i)
                     (%make-tile i (texture-atlas-ref atlas i) batch #f '()))))
            (vector-set! tiles i tile))
          (loop (+ i 1))))
      tiles))
  (define (first-gid node)
    (attr node 'firstgid string->number))
  (define (parse-internal-tileset node first-gid)
    (let* ((name (attr node 'name))
           (tile-width (attr node 'tilewidth string->number))
           (tile-height (attr node 'tileheight string->number))
           (margin (or (attr node 'margin string->number) 0))
           (spacing (or (attr node 'spacing string->number) 0))
           (columns (attr node 'columns string->number))
           (size (attr node 'tilecount string->number))
           (texture (parse-image ((sxpath '(image)) node)))
           (atlas (split-texture texture tile-width tile-height
                                 #:margin margin #:spacing spacing))
           (batch (make-sprite-batch texture))
           (tiles (parse-tiles ((sxpath '(tile)) node) size columns atlas batch))
           (properties (map parse-property
                            ((sxpath '(properties property)) node))))
      (%make-tileset name first-gid size tile-width tile-height
                     atlas tiles properties batch)))
  (define (parse-external-tileset node)
    (let* ((first-gid (attr node 'firstgid string->number))
           (source (scope (attr node 'source)))
           (tree (call-with-input-file source xml->sxml)))
      (parse-internal-tileset (car ((sxpath '(tileset)) tree)) first-gid)))
  (define (parse-tileset node)
    (if (attr node 'source)
        (parse-external-tileset node)
        (parse-internal-tileset node (first-gid node))))
  (define (tile-gid->map-tile raw-gid tilesets x y tile-width tile-height)
    ;; The top 3 bits of the tile gid are flags for various types of
    ;; flipping.
    ;;
    ;; TODO: Respect the flipping settings.
    (let* ((flipped-horizontally? (> (logand raw-gid #x80000000) 0))
           (flipped-vertically? (> (logand raw-gid #x40000000) 0))
           (flipped-diagonally? (> (logand raw-gid #x20000000) 0))
           ;; Remove the upper 3 bits to get the true tile id.
           (gid (logand raw-gid #x1FFFFFFF))
           (tileset (find (lambda (t)
                            (and (>= gid (tileset-first-gid t))
                                 (< gid (+ (tileset-first-gid t)
                                           (tileset-size t)))))
                          tilesets))
           (tw (tileset-tile-width tileset))
           (th (tileset-tile-height tileset)))
      (%make-map-tile (vector-ref (tileset-tiles tileset)
                                  (- gid (tileset-first-gid tileset)))
                      (make-rect (* x tw) (* y th) tw th))))
  (define (tile-gids->map-tiles gids width height tilesets)
    (let ((tiles (make-vector (* width height))))
      (let y-loop ((y 0)
                   (rows (reverse gids))) ; invert y
        (when (< y height)
          (match rows
            ((row . rest)
             (let x-loop ((x 0)
                          (columns row))
               (when (< x width)
                 (match columns
                   ((gid . rest)
                    (vector-set! tiles
                                 (+ (* width y) x)
                                 (if (zero? gid)
                                     #f
                                     (tile-gid->map-tile gid tilesets
                                                         x y width height)))
                    (x-loop (+ x 1) rest)))))
             (y-loop (+ y 1) rest)))))
      tiles))
  (define (parse-csv lines width height tilesets)
    (let ((gids (map (lambda (line)
                       (filter-map (lambda (s)
                                     (and (not (string-null? s))
                                          (string->number s)))
                                   (string-split line #\,)))
                     (take (drop (string-split lines #\newline) 1) height))))
      (tile-gids->map-tiles gids width height tilesets)))
  (define (parse-layer-data node width height tilesets)
    (let ((encoding (attr node 'encoding string->symbol))
          (data (car ((sxpath '(*text*)) node))))
      (match encoding
        ('csv (parse-csv data width height tilesets))
        (_ (error "unsupported tile layer encoding" encoding)))))
  (define (parse-tile-layer node tilesets)
    (let* ((name (attr node 'name))
           (width (attr node 'width string->number))
           (height (attr node 'height string->number))
           (tiles (parse-layer-data ((sxpath '(data)) node)
                                    width height tilesets))
           (properties (map parse-property
                            ((sxpath '(properties property)) node))))
      (%make-tile-layer name width height tiles properties)))
  (define (parse-polygon node pixel-height)
    (make-polygon
     (list->vector
      (map (lambda (s)
             (match (string-split s #\,)
               ((x y)
                (vec2 (string->number x)
                      (- pixel-height (string->number y))))))
           (string-split (attr node 'points) #\space)))))
  (define (parse-object node pixel-height)
    (let* ((id (attr node 'id string->number))
           (name (attr node 'name))
           (type (attr node 'type string->symbol))
           (x (attr node 'x string->number))
           (y (- pixel-height (attr node 'y string->number)))
           (width (attr node 'width string->number))
           (height (attr node 'height string->number))
           (shape (if (and width height)
                      (make-rect x y width height)
                      (parse-polygon (car ((sxpath '(polygon)) node))
                                     pixel-height)))
           (properties (map parse-property
                            ((sxpath '(properties property)) node))))
      (%make-map-object id name type shape properties)))
  (define (parse-object-layer node pixel-height)
    (let ((name (attr node 'name))
          (objects (map (lambda (node)
                          (parse-object node pixel-height))
                        ((sxpath '(object)) node)))
          (properties (map parse-property
                           ((sxpath '(properties property)) node))))
      (%make-object-layer name objects properties)))
  (let* ((tree (call-with-input-file file-name xml->sxml))
         (m ((sxpath '(map)) tree))
         (version (let ((version (attr m 'version)))
                    (unless (any (lambda (v) (string=? version v)) '("1.0" "1.1" "1.2"))
                      (error "unsupported tiled map format version" version))
                    version))
         (orientation (attr m 'orientation string->symbol))
         (width (attr m 'width string->number))
         (height (attr m 'height string->number))
         (tile-width (attr m 'tilewidth string->number))
         (tile-height (attr m 'tileheight string->number))
         (properties ((sxpath '(map properties property)) tree))
         (tilesets (map parse-tileset ((sxpath '(map tileset)) tree)))
         (layers ((node-or (sxpath '(map layer))
                           (sxpath '(map objectgroup)))
                  tree)))
    (%make-tile-map orientation width height tile-width tile-height
                    tilesets
                    (list->vector
                     (map (lambda (node)
                            (match node
                              (('layer . _)
                               (parse-tile-layer node tilesets))
                              (('objectgroup . _)
                               (parse-object-layer node (* height tile-height)))))
                          layers))
                    (map parse-property properties)
                    (make-rect 0.0
                               0.0
                               (* width tile-width)
                               (* height tile-height)))))


(define (draw-tile-layer layer matrix x1 y1 x2 y2)
  (let ((width (tile-layer-width layer))
        (height (tile-layer-height layer)))
    (let y-loop ((y y1))
      (when (< y y2)
        (let x-loop ((x x1))
          (when (< x x2)
            (let ((tile (vector-ref (tile-layer-tiles layer)
                                    (+ (* y width) x))))
              (when tile
                (let ((tref (map-tile-ref tile)))
                  (sprite-batch-add* (tile-batch tref)
                                     (map-tile-rect tile)
                                     matrix
                                     #:texture-region (tile-texture tref)))))
            (x-loop (+ x 1))))
        (y-loop (+ y 1))))))

(define* (draw-tile-map* tile-map matrix region #:key layers)
  ;; Calculate the tiles that are visible so we don't waste time
  ;; drawing unnecessary sprites.
  (let* ((w (tile-map-width tile-map))
         (h (tile-map-height tile-map))
         (tw (tile-map-tile-width tile-map))
         (th (tile-map-tile-height tile-map))
         (rx (rect-x region))
         (ry (rect-y region))
         (rw (rect-width region))
         (rh (rect-height region))
         (x1 (max (inexact->exact (floor (/ rx tw))) 0))
         (y1 (max (inexact->exact (floor (/ ry th))) 0))
         (x2 (min (inexact->exact (ceiling (/ (+ rx rw) tw))) w))
         (y2 (min (inexact->exact (ceiling (/ (+ ry rh) th))) h)))
    (vector-for-each (lambda (i layer)
                       (when (and (tile-layer? layer)
                                  (or (not layers)
                                      (memv i layers)))
                             (for-each (lambda (tileset)
                                         (sprite-batch-clear! (tileset-batch tileset)))
                                       (tile-map-tilesets tile-map))
                             (draw-tile-layer layer matrix x1 y1 x2 y2)
                             (for-each (lambda (tileset)
                                         (draw-sprite-batch (tileset-batch tileset)))
                                       (tile-map-tilesets tile-map))))
                     (tile-map-layers tile-map))))

(define %null-vec2 (vec2 0.0 0.0))
(define %default-scale (vec2 1.0 1.0))
(define %matrix (make-null-matrix4))
(define %region (make-rect 0.0 0.0 0.0 0.0))

;; Make a default region that is as big as the viewport.
(define (default-region tile-map position)
  (let ((vp (current-viewport)))
    (set-rect-x! %region (- (vec2-x position)))
    (set-rect-y! %region (- (vec2-y position)))
    (set-rect-width! %region (viewport-width vp))
    (set-rect-height! %region (viewport-height vp))
    %region))

(define* (draw-tile-map tile-map
                        #:key
                        layers
                        (position %null-vec2)
                        (region (default-region tile-map position))
                        (origin %null-vec2)
                        (scale %default-scale)
                        (rotation 0.0))
  "Draw TILE-MAP.  By default, all layers are drawn.  The LAYERS
argument may be used to specify a list of layers to draw, instead."
  (matrix4-2d-transform! %matrix
                         #:origin origin
                         #:position position
                         #:rotation rotation
                         #:scale scale)
  (draw-tile-map* tile-map %matrix region #:layers layers))
