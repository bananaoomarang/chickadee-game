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
;; Axis-aligned bounding box collision grid.
;;
;; Lots of inspiration drawn from https://github.com/kikito/bump.lua
;;
;;; Code:

(define-module (chickadee math grid)
  #:use-module (chickadee array-list)
  #:use-module (chickadee math rect)
  #:use-module (chickadee math vector)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:export (slide
            cell?
            cell-count
            make-grid
            grid?
            grid-cell-size
            grid-add
            grid-remove
            grid-move
            grid-clear
            grid-cell-count
            grid-item-count
            for-each-cell
            for-each-item))


;;;
;;; Collision resolvers
;;;

(define (slide item item-rect other other-rect goal)
  "Resolve the collision that occurs between ITEM and OTHER when
moving ITEM-RECT to GOAL by sliding ITEM-RECT the minimum amount
needed to make the rectangles no longer overlap."
  (let* ((goal-x (vec2-x goal))
         (goal-y (vec2-y goal))
         (x1 (max goal-x (rect-left other-rect)))
         (x2 (min (+ goal-x (rect-width item-rect)) (rect-right other-rect)))
         (y1 (max goal-y (rect-bottom other-rect)))
         (y2 (min (+ goal-y (rect-height item-rect)) (rect-top other-rect)))
         (x-fix (- x2 x1))
         (y-fix (- y2 y1)))
    (if (< x-fix y-fix)
        (if (= goal-x x1)
            (set-vec2-x! goal (+ (vec2-x goal) x-fix))
            (set-vec2-x! goal (- (vec2-x goal) x-fix)))
        (if (= goal-y y1)
            (set-vec2-y! goal (+ (vec2-y goal) y-fix))
            (set-vec2-y! goal (- (vec2-y goal) y-fix))))))


;;;
;;; Cells
;;;

(define-record-type <cell>
  (make-cell items count)
  cell?
  (items cell-items)
  (count cell-count set-cell-count!))

(define (make-empty-cell)
  (make-cell (make-hash-table) 0))

(define (cell-add cell item)
  (hashq-set! (cell-items cell) item #t)
  (set-cell-count! cell (+ (cell-count cell) 1)))

(define (cell-remove cell item)
  (hashq-remove! (cell-items cell) item)
  (set-cell-count! cell (- (cell-count cell) 1)))


;;;
;;; Grid
;;;

(define-record-type <grid>
  (%make-grid cell-size rects rows scratch-rect buffer visited)
  grid?
  (cell-size %grid-cell-size)
  (rects grid-rects)
  (rows grid-rows)
  ;; The below fields are scratch space data structures that are
  ;; allocated once when the grid is created to cut down on
  ;; allocations while modifying the grid and checking for collisions.
  (scratch-rect grid-scratch-rect)
  (buffer grid-buffer)
  (visited grid-visited))

(define* (make-grid #:optional (cell-size 64.0))
  "Create new grid partitioned by CELL-SIZE."
  (%make-grid (f32vector cell-size)
              (make-hash-table 1000)
              (make-hash-table)
              (make-rect 0.0 0.0 0.0 0.0)
              (make-array-list)
              (make-hash-table)))

(define-inlinable (grid-cell-size grid)
  (f32vector-ref (%grid-cell-size grid) 0))

(define (item-in-grid? grid item)
  "Return #t if ITEM is in GRID."
  (hashq-ref (grid-rects grid) item))

(define (grid-row-ref grid y)
  "Return the row at index Y in GRID."
  (let ((rows (grid-rows grid)))
    (or (hashq-ref rows y)
        (let ((new-row (make-hash-table)))
          (hashq-set! rows y new-row)
          new-row))))

(define (row-column-ref row x)
  "Return the cell at index X in ROW."
  (or (hashq-ref row x)
      (let ((new-cell (make-empty-cell)))
        (hashq-set! row x new-cell)
        new-cell)))

(define (grid-cell-ref grid x y)
  "Return the cell in GRID at (X, Y)."
  (row-column-ref (grid-row-ref grid y) x))

(define (grid-rect-ref grid item)
  "Return the rect for ITEM in GRID."
  (hashq-ref (grid-rects grid) item))

(define (grid-cell-bounds grid rect)
  "Return the range of cells that RECT occupies in GRID.  The first
two return values are the min/max x coordinate, the last two are the
min/max y coordinate."
  (let ((cell-size (grid-cell-size grid))
        (x (rect-x rect))
        (y (rect-y rect))
        (w (rect-width rect))
        (h (rect-height rect)))
    (define (to-cell n)
      (inexact->exact (floor (/ n cell-size))))
    (values (to-cell x)
            (to-cell (+ x w))
            (to-cell y)
            (to-cell (+ y h)))))

(define-inlinable (for-each-coord proc minx maxx miny maxy)
;;   "Call PROC with each (X, Y) coordinate pair formed by the inclusive
;; ranges [MINX, MAXX] and [MINY, MAXY]."
  (let yloop ((y miny))
    (when (<= y maxy)
      (let xloop ((x minx))
        (when (<= x maxx)
          (proc x y)
          (xloop (+ x 1))))
      (yloop (+ y 1)))))

(define* (for-each-cell proc grid #:optional rect)
  "Call PROC with each cell in GRID that intersects RECT, or every
cell if RECT is #f."
  (if rect
      (let-values (((minx maxx miny maxy) (grid-cell-bounds grid rect)))
        (for-each-coord (lambda (x y)
                          (proc (grid-cell-ref grid x y) x y))
                        minx maxx miny maxy))
      (hash-for-each (lambda (y row)
                       (hash-for-each (lambda (x cell)
                                        (proc cell x y))
                                      row))
                     (grid-rows grid))))

(define (for-each-item proc grid)
  "Call PROC for each item in GRID."
  (hash-for-each proc (grid-rects grid)))

(define (grid-add grid item x y width height)
  "Add ITEM to GRID represented by axis-aligned bounding box defined
by X, Y, WIDTH, HEIGHT."
  (when (item-in-grid? grid item)
    (error "item already in grid" item))
  (let ((rect (make-rect x y width height)))
    (hashq-set! (grid-rects grid) item rect)
    (for-each-cell (lambda (cell x y)
                     (cell-add cell item))
                   grid rect)))

(define (grid-remove grid item)
  "Remove ITEM from GRID."
  (let ((rect (grid-rect-ref grid item)))
    (unless rect
      (error "item not in grid" item))
    (hashq-remove! (grid-rects grid) item)
    (for-each-cell (lambda (cell x y)
                     (cell-remove cell item))
                   grid rect)))

(define inexact->exact*
  (let ((cache '()))
    (lambda (n)
      (or (assv-ref cache n)
          (let ((result (inexact->exact n)))
            (set! cache (cons (cons n result) cache))
            result)))))

(define (grid-move grid item goal filter)
  "Attempt to move ITEM in GRID to POSITION (a 2D vector) and check
for collisions.  For each collision, FILTER will be called with two
arguments: ITEM and the item it collided with.  If a collision occurs,
POSITION may be modified to resolve the colliding objects."
  (let* ((rect (grid-rect-ref grid item))
         (x (rect-x rect))
         (y (rect-y rect))
         (w (rect-width rect))
         (h (rect-height rect))
         (cell-size (grid-cell-size grid))
         (collisions (grid-buffer grid))
         (visited (grid-visited grid)))
    (define (to-cell n)
      (inexact->exact* (floor (/ n cell-size))))
    (define (collision? rect1 rect2 goal)
      (let ((goal-x (vec2-x goal))
            (goal-y (vec2-y goal)))
        (and (< goal-x (rect-right rect2))
             (> (+ goal-x (rect-width rect1)) (rect-left rect2))
             (< goal-y (rect-top rect2))
             (> (+ goal-y (rect-height rect1)) (rect-bottom rect2)))))
    (define (overlap-area rect1 rect2 goal)
      (let ((goal-x (vec2-x goal))
            (goal-y (vec2-y goal)))
        (* (- (min (+ goal-x (rect-width rect1)) (rect-right rect2))
              (max goal-x (rect-left rect2)))
           (- (min (+ goal-y (rect-height rect1)) (rect-top rect2))
              (max goal-y (rect-bottom rect2))))))
    (define (check other other-rect)
      ;; Since items can occupy multiple cells, we must track which
      ;; items have been processed already so that we don't have
      ;; duplicate collision results which will almost certainly
      ;; yield strange behavior.
      (unless (hashq-ref visited other)
        ;; The user-provided filter is expected to return a procedure
        ;; that can resolve a collision between itself and other,
        ;; should one occur.  If the items should clip through each
        ;; other without any collision, the filter returns #f and we
        ;; do not waste any time testing for collision.
        (let ((resolve (filter item other)))
          (when (and resolve (collision? rect other-rect goal))
            (array-list-push! collisions
                              (list other
                                    (overlap-area rect other-rect goal)
                                    resolve))))))
    (define (sort-by-area)
      ;; This is just an insertion sort, which will be slow if there are a
      ;; large number of simultaneous collisions.  I think that the number
      ;; of simultaneous collisions is almost always a single digit
      ;; number, so a more efficient sorting algorithm doesn't gain us
      ;; anything.
      (define (compare a b)
        (match a
          ((_ area-a _)
           (match b
             ((_ area-b _)
              (< area-a area-b))))))
      (define (swap i j)
        (let ((tmp (array-list-ref collisions i)))
          (array-list-set! collisions i (array-list-ref collisions j))
          (array-list-set! collisions j tmp)))
      (let ((size (array-list-size collisions)))
        (let outer
            ((i 0))
          (when (< i size)
            (let inner ((j (+ i 1)))
              (when (< j size)
                (when (compare (array-list-ref collisions i)
                               (array-list-ref collisions j))
                  (swap i j))
                (inner (+ j 1))))
            (outer (+ i 1))))))
    (define (find-collisions)
      ;; The search area is the bounding box formed by union of the
      ;; current rect and the rect formed by moving it to the desired
      ;; position.
      (let* ((goal-x (vec2-x goal))
             (goal-y (vec2-y goal))
             (search-x (min goal-x x))
             (search-y (min goal-y y))
             (search-w (+ w (min (- goal-x x) 0.0)))
             (search-h (+ h (min (- goal-y y) 0.0)))
             (minx (to-cell x))
             (maxx (to-cell (+ x w)))
             (miny (to-cell y))
             (maxy (to-cell (+ y h))))
        ;; Reset our scratch space.
        (array-list-clear! collisions)
        ;; Visit every cell in the search area.
        (let yloop ((cy miny))
          (when (<= cy maxy)
            (let ((row (grid-row-ref grid cy)))
              (let xloop ((cx minx))
                (when (<= cx maxx)
                  (let ((cell (row-column-ref row cx)))
                    (hash-for-each (lambda (other unused)
                                     (check other (grid-rect-ref grid other)))
                                   (cell-items cell)))
                  (xloop (+ cx 1)))))
            (yloop (+ cy 1))))
        ;; Sort collisions by overlap area and return the biggest
        ;; collision.  There's definitely improvements that can be made in
        ;; the heuristic department here, but it's enough for now.
        (sort-by-area)
        ;; Return the biggest collision.
        (if (array-list-empty? collisions)
            #f
            (array-list-ref collisions 0))))
    (define (collide)
      (match (find-collisions)
        ((other _ resolve)
         (hashq-set! visited other #t)
         (resolve item
                  (grid-rect-ref grid item)
                  other
                  (grid-rect-ref grid other)
                  goal)
         ;; Resolving the collision may have caused an another
         ;; collision, so we must perform the collision test again.
         ;; This loop continues until the item is no longer colliding
         ;; with any other item.
         (collide))
        (#f #f)))
    ;; Reset shared scratch space.
    (hash-clear! visited)
    ;; Never check collision against ourselves.
    (hashq-set! visited item #t)
    (collide)
    (let* ((new-x (vec2-x goal))
           (new-y (vec2-y goal))
           (minx1 (to-cell x))
           (miny1 (to-cell y))
           (maxx1 (to-cell (+ x w)))
           (maxy1 (to-cell (+ y h)))
           (minx2 (to-cell new-x))
           (miny2 (to-cell new-y))
           (maxx2 (to-cell (+ new-x w)))
           (maxy2 (to-cell (+ new-y h))))
      (set-rect-x! rect new-x)
      (set-rect-y! rect new-y)
      (for-each-coord (lambda (x y)
                        (when (or (< x minx2) (> x maxx2)
                                  (< y miny2) (> y maxy2))
                          (cell-remove (grid-cell-ref grid x y) item)))
                      minx1 maxx1 miny1 maxy1)
      (for-each-coord (lambda (x y)
                        (when (or (< x minx1) (> x maxx1)
                                  (< y miny1) (> y maxy1))
                          (cell-add (grid-cell-ref grid x y) item)))
                      minx2 maxx2 miny2 maxy2))))

(define (grid-clear grid)
  "Remove all items from GRID."
  (hash-clear! (grid-rects grid))
  (hash-clear! (grid-rows grid)))

(define (grid-cell-count grid)
  "Return the number of cells in GRID."
  (hash-fold (lambda (y row result)
               (+ result (hash-count (const #t) row)))
             0
             (grid-rows grid)))

(define (grid-item-count grid)
  "Return the number of items in GRID."
  (hash-count (const #t) (grid-rects grid)))
