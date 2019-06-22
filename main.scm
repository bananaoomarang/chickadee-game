(use-modules
 (system repl coop-server)
 (ice-9 pretty-print)
 (chickadee)
 (chickadee math vector)
 (chickadee math rect)
 (chickadee render)
 (chickadee render sprite)
 (chickadee render texture)
 (chickadee render font)
 (chickadee render color)
 (chickadee render viewport)
 (chickadee scripting))

(define repl (spawn-coop-repl-server))

;;(define window-width 1280)
;;(define window-height 960)
(define window-width 800)
(define window-height 600)
(define PI 3.14159265359)
(define BIRD-SPEED 10.0)
(define BIRD-ANG-SPEED 5.0)

(define BULLET-SPEED 300)

(define MAX-ASTEROID-SPEED 200)

(define bird-hw 64)
(define bird-hh 64)

(define bird-sprite #f)
(define bullet-sprite #f)
(define asteroid-sprite #f)

(define bird #f)
(define bullets '())
(define asteroids '())
(define score 0)


(define black (make-color 0.0 0.0 0.0 1.0))
(define main-viewport
  (make-viewport 0 0 window-width window-height #:clear-color black))

(define (reset!)
  (assoc-set! bird 'angular-vel 0.0)
  (assoc-set! bird 'rotation 0.0)
  (assoc-set! bird 'accel 0.0)
  (set! bullets '())
  (set! asteroids '())
  (set-vec2! (assoc-ref bird 'velocity) 0.0 0.0)
  (set-vec2! (assoc-ref bird 'pos) 600.0 100.0))

(define (deg->rad d)
  (* (/ PI 180) d))

(define* (create-entity sprite pos
                        #:key
                        (origin #v(0 0))
                        (rotation 0)
                        (speed 0)
                        (velocity #v(0 0))
                        (accel #v(0 0))
                        (angular-vel 0)
                        (scale #v(1 1)))
  (list (cons 'sprite sprite)
        (cons 'pos pos)
        (cons 'origin origin)
        (cons 'speed speed)
        (cons 'velocity velocity)
        (cons 'accel accel)
        (cons 'angular-vel angular-vel)
        (cons 'rotation rotation)
        (cons 'scale scale)
        (cons 'aabb (rect (vec2-x pos) (vec2-y pos)
                          (texture-width sprite) (texture-height sprite)))))

(define (collides? entity-a entity-b)
  (rect-intersects?
   (assoc-ref entity-a 'aabb)
   (assoc-ref entity-b 'aabb)))

(define (spawn-bullet! pos dir)
  (set! bullets
    (cons
     (create-entity bullet-sprite pos #:velocity (vec2* dir BULLET-SPEED)) bullets)))

(define (spawn-asteroid!)
  (let ((pos (vec2 (random window-width) (random window-height)))
        (vel (vec2
              (- (random MAX-ASTEROID-SPEED) (random (* 2 MAX-ASTEROID-SPEED)))
              (- (random MAX-ASTEROID-SPEED) (random (* 2 MAX-ASTEROID-SPEED))))))
    (set! asteroids
      (cons
       (create-entity asteroid-sprite pos #:velocity vel) asteroids))))

(define (get-direction rad)
  "Get direction vector from radians IDK"

  ;; IDK why the * -1 is necessary sorry! it's just the way it is baby
  (let ((-rad (* -1 rad)))
    #v((sin -rad) (cos -rad))))

(define (update-entity! entity dt)
  (let ((rotation (assoc-ref entity 'rotation))
        (angular-vel (assoc-ref entity 'angular-vel))
        (velocity (assoc-ref entity 'velocity))
        (accel (assoc-ref entity 'accel))
        (pos (assoc-ref entity 'pos))
        (aabb (assoc-ref entity 'aabb)))

    (if (< (vec2-x pos) 0) (set-vec2-x! pos window-width))
    (if (< (vec2-y pos) 0) (set-vec2-y! pos window-height))

    (if (> (vec2-x pos) window-width) (set-vec2-x! pos 0))
    (if (> (vec2-y pos) window-height) (set-vec2-y! pos 0))

    (assoc-set! entity 'rotation (+ rotation (* dt angular-vel)))

    (vec2-add! velocity accel)

    (set-vec2-x! pos (+ (vec2-x pos) (* dt (vec2-x velocity))))
    (set-vec2-y! pos (+ (vec2-y pos) (* dt (vec2-y velocity))))

    (set-rect-x! aabb (vec2-x pos))
    (set-rect-y! aabb (vec2-y pos))))

(define (bird-rot! n)
  (let ((angular-vel (assoc-ref bird 'angular-vel)))
        (if (<= (abs (+ n angular-vel)) BIRD-ANG-SPEED)
            (assoc-set! bird 'angular-vel (+ n angular-vel))
            (pretty-print angular-vel))))

(define (shoot!)
  (spawn-bullet! (vec2-copy (assoc-ref bird 'pos)) (get-direction (assoc-ref bird 'rotation))))

(define key-press-handlers (make-hash-table))
(hash-set! key-press-handlers 'space shoot!)

(define key-state (list
                   (cons 'space #f)
                   (cons 'up #f)
                   (cons 'left #f)
                   (cons 'right #f)))

(define (key-press key scancode modifiers repeat?)
  (assoc-set! key-state key #t))

(define (key-release key scancode modifiers)
  (assoc-set! key-state key #f))

(define (handle-move-bird! key-state)
  "Move the bird if they click the arrows!"

  (let ((left?  (assoc-ref key-state 'left))
        (right? (assoc-ref key-state 'right))
        (up?    (assoc-ref key-state 'up)))
    (if up?
        (assoc-set! bird 'accel (vec2*
                                 (get-direction (assoc-ref bird 'rotation))
                                 BIRD-SPEED))
        (assoc-set! bird 'accel 0))

    (cond
     ((and left? right?) (assoc-set! bird 'angular-vel 0))
     ((and (not left?) (not right?)) (assoc-set! bird 'angular-vel 0))
     (left? (assoc-set! bird 'angular-vel BIRD-ANG-SPEED))
     (right? (assoc-set! bird 'angular-vel (* -1 BIRD-ANG-SPEED))))))

(define shot #f)
(define (handle-shoot! key-state)
  "Shoot if they tap space"

  (let ((space? (assoc-ref key-state 'space)))
    (if (and space? (not shot))
        (begin
          (shoot!)
          (set! shot #t)))

    (if (not space?)
        (set! shot #f))))

(define (update-bullet! bullet dt)
  (for-each
   (lambda (asteroid)
     (if (collides? bullet asteroid)
         (begin
           (set! bullets (delete bullet bullets))
           (set! asteroids (delete asteroid asteroids))
           (set! score (+ score 1)))))
   asteroids)
  
  (update-entity! bullet dt))

;; Lambdas are for hot reloading...
(define key-handlers
  (list
   (lambda (s) (handle-move-bird! s))
   (lambda (s) (handle-shoot! s))))

(define (draw-entity entity)
  (draw-sprite
   (assoc-ref entity 'sprite)
   (assoc-ref entity 'pos)
   #:rotation (assoc-ref entity 'rotation)
   #:origin (assoc-ref entity 'origin)
   #:scale (assoc-ref entity 'scale)))

(define (load)
  (set! bird-sprite (load-image "assets/images/chickadee.png"))
  (set! bullet-sprite (load-image "assets/images/dot.png"))
  (set! asteroid-sprite (load-image "assets/images/asteroid.png"))
  (set! bird (create-entity bird-sprite #v(100 100)
                            #:speed BIRD-SPEED
                            #:origin #v(bird-hw bird-hh)
                            #:scale #v(0.75 0.75))))

(define (game-draw alpha)
  (draw-entity bird)
  (for-each draw-entity bullets)
  (for-each draw-entity asteroids)
  (draw-text (string-append "Score: " (number->string score)) #v(10 10) #:scale #v(2 2)))

(define (game-update dt)
  (let ((dt-seconds (/ dt 1000.0)))
    (update-agenda dt)

    (for-each (lambda (h) (h key-state)) key-handlers )
    (for-each (lambda (bullet) (update-bullet! bullet dt-seconds)) bullets)
    (for-each (lambda (asteroid) (update-entity! asteroid dt-seconds)) asteroids)
    (update-entity! bird dt-seconds)))

(define (draw alpha)
  "Call the game draw function but don't let it crash"

  (catch #t
    (lambda ()
      (with-viewport main-viewport 
                     (game-draw alpha)))
    (lambda (key . parameters)
      (pretty-print key)
      (pretty-print parameters))))

(define (update dt)
  "Call the game update function but keep calling the repl poll if it breaks"
  
  (catch #t
    (lambda () (game-update dt))
    (lambda (key . parameters)
      (pretty-print key)
      (pretty-print parameters)))

  (poll-coop-repl-server repl))

(define (start!)
  (run-game
   #:window-width window-width
   #:window-height window-height
   #:key-press key-press
   #:key-release key-release
   #:load load
   #:update update
   #:draw draw))

(define (stop!)
  (abort-game))

(start!)
(reset!)

(define spawn-asteroid-script
  (script
   (while #t
     (spawn-asteroid!)
     (sleep 5000))))

(cancel-script spawn-asteroid-script)
