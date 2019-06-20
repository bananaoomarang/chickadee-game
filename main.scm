(use-modules
 (system repl coop-server)
 (ice-9 pretty-print)
 (chickadee)
 (chickadee math vector)
 (chickadee render sprite)
 (chickadee render texture))

(define repl (spawn-coop-repl-server))

(define window-width 1280)
(define window-height 960)
(define PI 3.14159265359)
(define BIRD-SPEED 10.0)
(define BIRD-ANG-SPEED 5.0)

(define BULLET-SPEED 300)

(define bird-hw 64)
(define bird-hh 64)

(define bird-sprite #f)
(define bullet-sprite #f)
(define asteroid-sprite #f)

(define bullets '())

(define (reset!)
  (set! bird-angular 0.0)
  (set! bird-rotation 0.0)
  (set! bird-acc 0.0)
  (set! bullets '())
  (set-vec2! bird-vel 0.0 0.0)
  (set-vec2! bird-pos 600.0 100.0))

(define (deg->rad d)
  (* (/ PI 180) d))

(define* (create-entity sprite pos dir
                        #:key
                        (origin #v(0 0))
                        (rotation 0)
                        (speed 0)
                        (velocity #v(0 0))
                        (accel 0)
                        (angular-vel 0))
  (list (cons 'sprite sprite)
        (cons 'pos pos)
        (cons 'dir dir)
        (cons 'origin origin)
        (cons 'speed speed)
        (cons 'velocity velocity)
        (cons 'accel accel)
        (cons 'angular-vel angular-vel)
        (cons 'rotation rotation)))

(define (spawn-bullet! pos dir)
  (set! bullets (cons (create-entity bullet-sprite pos dir #:speed BULLET-SPEED) bullets)))

(define (get-direction rad)
  "Get direction vector from radians IDK"

  ;; IDK why the * -1 is necessary sorry! it's just the way it is baby
  (let ((-rad (* -1 rad)))
    #v((sin -rad) (cos -rad))))

(define (update-entity! entity dt)
  (let ((rotation (assoc-ref entity 'rotation))
        (dir (assoc-ref entity 'dir))
        (angular-vel (assoc-ref entity 'angular-vel))
        (velocity (assoc-ref entity 'velocity))
        (accel (assoc-ref entity 'accel))
        (pos (assoc-ref entity 'pos)))

    (assoc-set! entity 'rotation (+ rotation (* dt angular-vel)))
    (vec2-add! velocity (vec2* dir accel))

    (set-vec2-x! pos (+ (vec2-x pos) (* dt (vec2-x velocity))))
    (set-vec2-y! pos (+ (vec2-y pos) (* dt (vec2-y velocity))))))

(define bird #f)

(define (bird-update! dt)
  (assoc-set! bird 'dir (get-direction (assoc-ref bird 'rotation))))

(define (bullets-update! dt)
  (for-each (lambda (bullet)
              (vec2-add! (assoc-ref bullet 'pos) (vec2* (assoc-ref bullet 'dir) (* dt (assoc-ref bullet 'speed)))))
            bullets))

(define (bird-rot! n)
  (assoc-set! bird 'angular-vel (+ n (assoc-ref bird 'angular-vel))))

(define (shoot!)
  (spawn-bullet! (vec2-copy (assoc-ref bird 'pos)) (get-direction (assoc-ref bird 'rotation))))

(define key-press-handlers (make-hash-table))
(hash-set! key-press-handlers 'up (lambda () (assoc-set! bird 'accel BIRD-SPEED)))

(hash-set! key-press-handlers 'left (lambda () (bird-rot! BIRD-ANG-SPEED)))
(hash-set! key-press-handlers 'right (lambda () (bird-rot! (* -1 BIRD-ANG-SPEED))))

(hash-set! key-press-handlers 'space shoot!)

(define key-release-handlers (make-hash-table))
(hash-set! key-release-handlers 'up (lambda () (assoc-set! bird 'accel BIRD-SPEED)))

(hash-set! key-release-handlers 'left (lambda () (bird-rot! (* -1 BIRD-ANG-SPEED))))
(hash-set! key-release-handlers 'right (lambda () (bird-rot! BIRD-ANG-SPEED)))

(define (key-press key scancode modifiers repeat?)
  (let ((handler (hash-ref key-press-handlers key)))
    (if (and handler (not repeat?)) (handler))))

(define (key-release key scancode modifiers)
  (let ((handler (hash-ref key-release-handlers key)))
    (if handler (handler))))

(define (draw-entity entity)
  (draw-sprite
   (assoc-ref entity 'sprite)
   (assoc-ref entity 'pos)
   #:rotation (assoc-ref entity 'rotation)
   #:origin (assoc-ref entity 'origin)))

(define (load)
  (set! bird-sprite (load-image "assets/images/chickadee.png"))
  (set! bullet-sprite (load-image "assets/images/dot.png"))
  (set! bird (create-entity bird-sprite #v(100 100) #v(0 0) #:speed BIRD-SPEED #:origin #v(bird-hw bird-hh))))

(define (game-draw alpha)
  (draw-entity bird)
  (for-each draw-entity bullets))

(define (game-update dt)
  (let ((dt-seconds (/ dt 1000.0)))
    (bullets-update! dt-seconds)
    (update-entity! bird dt-seconds)
    (bird-update! dt)))

(define (draw alpha)
  "Call the game draw function but don't let it crash"

  (catch #t
    (lambda () (game-draw alpha))
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
