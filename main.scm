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

(define bird #f)
(define bird-hw 64)
(define bird-hh 64)
(define bird-angular 0.0)
(define bird-rotation 0.0)
(define bird-acc 0.0)
(define bird-vel #v(0.0 0.0))
(define bird-pos #v(100.0 100.0))

(define bullet-bird #f)

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

(define (create-entity bird pos dir)
  (let ((entity (make-hash-table)))
    (hash-set! entity 'bird bird)
    (hash-set! entity 'pos pos)
    (hash-set! entity 'dir dir)
    entity))

(define (spawn-bullet! pos dir)
  (set! bullets (cons (create-entity bullet-bird pos dir) bullets)))

(define (get-direction rad)
  "Get direction vector from radians IDK"

  ;; IDK why the * -1 is necessary sorry! it's just the way it is baby
  (let ((-rad (* -1 rad)))
    #v((sin -rad) (cos -rad))))

(define (bird-update! dt)
  (set! bird-rotation (+ bird-rotation (* dt bird-angular)))

  (vec2-add! bird-vel (vec2* (get-direction bird-rotation) bird-acc))

  (set-vec2-x! bird-pos (+ (vec2-x bird-pos) (* dt (vec2-x bird-vel))))
  (set-vec2-y! bird-pos (+ (vec2-y bird-pos) (* dt (vec2-y bird-vel)))))

(define (bullets-update! dt)
  (for-each (lambda (bullet)
              (vec2-add! (hash-ref bullet 'pos) (vec2* (hash-ref bullet 'dir) (* dt BULLET-SPEED))))
            bullets))

(define (bird-rot! n)
  (set! bird-angular (+ n bird-angular)))

(define (shoot!)
  (spawn-bullet! (vec2-copy bird-pos) (get-direction bird-rotation)))

(define key-press-handlers (make-hash-table))
(hash-set! key-press-handlers 'up (lambda () (set! bird-acc BIRD-SPEED)))

(hash-set! key-press-handlers 'left (lambda () (bird-rot! BIRD-ANG-SPEED)))
(hash-set! key-press-handlers 'right (lambda () (bird-rot! (* -1 BIRD-ANG-SPEED))))

(hash-set! key-press-handlers 'space shoot!)

(define key-release-handlers (make-hash-table))
(hash-set! key-release-handlers 'up (lambda () (set! bird-acc 0.0)))

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
   (hash-ref entity 'bird)
   (hash-ref entity 'pos)))

(define (load)
  (set! bird (load-image "assets/images/chickadee.png"))
  (set! bullet-bird (load-image "assets/images/dot.png")))

(define (game-draw alpha)
  (for-each draw-entity bullets)
  (draw-sprite bird bird-pos #:rotation bird-rotation #:origin #v(bird-hw bird-hh)))

(define (game-update dt)
  (let ((dt-seconds (/ dt 1000.0)))
    (bullets-update! dt-seconds)
    (bird-update! dt-seconds)))

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
