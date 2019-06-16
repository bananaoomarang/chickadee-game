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
(define SPRITE-SPEED 10.0)
(define SPRITE-ANG-SPEED 5.0)

(define BULLET-SPEED 300)

(define sprite #f)
(define sprite-hw 64)
(define sprite-hh 64)
(define sprite-angular 0.0)
(define sprite-rotation 0.0)
(define sprite-acc 0.0)
(define sprite-vel #v(0.0 0.0))
(define sprite-pos #v(100.0 100.0))

(define bullet-sprite #f)

(define bullets '())

(define (reset!)
  (set! sprite-angular 0.0)
  (set! sprite-rotation 0.0)
  (set! sprite-acc 0.0)
  (set! bullets '())
  (set-vec2! sprite-vel 0.0 0.0)
  (set-vec2! sprite-pos 600.0 100.0))

(define (deg->rad d)
  (* (/ PI 180) d))

(define (create-bullet pos dir)
  (let ((bullet (make-hash-table)))
    (hash-set! bullet 'pos pos)
    (hash-set! bullet 'dir dir)
    (hash-set! bullet 'decay 10)
    bullet))

(define (spawn-bullet! pos dir)
  (set! bullets (cons (create-bullet pos dir) bullets)))

(define (get-direction rad)
  "Get direction vector from radians IDK"

  ;; IDK why the * -1 is necessary sorry! it's just the way it is baby
  (let ((-rad (* -1 rad)))
    #v((sin -rad) (cos -rad))))

(define (sprite-update! dt)
  (set! sprite-rotation (+ sprite-rotation (* dt sprite-angular)))

  (vec2-add! sprite-vel (vec2* (get-direction sprite-rotation) sprite-acc))

  (set-vec2-x! sprite-pos (+ (vec2-x sprite-pos) (* dt (vec2-x sprite-vel))))
  (set-vec2-y! sprite-pos (+ (vec2-y sprite-pos) (* dt (vec2-y sprite-vel)))))

(define (bullets-update! dt)
  (for-each (lambda (bullet)
              (vec2-add! (hash-ref bullet 'pos) (vec2* (hash-ref bullet 'dir) (* dt BULLET-SPEED))))
            bullets))

(define (sprite-rot! n)
  (set! sprite-angular (+ n sprite-angular)))

(define (shoot!)
  (spawn-bullet! (vec2-copy sprite-pos) (get-direction sprite-rotation)))

(define key-press-handlers (make-hash-table))
(hash-set! key-press-handlers 'up (lambda () (set! sprite-acc SPRITE-SPEED)))

(hash-set! key-press-handlers 'left (lambda () (sprite-rot! SPRITE-ANG-SPEED)))
(hash-set! key-press-handlers 'right (lambda () (sprite-rot! (* -1 SPRITE-ANG-SPEED))))

(hash-set! key-press-handlers 'space shoot!)

(define key-release-handlers (make-hash-table))
(hash-set! key-release-handlers 'up (lambda () (set! sprite-acc 0.0)))

(hash-set! key-release-handlers 'left (lambda () (sprite-rot! (* -1 SPRITE-ANG-SPEED))))
(hash-set! key-release-handlers 'right (lambda () (sprite-rot! SPRITE-ANG-SPEED)))

(define (key-press key scancode modifiers repeat?)
  (let ((handler (hash-ref key-press-handlers key)))
    (if (and handler (not repeat?)) (handler))))

(define (key-release key scancode modifiers)
  (let ((handler (hash-ref key-release-handlers key)))
    (if handler (handler))))

(define (load)
  (set! sprite (load-image "assets/images/chickadee.png"))
  (set! bullet-sprite (load-image "assets/images/dot.png")))

(define (game-draw alpha)
  (for-each (lambda (bullet)
              (draw-sprite bullet-sprite (hash-ref bullet 'pos ))) bullets)
  (draw-sprite sprite sprite-pos #:rotation sprite-rotation #:origin #v(sprite-hw sprite-hh)))

(define (game-update dt)
  (let ((dt-seconds (/ dt 1000.0)))
    (bullets-update! dt-seconds)
    (sprite-update! dt-seconds)))

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
