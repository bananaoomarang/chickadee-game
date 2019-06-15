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
(define SPRITE-SPEED 200.0)

(define sprite #f)
(define sprite-vel #v(0.0 0.0))
(define sprite-pos #v(100.0 100.0))

(define (sprite-reset!)
  (set-vec2! sprite-vel 0.0 0.0)
  (set-vec2! sprite-pos 100.0 100.0))

(define (sprite-update! dt)
  (set-vec2-x! sprite-pos (+ (vec2-x sprite-pos) (* dt (vec2-x sprite-vel))))
  (set-vec2-y! sprite-pos (+ (vec2-y sprite-pos) (* dt (vec2-y sprite-vel)))))


(define (sprite-add-x n)
  (set-vec2-x! sprite-vel (+ n (vec2-x sprite-vel))))
(define (sprite-add-y n)
  (set-vec2-y! sprite-vel (+ n (vec2-y sprite-vel))))

(define key-press-handlers (make-hash-table))
(hash-set! key-press-handlers 'up (lambda () (sprite-add-y SPRITE-SPEED)))
(hash-set! key-press-handlers 'down (lambda () (sprite-add-y (* -1 SPRITE-SPEED))))
(hash-set! key-press-handlers 'left (lambda () (sprite-add-x (* -1 SPRITE-SPEED))))
(hash-set! key-press-handlers 'right (lambda () (sprite-add-x SPRITE-SPEED)))

(define key-release-handlers (make-hash-table))
(hash-set! key-release-handlers 'up (lambda () (sprite-add-y (* -1 SPRITE-SPEED))))
(hash-set! key-release-handlers 'down (lambda () (sprite-add-y SPRITE-SPEED)))
(hash-set! key-release-handlers 'left (lambda () (sprite-add-x SPRITE-SPEED)))
(hash-set! key-release-handlers 'right (lambda () (sprite-add-x (* -1 SPRITE-SPEED))))

(define (key-press key scancode modifiers repeat?)
  (let ((handler (hash-ref key-press-handlers key)))
    (if (and handler (not repeat?)) (handler))))

(define (key-release key scancode modifiers)
  (let ((handler (hash-ref key-release-handlers key)))
    (if handler (handler))))

(define (load)
  (set! sprite (load-image "assets/images/chickadee.png")))

(define (draw alpha)
  (draw-sprite sprite sprite-pos))

(define (game-update dt)
  (let ((dt-seconds (/ dt 1000.0)))
    (sprite-update! dt-seconds)))

(define (update dt)
  "Call the game update but keep calling the repl poll if it breaks"
  
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
