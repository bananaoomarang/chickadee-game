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
;; Bitmap font rendering.
;;
;;; Code:

(define-module (chickadee render font)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (sxml xpath)
  #:use-module (sxml simple)
  #:use-module (chickadee config)
  #:use-module (chickadee math matrix)
  #:use-module (chickadee math rect)
  #:use-module (chickadee math vector)
  #:use-module (chickadee render)
  #:use-module (chickadee render shader)
  #:use-module (chickadee render sprite)
  #:use-module (chickadee render texture)
  #:export (load-font
            load-tile-font
            font?
            font-face
            font-line-height
            font-line-width
            font-bold?
            font-italic?
            default-font
            draw-text*
            draw-text))

(define-record-type <font-char>
  (make-font-char id texture-region offset dimensions advance)
  font-char?
  (id font-char-id)
  (texture-region font-char-texture-region)
  (offset font-char-offset)
  (dimensions font-char-dimensions)
  (advance font-char-advance))

(define-record-type <font>
  (make-font face bold? italic? line-height chars kerning sprite-batches)
  font?
  (face font-face)
  (bold? font-bold?)
  (italic? font-italic?)
  (line-height font-line-height)
  (chars font-chars)
  (kerning font-kerning)
  (sprite-batches font-sprite-batches))

(define (display-font font port)
  (format port "#<font face: ~a line-height: ~d bold?: ~a italic?: ~a>"
          (font-face font)
          (font-line-height font)
          (font-bold? font)
          (font-italic? font)))

(set-record-type-printer! <font> display-font)

(define (font-line-width font text)
  "Return the width of TEXT when rendered with FONT."
  (let loop ((width 0.0)
             (i 0))
    (if (< i (string-length text))
        (let ((char (or (font-ref font (string-ref text i))
                        (font-ref font #\?))))
          (loop (+ width (vec2-x (font-char-advance char)))
                (+ i 1)))
        width)))

(define* (load-tile-font file tile-width tile-height characters #:key
                         (face "untitled") (margin 0) (spacing 0))
  "Load the font named FACE from FILE, a bitmap image containing the
characters in the string CHARACTERS that are TILE-WIDTH by TILE-HEIGHT
pixels in size.  The characters in the image *must* appear in the
order that they are specified in the character set or text will not
render properly.  Optionally, each tile may have SPACING pixels of
horizontal and vertical space between surrounding tiles and the entire
image may have MARGIN pixels of empty space around its border."
  (let* ((texture (load-image file))
         (atlas (split-texture texture tile-width tile-height
                               #:margin margin
                               #:spacing spacing))
         (chars
          (let ((table (make-hash-table)))
            (string-for-each-index
             (lambda (i)
               (hash-set! table (string-ref characters i)
                          (make-font-char (string-ref characters i)
                                          (texture-atlas-ref atlas i)
                                          (vec2 0.0 0.0)
                                          (vec2 tile-width tile-height)
                                          (vec2 tile-width 0.0))))
             characters)
            table))
         ;; These fonts are by definition monospace fonts, so no
         ;; kerning.
         (kernings (make-hash-table))
         (batches (make-hash-table)))
    (hashq-set! batches texture (make-sprite-batch texture))
    (make-font face #f #f tile-height chars kernings batches)))

(define (load-font file)
  "Load the AngelCode formatted bitmap font within FILE.  The file
extension must be either .xml or .fnt."
  (cond
   ((string-suffix? ".xml" file)
    (parse-bmfont-sxml file (call-with-input-file file xml->sxml)))
   ((string-suffix? ".fnt" file)
    (parse-bmfont-sxml file (parse-fnt file)))
   (else
    (error "unknown bmfont file type: " file))))

(define (parse-fnt file)
  (define (newline? char)
    (eqv? char #\newline))
  (define (whitespace? char)
    (and (not (newline? char))
         (char-set-contains? char-set:whitespace char)))
  (define (letter? char)
    (char-set-contains? char-set:letter char))
  (define (consume-whitespace port)
    (match (peek-char port)
      ((? eof-object?) *unspecified*)
      ((? whitespace?)
       (read-char port)
       (consume-whitespace port))
      (_ *unspecified*)))
  (define (read-tag port)
    (list->symbol
     (let loop ()
       (match (peek-char port)
         ((? letter? char)
          (read-char port)
          (cons char (loop)))
         ((? whitespace? char)
          '())))))
  (define (read-key port)
    (list->symbol
     (let loop ()
       (match (read-char port)
         (#\= '())
         ((? letter? char)
          (cons char (loop)))))))
  (define (read-quoted-string port)
    (match (read-char port)
      (#\" #t))
    (list->string
     (let loop ()
       (match (read-char port)
         (#\"
          (if (or (whitespace? (peek-char port))
                  (newline? (peek-char port)))
              '()
              (cons #\" (loop))))
         (char (cons char (loop)))))))
  (define (read-unquoted-string port)
    (list->string
     (let loop ()
       (match (peek-char port)
         ((or (? whitespace?)
              (? newline?))
          '())
         (char
          (read-char port)
          (cons char (loop)))))))
  (define (read-value port)
    (match (peek-char port)
      (#\"
       (read-quoted-string port))
      (_ (read-unquoted-string port))))
  (define (read-key/value-pair port)
    (list (read-key port) (read-value port)))
  (define (read-key/value-pairs port)
    (cons '@
          (let loop ()
            (consume-whitespace port)
            (match (peek-char port)
              ((? newline?)
               (read-char port)
               '())
              ((? letter?)
               (cons (read-key/value-pair port)
                     (loop)))))))
  (define (read-line port)
    (list (read-tag port) (read-key/value-pairs port)))
  `(*TOP*
    (font
     ,@(call-with-input-file file
         (lambda (port)
           (let loop ((pages '()))
             (match (peek-char port)
               ((? eof-object?)
                `((pages (@ (count ,(number->string (length pages))))
                         ,@pages)))
               ((? newline?)
                (read-char port)
                (loop pages))
               ((? letter?)
                (match (read-line port)
                  ((tag ('@ ('count count)))
                   (cons (cons* tag
                                `(@ (count ,count))
                                (list-tabulate (string->number count)
                                               (lambda (i)
                                                 (read-line port))))
                         (loop pages)))
                  ((and ('page . _) page)
                   (loop (cons page pages)))
                  (exp (cons exp (loop pages))))))))))))

(define (parse-bmfont-sxml file tree)
  (define directory (dirname file))
  (define* (attr tree name #:optional (parse identity))
    (let ((result ((sxpath `(@ ,name *text*)) tree)))
      (if (null? result)
          #f
          (parse (car result)))))
  (define (parse-pages nodes)
    (let ((table (make-hash-table)))
      (for-each (lambda (node)
                  (let* ((id (attr node 'id string->number))
                         (file (attr node 'file))
                         (texture (load-image
                                   (string-append directory "/" file))))
                    (hash-set! table id texture)))
                nodes)
      table))
  (define (string->character s)
    (integer->char (string->number s)))
  (define (parse-chars nodes pages image-width image-height line-height)
    (define (x->s x)
      (exact->inexact (/ x image-width)))
    (define (y->t y)
      (exact->inexact (/ y image-height)))
    (let ((table (make-hash-table)))
      (for-each (lambda (node)
                  (let* ((id (attr node 'id string->character))
                         (width (attr node 'width string->number))
                         (height (attr node 'height string->number))
                         (x (attr node 'x string->number))
                         (y (attr node 'y string->number))
                         (x-offset (attr node 'xoffset string->number))
                         (y-offset (- line-height height
                                      (attr node 'yoffset string->number)))
                         (x-advance (attr node 'xadvance string->number))
                         (page (or (attr node 'page string->number) 0))
                         (region (make-texture-region (hash-ref pages page)
                                                      (make-rect x y width height)))
                         (char (make-font-char id
                                               region
                                               (vec2 x-offset y-offset)
                                               (vec2 width height)
                                               (vec2 x-advance 0.0))))
                    (hash-set! table id char)))
                nodes)
      table))
  (define (parse-kernings nodes)
    (let ((table (make-hash-table)))
      (for-each (lambda (node)
                  (let* ((first (attr node 'first string->character))
                         (second (attr node 'second string->character))
                         (x-offset (attr node 'amount string->number))
                         (inner-table (hash-ref table first)))
                    (if inner-table
                        (hash-set! inner-table second (vec2 x-offset 0.0))
                        (let ((inner-table (make-hash-table)))
                          (hash-set! inner-table second (vec2 x-offset 0.0))
                          (hash-set! table first inner-table)))))
                nodes)
      table))
  (let* ((info ((sxpath '(font info)) tree))
         (common ((sxpath '(font common)) tree))
         (face (attr info 'face))
         (bold? (attr info 'bold (const #t)))
         (italic? (attr info 'italic (const #t)))
         (line-height (attr common 'lineHeight string->number))
         (image-width (attr common 'scaleW string->number))
         (image-height (attr common 'scaleH string->number))
         (pages (parse-pages ((sxpath '(font pages page)) tree)))
         (chars (parse-chars ((sxpath '(font chars char)) tree)
                             pages
                             image-width
                             image-height
                             line-height))
         (kernings (parse-kernings ((sxpath '(font kernings kerning)) tree)))
         (batches (make-hash-table)))
    (hash-for-each (lambda (id texture)
                     (hashq-set! batches texture (make-sprite-batch texture)))
                   pages)
    (make-font face bold? italic? line-height chars kernings batches)))

(define (font-ref font char)
  (hashv-ref (font-chars font) char))

(define draw-text*
  (let ((cursor (vec2 0.0 0.0))
        (rect (make-rect 0.0 0.0 0.0 0.0)))
    (lambda* (font text matrix #:key (blend-mode 'alpha)
                   (start 0) (end (string-length text)))
      (let ((batches (font-sprite-batches font)))
        ;; TODO: Respect kerning.
        (define (render-char c)
          (if (eqv? c #\newline)
              (begin
                (set-vec2-x! cursor 0.0)
                (set-vec2-y! cursor (- (vec2-y cursor) (font-line-height font))))
              ;; TODO: What if "?" isn't in the font?
              (let* ((char (or (font-ref font c) (font-ref font #\?)))
                     (texture (font-char-texture-region char))
                     (batch (hashq-ref batches (texture-parent texture)))
                     (dimensions (font-char-dimensions char))
                     (offset (font-char-offset char)))
                (set-rect-x! rect (+ (vec2-x cursor) (vec2-x offset)))
                (set-rect-y! rect (+ (vec2-y cursor) (vec2-y offset)))
                (set-rect-width! rect (vec2-x dimensions))
                (set-rect-height! rect (vec2-y dimensions))
                (sprite-batch-add* batch rect matrix
                                   #:texture-region texture)
                ;; Move forward to where the next character needs to be drawn.
                (set-vec2-x! cursor
                             (+ (vec2-x cursor)
                                (vec2-x
                                 (font-char-advance char)))))))
        (set-vec2! cursor 0.0 0.0)
        (hash-for-each (lambda (texture batch)
                         (sprite-batch-clear! batch))
                       batches)
        (string-for-each render-char text start end)
        (hash-for-each (lambda (texture batch)
                         (draw-sprite-batch batch #:blend-mode blend-mode))
                       batches)))))

(define %default-scale (vec2 1.0 1.0))
(define %null-vec2 (vec2 0.0 0.0))

(define default-font
  (let ((font (delay (load-font (scope-datadir "fonts/good-neighbors.fnt")))))
    (lambda ()
      (force font))))

(define draw-text
  (let ((matrix (make-null-matrix4)))
    (lambda* (text
              position
              #:key
              (font (default-font))
              (origin %null-vec2)
              (rotation 0)
              (scale %default-scale)
              (blend-mode 'alpha)
              (start 0)
              (end (string-length text)))
      "Draw the string TEXT with the first character starting at
POSITION using FONT."
      (matrix4-2d-transform! matrix
                             #:origin origin
                             #:position position
                             #:rotation rotation
                             #:scale scale)
      (draw-text* font text matrix #:blend-mode blend-mode
                  #:start start #:end end))))
