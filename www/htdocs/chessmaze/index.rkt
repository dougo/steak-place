#lang planet dyoo/whalesong

#|
chessmaze/index.rkt - Mark J. P. Wolf's chess maze as a Whalesong program.
Copyright © 2011 by Doug Orleans.  This program is free software,
under the terms of the GNU Affero General Public License version 3.
http://www.gnu.org/licenses/
|#

(require (planet dyoo/whalesong/js)
         (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

(define-resource chessmaze.html)

(define (cadr x) (car (cdr x)))

;; TO DO: currently xexp can't include symbols, only strings.
(define nbsp "\ua0")

(define *start-board*
  '((♕ ♜ - ♙ - ♖ ♖ ♛)
    (♙ ♝ - ♞ ♝ - ♝ -)
    (♞ - ♖ - - - - ♞)
    (- - ♘ ♟ ♘ - ♘ ♗)
    (♖ ♖ - ♞ ♜ ♞ - ♞)
    (- - ♘ ♗ - ♜ - ♟)
    (- ♖ ♕ - - - ♖ -)
    (♟ ♜ - ♝ - ♘ ♝ ♔)))

(define *pieces*
  '((♔ ♚) (♕ ♛) (♖ ♜) (♗ ♝) (♘ ♞) (♙ ♟)))

(define (white? piece)
  (assq piece *pieces*))

(define (invert piece)
  (cadr (assq piece *pieces*)))

(define blank '-)

(define (blank? piece)
  (eq? piece blank))

(define (color piece)
  (cond ((blank? piece) 'blank)
        ((white? piece) 'white)
        (else           'black)))

(define (opposite-colors? p1 p2)
  (and (not (blank? p1)) (not (blank? p2))
       (not (eq? (color p1) (color p2)))))

(define (piece-at board r c)
  (list-ref (list-ref board r) c))

(define (remove-piece board r c)
  (let*-values (((above below) (split-at board r))
                ((left right) (split-at (car below) c)))
    (let ((new-row (append left (list blank) (cdr right))))
      (append above (list new-row) (cdr below)))))


;; A maze is a board plus a cursor row and column.
(define-struct maze (board row col))

(define *start-maze* (make-maze *start-board* 0 0))

(define (cursor-at? maze r c)
  (and (= (maze-row maze) r) (= (maze-col maze) c)))

(define (valid-move? maze to-r to-c)
  (let ((board (maze-board maze))
        (from-r (maze-row maze))
        (from-c (maze-col maze)))
    (define (path-clear?)
      (let ((sgn-r (sgn (- to-r from-r)))
            (sgn-c (sgn (- to-c from-c))))
        (let loop ((r (+ from-r sgn-r))
                   (c (+ from-c sgn-c)))
          (or (and (= r to-r) (= c to-c))
              (and (blank? (piece-at board r c))
                   (loop (+ r sgn-r)
                         (+ c sgn-c)))))))
    (let ((from (piece-at board from-r from-c))
          (to (piece-at board to-r to-c)))
      (and (opposite-colors? from to)
           (let ((Δr (abs (- from-r to-r)))
                 (Δc (abs (- from-c to-c))))
             (case from
               ((♔ ♚) (and (<= Δr 1) (<= Δc 1)))
               ((♕ ♛) (and (or (= Δr Δc) (= (* Δr Δc) 0)) (path-clear?)))
               ((♖ ♜) (and (= (* Δr Δc) 0) (path-clear?)))
               ((♗ ♝) (and (= Δr Δc) (path-clear?)))
               ((♘ ♞) (= (* Δr Δc) 2))
               ((♙ ♟) (= Δr Δc 1))))))))

(define (maze-click maze r c)
  (make-maze (remove-piece (maze-board maze) (maze-row maze) (maze-col maze))
             r c))

(define (maze->view maze parent)

  (define (board->view board parent)
    (let loop ((rows board) (r 0)
               (view (view-append-child
                      parent (xexp->dom `(div (@ (class "board")))))))
      (if (null? rows)
          (view-up (view-append-child view (xexp->dom bottom-border-xexp)))
          (let ((row (car rows)))
            (loop (cdr rows) (+ r 1)
                  (view-up
                   (row->view row r view)))))))

  (define (row->view row r parent)
    (let loop ((pieces row) (c 0)
               (view (view-append-child
                      parent (xexp->dom `(div
                                          (span (@ (class "border"))
                                                ,(number->string (- 8 r))))))))
      (if (null? pieces)
          view
          (let ((piece (car pieces)))
            (loop (cdr pieces) (+ c 1)
                  (view-up
                   (piece->view piece r c view)))))))

  (define (piece->view piece r c parent)
    (let* ((classes
            (string-append
             (symbol->string (color piece))
             (if (odd? (+ r c)) " black-square" " white-square")
             (if (cursor-at? maze r c) " cursor" "")))
           (x (piece->xexpr piece)))
      (if (valid-move? maze r c)
          (view-bind
           (view-append-child
            parent (xexp->dom `(a (@ (class ,classes) (href "#")) ,x)))
           "click" (lambda (maze view event) (maze-click maze r c)))
          (view-append-child
           parent (xexp->dom `(span (@ (class ,classes)) ,x))))))

  (board->view (maze-board maze) parent))

;; Sadly, using the actual white-piece glyph makes it look like the
;; background color (i.e. the color of the square) rather than white.
;; So we use black-piece glyphs for everything and change the
;; foreground color to white for a white piece.
(define (piece->xexpr piece)
  (if (blank? piece)
      nbsp
      (symbol->string
       (if (white? piece)
           (invert piece)
           piece))))

(define bottom-border-xexp
  `(div
    ,@(let loop ((elts `(,nbsp "a" "b" "c" "d" "e" "f" "g" "h")))
        (if (null? elts)
            null
            (cons `(span (@ (class "border")) ,(car elts))
                  (loop (cdr elts)))))))

(define (draw maze view)
  (maze->view maze (view-remove
                    (view-down
                     (view-focus view "maze-container")))))

(big-bang *start-maze*
          (initial-view
           (view-bind (view-focus (->view chessmaze.html) "restart")
                      "click" (lambda (maze view event) *start-maze*)))
          (to-draw draw))
