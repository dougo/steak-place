#lang web-server

#|
chessmaze.rkt - Mark J. P. Wolf's chess maze as a Racket servlet.
Copyright © 2011 by Doug Orleans.  This program is free software,
under the terms of the GNU Affero General Public License version 3.
http://www.gnu.org/licenses/
|#

(provide interface-version start)
(define interface-version 'stateless)

(define (start request)
  (make-page *start-maze* 0 0))

(define rules #<<END
Starting with the White Queen at a8, capture your way to the White
King at h1.  Pieces capture as they do in chess (except pawns, which
can capture one square diagonally in any direction), and pieces can
only capture pieces of the opposite color.  However, once a capture is
made your piece becomes the type of piece that was just captured (and
moves accordingly on the next move) and all moves must end in a
capture.  Pieces that are captured are removed from the board, so the
number of pieces on the board gradually decreases.  (For an easy
warm-up, try capturing the black queen in five moves, or the black
pawn at a1 in nine moves).
END
)

(define *start-maze*
  '((♕ ♜ - ♙ - ♖ ♖ ♛)
    (♙ ♝ - ♞ ♝ - ♝ -)
    (♞ - ♖ - - - - ♞)
    (- - ♘ ♟ ♘ - ♘ ♗)
    (♖ ♖ - ♞ ♜ ♞ - ♞)
    (- - ♘ ♗ - ♜ - ♟)
    (- ♖ ♕ - - - ♖ -)
    (♟ ♜ - ♝ - ♘ ♝ ♔)))

;;; Make a page from a board and the row and column indices of the cursor.
(define (make-page board r c)
  (send/suspend/dispatch
   (λ (k-url)
      (response/xexpr
       `(html
         (head
          (title "Chess Maze")
          (link
           ((rel "stylesheet") (href "/chess/chess.css") (type "text/css"))))
         (body
          (h1 (hr) "Chess Maze")
          (p "by "
             (a ((href "http://www.cuw.edu/fs/markwolf")) "Mark J. P. Wolf")
             ".  Originally appeared at "
             (a ((href "http://mathpuzzle.com")) "Mathpuzzle.com")
             " on "
             (a ((href "http://www.mathpuzzle.com/22Jan04.html"))
                "January 22, 2004")
             ".")
          (p ,rules)
          ,(board->xexpr board r c k-url)
          (p (a ((href "chessmaze.rkt")) "Restart"))
          (p (a ((href "/src/chessmaze.rkt")) "View Racket source code")) 
          (p (a ((href "/chessmaze/")) "Switch to Whalesong version"))
          (hr)
          (address (a ((href "/dougo/")) "Doug Orleans") " "
                   (a ((href "mailto:dougorleans@gmail.com"))
                      "<dougorleans@gmail.com>"))))))))

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

;; Exclusive range sequence, but infinite if x = y.
(define (in-between x y)
  (cond ((< x y) (in-range (+ x 1) y))
        ((= x y) (in-cycle (in-value x)))
        ((> x y) (in-range (- x 1) y -1))))

(define (valid-move? board from-r from-c to-r to-c)
  (define (path-clear?)
    (for/and ((r (in-between from-r to-r))
              (c (in-between from-c to-c)))
      (blank? (piece-at board r c))))
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
             ((♙ ♟) (= Δr Δc 1)))))))

(define (remove-piece board r c)
  (let*-values (((above below) (split-at board r))
                ((left right) (split-at (car below) c)))
    (let ((new-row (append left (list blank) (cdr right))))
      (append above (list new-row) (cdr below)))))

(define (board->xexpr board cr cc k-url)
  (let ((next (remove-piece board cr cc))
        (rows (length board))
        (cols (length (car board))))
    `(div
      ((class "board"))
      ,@(for/list (((row r) (in-indexed board)))
          `(div
            (span ((class "border")) ,(number->string (- rows r)))
            ,@(for/list (((piece c) (in-indexed row)))
                (let* ((classes
                        (string-join
                         (list*
                          (symbol->string (color piece))
                          (if (odd? (+ r c)) "black-square" "white-square")
                          (if (and (= r cr) (= c cc)) '("cursor") null))
                         " "))
                       (x (piece->xexpr piece)))
                  (if (valid-move? board cr cc r c)
                      `(a ((class ,classes)
                           (href ,(k-url (λ (req) (make-page next r c)))))
                          ,x)
                      `(span ((class ,classes)) ,x))))))
      ;; Bottom border:
      ,@(for/list ((elt `(nbsp ,@(for/list ((c cols))
                                   (string
                                    (integer->char
                                     (+ (char->integer #\a) c)))))))
          `(span ((class "border")) ,elt)))))

;; Sadly, using the actual white-piece glyph makes it look like the
;; background color (i.e. the color of the square) rather than white.
;; So we use black-piece glyphs for everything and change the
;; foreground color to white for a white piece.
(define (piece->xexpr piece)
  (if (blank? piece)
      'nbsp
      (symbol->string
       (if (white? piece)
           (invert piece)
           piece))))
