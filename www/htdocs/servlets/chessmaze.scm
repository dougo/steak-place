#lang web-server

(require web-server/servlet)
(require net/url)
(provide interface-version start)
(define interface-version 'stateless)

(define (start req)
  (with-errors-to-browser send/back (lambda () (handle-request req))))

(define *start-maze*
  (string-append
   "@wqw,brb,w,wpb,w,wrb,wrw,bqb-"
   "wpb,bbw,b,bnw,bbb,w,bbb,w-"
   "bnw,b,wrw,b,w,b,w,bnb-"
   "b,w,wnb,bpw,wnb,w,wnb,wbw-"
   "wrw,wrb,w,bnb,brw,bnb,w,bnb-"
   "b,w,wnb,wbw,b,brw,b,bpw-"
   "w,wrb,wqw,b,w,b,wrw,b-"
   "bpb,brw,b,bbw,b,wnw,bbb,wkw"))

(define (decode-board board)
  (map decode-row (regexp-split "-" board)))

(define (decode-row row)
  (regexp-split "," row))

(define (piece-color-board board)
  (ormap piece-color-row board))

(define (piece-color-row row)
  (ormap piece-color row))

(define (piece-color square)
  (and (at? square)
       (let ((square (decode-at square)))
	 (and (not (vacant? square))
	      (decode-color square)))))

(define (remove-piece-from-board board)
  (map remove-piece-from-row board))

(define (remove-piece-from-row row)
  (map remove-piece row))

(define (remove-piece square)
  (if (at? square)
      (bg square)
      square))

(define (at board r c)
  (let loop ((rows board))
    (if (null? rows)
	'()
	(let ((row (car rows))
	      (rows (cdr rows)))
	  (if (= r (length rows))
	      (cons (at-row row c) rows)
	      (cons row (loop rows)))))))

(define (at-row row c)
  (let loop ((cols row))
    (if (null? cols)
	'()
	(let ((col (car cols))
	      (cols (cdr cols)))
	  (if (= c (length cols))
	      (cons (encode-at col) cols)
	      (cons col (loop cols)))))))

(define (board-xexprs board next color)
  (let loop ((rows board))
    (if (null? rows)
	`((tr ((align "center"))
	      (td) ,@(map (lambda (s) `(td (b (font ((size "+2")) ,s))))
			  (list "a" "b" "c" "d" "e" "f" "g" "h"))))
	(let ((row (car rows))
	      (rows (cdr rows)))
	  (cons `(tr (td (b (font ((size "+2"))
				  ,(number->string (+ 1 (length rows))))))
		     ,@(row-xexprs row next (length rows) color))
		(loop rows))))))

(define (row-xexprs row next r color)
  (let loop ((cols row))
    (if (null? cols)
	'()
	(let ((col (car cols))
	      (cols (cdr cols)))
	  (cons (square-xexpr col next r (length cols) color)
		(loop cols))))))

(define (square-xexpr square next r c color)
  (cond ((at? square)
	 `(td ((bgcolor "red")) ,(image (decode-at square))))
	((or (vacant? square)
	     (eq? color (decode-color square)))
	 `(td ,(image square)))
	(else
	 `(td (a ((href ,(string-append "?b=" (encode-board (at next r c)))))
		 ,(image square))))))

(define (vacant? str)
  (= (string-length str) 1))

(define (bg str)
  (substring str (- (string-length str) 1)))

(define (image col)
  `(img ((src ,(string-append "/chess/" col ".gif")))))

(define (encode-at col)
  (string-append "@" col))

(define (at? square)
  (char=? (string-ref square 0) #\@))

(define (decode-at square)
  (substring square 1))

(define (decode-color square)
  (string-ref square 0))

(define (encode-board board)
  (string-append-separator (map encode-row board) "-"))

(define (encode-row row)
  (string-append-separator row ","))

(define (string-append-separator strings sep)
  (cond ((null? strings) strings)
	((null? (cdr strings)) (car strings))
	(else (string-append (car strings) sep
			     (string-append-separator (cdr strings) sep)))))

(define (query-board-xexprs query)
  (let ((board (decode-board query)))
    (board-xexprs board (remove-piece-from-board board)
		  (piece-color-board board))))

(define (handle-request req)
  (let* ((binding (bindings-assq #"b" (request-bindings/raw req)))
	 (query (if (binding:form? binding)
		    (bytes->string/utf-8 (binding:form-value binding))
		    *start-maze*)))
    (response/xexpr
     `(html
       (head (title "Chess Maze"))
       (body
        (h1 (hr) "Chess Maze")
        (p "by Mark J. P. Wolf (" (tt "mark.wolf") " at " (tt "cuw.edu") ")."
           "  Originally appeared at "
           (a ((href "http://mathpuzzle.com")) "Mathpuzzle.com") ".")
        ;; TODO: link to new versions
        (p "Starting with the White Queen at a8, capture your way to the White King at h1.  Pieces capture as they do in chess (except pawns, which can capture one square diagonally in any direction), and pieces can only capture pieces of the opposite color.  However, once a capture is made your piece becomes the type of piece that was just captured (and moves accordingly on the next move) and all moves must end in a capture.  Pieces that are captured are removed from the board, so the number of pieces on the board gradually decreases.  (For an easy warm-up, try capturing the black queen in five moves, or the black pawn at a1 in nine moves).")
        (p "Note that this servlet does not (yet) enforce the movement rules, just the color capture rule.")
        (table ((cellspacing "0"))
               ,@(query-board-xexprs query))
        (a ((href "chessmaze.scm")) "Restart")
        (hr)
        (address (a ((href "/dougo/")) "Doug Orleans") " "
                 (a ((href "mailto:dougorleans@gmail.com"))
                    "<dougorleans@gmail.com>"))
        )))))
