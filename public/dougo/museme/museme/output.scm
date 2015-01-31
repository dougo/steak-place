;;; ouput that forces itself

(define d display)
(define n newline)
(define w write)
(define wc write-char)
(define (fo) (force-output (current-output-port)))

(define (display . args) (apply d args) (fo))
(define (write . args) (apply w args) (fo))
(define (newline . args) (apply n args) (fo))
(define (write-char . args) (apply wc args) (fo))
(define (print . args) (for-each (lambda (arg) (d arg)) args) (n) (fo))
