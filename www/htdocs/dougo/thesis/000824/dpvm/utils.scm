;	$Id: utils.scm,v 1.8 2000/03/01 18:27:03 gregs Exp gregs $	

;; Some generally useful functions, IMHO.  YMMV.

(define (spaces i)
  (do ((i i (- i 1)) (out '() (cons #\space out)))
      ((<= i 0) (list->string out))))

(define (assert test . args)
  (if (not test)
      (apply error #t args)))

(define curry
  (lambda (f . largs)
    (lambda rargs
      (apply f (append largs rargs)))))

(define rcurry
  (lambda (f . rargs)
    (lambda largs
      (apply f (append largs rargs)))))

(define (const k)
  (lambda (ignore) k))

;; f must be a function of one argument
(define (compose f g)
  (lambda args
    (f (apply g args))))

;; lists must all be the same length, call it n.
;; fn must take 1 + n arguments.
(define (reduce fn initval . lists)
  (let loop ((lists lists) (val initval))
    (if (null? (car lists))
	val
	(loop (map cdr lists)
	      (apply fn (cons val (map car lists)))))))

;; ex: (filter-collecting 
;;       (lambda (x y) (< x y)
;;       (lambda (x y) (+ x y)
;;       '(1 7 3 9)
;;       '(5 5 5 5)
;;  => (6 8)
(define (filter-collecting predicate collector . lists)
  (let loop ((lists lists) (out '()))
    (if (null? (car lists))
	(reverse out)
	(let ((heads (map car lists)))
	  (if (apply predicate heads)
	      (loop (map cdr lists) (cons (apply collector heads) out))
	      (loop (map cdr lists) out))))))

(define (first-n n in-lst)
  (let loop ((lst in-lst) (i 0) (out '()))
    (cond
     ((>= i n) (reverse out))
     ((null? lst) (error "first-n, n=~a, past end of list ~a" n in-lst))
     (else (loop (cdr lst) (+ i 1) (cons (car lst) out))))))

(define (last l)
  (car (reverse l)))

(define (non-last l)
  (reverse (cdr (reverse l))))

(define (symbols-to-string syms)
  (do ((syms syms (cdr syms))
       (str "" (string-append str (symbol->string (car syms)))))
      ((null? syms) str)))

(define (make-name format-string . args)
  (string->symbol (apply format #f format-string args)))

(define (looks-like-an-integer? e)
  (let* ((s (format #f "~a" e))
	 (l (string->list s)))
    (every? char-numeric? l)))

(define *gensym-counter* (vector 0))
(define gensym (lambda ()
		 (let ((n (vector-ref *gensym-counter* 0)))
		   (vector-set! *gensym-counter* 0 (+ n 1))
		   (+ n 1))))
			      
		 
;; eof
