;; llvm-test.scm
;	$Id: dpvm-test.scm,v 1.6 2000/03/09 17:39:19 gregs Exp gregs $	

,load ../scheme-utils/utils.scm
,load dpvm.scm initialize.scm predicates.scm dpvml.scm contexts.scm

(dpvml-repl)

(define (assert= id v1 v2) (-> <top> <top>)
  (assert (= v1 v2) "Oops ~a" id))
(define (assert-string= id v1 v2) (-> <top> <top>)
  (assert (string=? v1 v2) "Oops ~a" id))

(define x <integer> 5)
(let ((x 3) (y x)) <top> (+ x y))	; => 8
(let* ((x 3) (y x)) <top> (+ x y))	; => 6

(assert= "t1" 1
	 (let ((x 1)) <top> x)
	 )
(assert= "t2" 5
	 (let ((x 2) (y 3))
	   (list-pred <integer> <integer>)
	   (+ x y))
	 )
(assert= "t3" 12
	 (let ((f (lambda (x y)
		       (-> (list-pred <integer> <integer>) <integer>)
		       (+ x y))))
	   <top>
	   (f 3 (f 4 5)))
	 )
(assert= "t4" 12
	 (let ((x (list 9 2 3)))
	   <top>
	   (+ (list-ref x 0) (list-ref x 2)))
	 )
(assert-string= "t5" "yes"
		(let ((x 1) (y 4))
		  (list-pred <integer> <integer>)
		  (if (< x 2)
		      "yes"
		      "no"))
		)
(assert= "t6" 3
	 (let ((x 3))
	   (pred vals (< (car vals) 5))
	   x)
	 )

(assert= "t7" 3
	 (let ((x 1) (y 2))
	   (pred vals
		 (< (car vals) (cadr vals)))
	   (+ x y))
	 )

(define x <integer> 4)
(define <is-x> <pred> (eq-pred x))
(define y <list> (list 1 2 3))
(define <is-y> <pred> (eq-pred y))
(define <x-or-y> <pred> (or-pred <is-x> <is-y>))

(assert
 (pred-implies? <is-x> <x-or-y>)
 "t8")

(let ((x 1) (y 2))
  (pred vals
	(< (car vals) (cadr vals)))
  (+ x y))

(define t1 <list> (list 2 4 6 8 0))

(define <even> <pred>
  (and-pred <integer> (pred v (= 0 (mod v 2)))))

(check-pred <even> 6)

(define <foo> <pred>
	(or-pred <even>
		 (pred (v) (string=? v "hello"))))

(check-pred <foo> "hello")

(define (f1 x y)
  (-> (elt-pred <integer>) <integer>)
  (+ 1 (+ x y)))

(apply f1 (list 2 3))

(define (fact n)
  (-> (list-pred <integer>) <integer>)
  (if (< n 2)
      1
      (* n (fact (- n 1)))))

(define (iter-fact n) 
  (-> (list-pred <integer>) <integer>)
  (let* ((f (lambda (n tot)
		 (-> (list-pred <integer> <integer>) <integer>)
		 (if (< n 2)
		     tot
		     (f (- n 1) (* n tot))))))
    <top>
    (f n 1)))

(define plus2 <top> (curry + 2))
(define minus1 <top> (rcurry - 1))
(define plus1 <top> (compose plus2 minus1))
(plus1 6)

(map plus1 (list 1 2 3))

(let loop ((x 4) (y 0)) <top>
  (if (< x 1)
      y
      (loop (- x 1) (+ y 2))))

(define foo <integer> 1)
(define <is-foo> <pred> (eq-pred foo))
(check-pred <is-foo> foo)
(check-pred <is-foo> 1)

(let* ((f (lambda (x y) (-> (list-pred <integer> <integer>) <list>)
	       (if (< x 1)
		   (list "done with f, " x y)
		   (g (- x 1) (+ y x)))))
       (g (lambda (x y) (-> (list-pred <integer> <integer>) <list>)
	       (if (< x 1)
		   (list "done with g, " x y)
		   (f (- x 1) (* y x))))))
  <top>
  (f 8 5))

;; following used to go into a loop:
(pred-implies? <boolean> <integer>)

(let ((z <))
  (list-pred (-> (list-pred <integer> <integer>) <boolean>))
  (z 1 2))

(let ((x 3) (y "hello") (z <))
  (list-pred <integer> <string> 
	     (-> (list-pred <integer> <integer>) <integer>))
  (apply z (list x 2)))


;; eof		       

