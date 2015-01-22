(define (list-stuff start nameproc lst end . nothing)
  (cond ((null? lst) (if (pair? nothing) (print (car nothing))))
	((null? (cdr lst)) (print start (nameproc (car lst)) end))
	((null? (cddr lst)) (print start (nameproc (car lst)) " and "
				   (nameproc (cadr lst)) end))
	(else (display start)
	      (for-each (lambda (obj)
			  (display (nameproc obj))
			  (display ", "))
			(cdr lst))
	      (print "and " (nameproc (car lst)) end))))

(define (delq elt lst)
  (cond ((null? lst) lst)
	((eq? (car lst) elt) (delq elt (cdr lst)))
	(else (cons (car lst) (delq elt (cdr lst))))))

(define (delete elt lst)
  (cond ((null? lst) '())
	((equal? elt (car lst)) (delete elt (cdr lst)))
	(else (cons (car lst) (delete elt (cdr lst))))))

(define (filter pred lst)
  (cond ((null? lst) '())
	((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
	(else (filter pred (cdr lst)))))

; return a pair of lists: those that do, and those that don't satisfy pred
(define (bifilter pred lst)
  (let bif ((lst lst)
	    (yes '())
	    (no '()))
    (cond ((null? lst) (cons yes no))
	  ((pred (car lst)) (bif (cdr lst) (cons (car lst) yes) no))
	  (else (bif (cdr lst) yes (cons (car lst) no))))))

;; code for dealing w/ alists nicely
(define (lookup elt alist success failure)
  (let ((m (assq elt alist)))
    (if m
	(if (pair? (cdr m))
	    (apply success (cdr m))
	    (success (cdr m)))
	(failure))))

;; delete entries with given tag from an alist
(define (adelq tag alist)
  (cond ((null? alist) alist)
	((eq? (caar alist) tag) (adelq tag (cdr alist)))
	(else (cons (car alist) (adelq tag (cdr alist))))))
	
(define (ensure-is-a pred obj type-name . name)
  (if (not (pred obj))
      (if (null? name)
	  (error (string-append "argument must be a " type-name) obj)
	  (error (string-append name " must be a " type-name) obj))))

(define (muserror . data)
  (apply error (cons 'museme data)))
