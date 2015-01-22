; Mode: Scheme
;
;
; *************************************************************************
; Copyright (c) 1992 Xerox Corporation.  
; All Rights Reserved.  
;
; Use, reproduction, and preparation of derivative works are permitted.
; Any copy of this software or of any derivative work must include the
; above copyright notice of Xerox Corporation, this paragraph and the
; one after it.  Any distribution of this software or derivative works
; must comply with all applicable United States export control laws.
;
; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS
; ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGES.
; *************************************************************************
;
;
; Scheme is such a wonderful language, you can't program in it!
;
; This is a library of stuff I find useful.  I'll bet there's dozens
; of these out there.
;
; 11/12/99 -- converted to use SLIB & R5RS.  --dougo
;             attempt at portable `error' behavior.  --dougo
;

(require 'sort)


;; Some versions of `error' put spaces between arguments when forming
;; a string, some don't...  We just assume the former, and use this
;; function on systems with the latter.
(define (add-spaces-between-elements l)
  (if (or (null? l) (null? (cdr l)))
      l
      (list* (car l) " " (add-spaces-between-elements (cdr l)))))

;; Some versions of `error' want a format string.
(define (make-format-string n)
  (if (= n 0)
      "~%"
      (string-append "~a" (make-format-string (1- n)))))

(define gerror
  (case (scheme-implementation-type)
    ((MITScheme) error)
    ((chez)
     (lambda args
	 (set! args (add-spaces-between-elements args))
	 (apply error #f (make-format-string (length args)) args)))
    ((larceny)
     (lambda args
       (apply error (add-spaces-between-elements args))))
    (else error)))

(define ??? 'unspecified-result)

(define (position-of x lst)
  (if (eq? x (car lst)) 0 (+ 1 (position-of x (cdr lst)))))

(define (every test . lists)
  (let scan ((tails lists))
    (if (member #t (map null? tails))             ;(any null? lists)
	#t
	(and (apply test (map car tails))
	     (scan (map cdr tails))))))

(define (getl initargs name . not-found)
  (letrec ((scan (lambda (tail)
		   (cond ((null? tail)
			  (if (pair? not-found)
			      (car not-found)
			      (gerror "GETL couldn't find" name)))
			 ((eq? (car tail) name) (cadr tail))
			 (else (scan (cddr tail)))))))
    (scan initargs)))


(define (filter-in test? list)
  (cond ((null? list) '())
	((test? (car list))
	 (cons (car list) (filter-in test? (cdr list))))
	(else (filter-in test? (cdr list)))))



;
; A simple topological sort.
;
; It's in this file so that both TinyClos and Objects can use it.
;
; This is a fairly modified version of code I originally got from Anurag
; Mendhekar <anurag@moose.cs.indiana.edu>.
;
;

(define (compute-std-cpl c get-direct-supers)
  (top-sort ((build-transitive-closure get-direct-supers) c)
	    ((build-constraints get-direct-supers) c)
	    (std-tie-breaker get-direct-supers)))


(define (top-sort elements constraints tie-breaker)
  (let loop ((elements    elements)
	     (constraints constraints)
	     (result      '()))
    (if (null? elements)
	result
	(let ((can-go-in-now
	       (filter-in
		(lambda (x)
		  (every (lambda (constraint)
			   (or (not (eq? (cadr constraint) x))
			       (memq (car constraint) result)))
			 constraints))
		elements)))
	  (if (null? can-go-in-now)
	      (gerror "top-sort: Invalid constraints")
	      (let ((choice (if (null? (cdr can-go-in-now))
				(car can-go-in-now)
				(tie-breaker result
					     can-go-in-now))))
		(loop
		 (filter-in (lambda (x) (not (eq? x choice)))
			    elements)
		;(filter-in (lambda (x) (not (eq? (cadr x) choice)))
		;           constraints)
		 constraints
		 (append result (list choice)))))))))

(define (std-tie-breaker get-supers)
  (lambda (partial-cpl min-elts)
    (let loop ((pcpl (reverse partial-cpl)))
      (let* ((current-elt (car pcpl))
	     (ds-of-ce (get-supers current-elt))
	     (common (filter-in (lambda (x)
				  (memq x ds-of-ce))
				min-elts)))
	(if (null? common)
	    (if (null? (cdr pcpl))
		(gerror "std-tie-breaker: Nothing valid")
		(loop (cdr pcpl)))
	    (car common))))))

(define (build-transitive-closure get-follow-ons)
  (lambda (x)
    (let track ((result '())
		(pending (list x)))
      (if (null? pending)
	  result
	  (let ((next (car pending)))
	    (if (memq next result)
		(track result (cdr pending))
		(track (cons next result)
		       (append (get-follow-ons next)
			       (cdr pending)))))))))

(define (build-constraints get-follow-ons)
  (lambda (x)
    (let loop ((elements ((build-transitive-closure get-follow-ons) x))
	       (this-one '())
	       (result '()))
      (if (or (null? this-one) (null? (cdr this-one)))
	  (if (null? elements)
	      result
	      (loop (cdr elements)
		    (cons (car elements)
			  (get-follow-ons (car elements)))
		    result))
	  (loop elements
		(cdr this-one)
		(cons (list (car this-one) (cadr this-one))
		      result))))))
