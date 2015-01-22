;	$Id: mmmoo-front.scm,v 1.2 2000/03/09 17:39:43 gregs Exp gregs $	

;; miniature multi-method object-oriented language

;; Program ::= [DefExp*] [Exp]
;;
;; DefExp ::= (defclass classname superclass-list SlotSpec-list)
;;         |  (defmethod methname Var-list arg-Classnames result-Classname body-Exp)
;;
;; Exp ::= (let Var ClassName Exp Exp)
;;      |  (define Var Exp)
;;      |  (if Exp Exp Exp)
;;      |  (seq Exp ...)
;;      |  (list Exp ...)
;;      |  (Exp Exp-list)          -- application
;;      |  Var
;;      |  Constant
;;
;; SlotSpec ::= (slotname ClassName)
;; Classname :== Identifier
;;
;; * for a slot f, there is a getter method f(obj) 
;;     and a setter method set-f!(obj, newval)
;;
;; Notes:
;;  * any "real" system would keep a map from names/bindings in source to 
;;    names/bindings in the VM.  This translation imposes the same lookup 
;;    semantics of the vm on the source language, for simplicity.

;; TO DO:
;;   * init keywords to make
;;   * keywords in general.
;;   * distinguish btwn mutable and immutable slots
    
(define (mmmoo-sexp-to-dpvml-sexp e)
  (if (pair? e)
      (case (car e)
	((defclass)
	 `(define ,(list-ref e 1)
	    <class>
	    (make-class
	     ;; class name
	     ,(symbol->string (list-ref e 1))
	     ;; superclass list
	     (list ,@(list-ref e 2))
	     ;; slotspec list: list of (slot-name-string, class) lists
	     ;; e.g. (list (list "slot1" <object>) (list "slot2" <foo>) ...)
	     (list 
	      ,@(map (lambda (l)
		       (list 'list (symbol->string (car l)) ; slot name string
			     (cadr l)))	; class
		     (list-ref e 3))))))
	((defmethod)
	 `(add-method-to-name
	   ,(symbol->string (list-ref e 1)) ; generic name
	   ;; closure
	   (lambda ,(list-ref e 2)	; var-list
		(-> (list-pred
		     ,@(map (lambda (e) (list 'class-pred e)) ; arg classes
			    (list-ref e 3)))
		    (class-pred ,(list-ref e 4))) ; result class
		,(mmmoo-sexp-to-dpvml-sexp (list-ref e 5))))) ; body
	((define)
	 `(define ,(list-ref e 1) <top>
	    ,(mmmoo-sexp-to-dpvml-sexp (list-ref e 2))))
	((let)
	 `(let (,(list-ref e 1)
		,(mmmoo-sexp-to-dpvml-sexp (list-ref e 3)))
	    (list-pred (class-pred ,(mmmoo-sexp-to-dpvml-sexp (list-ref e 2))))
	    ,(mmmoo-sexp-to-dpvml-sexp (list-ref e 4))))
	((if)
	 `(if ,@(map mmmoo-sexp-to-dpvml-sexp (cdr e))))
	((seq)
	 `(seq ,@(map mmmoo-sexp-to-dpvml-sexp (cdr e))))
	((list)
	 `(as <object>
	      (list ,@(map mmmoo-sexp-to-dpvml-sexp (cdr e)))))
	(else   ;;; non-keyword pair -- must be an application
	 `(apply-generic ,(mmmoo-sexp-to-dpvml-sexp (car e))
			 (list ,@(map mmmoo-sexp-to-dpvml-sexp (cdr e))))))
      ;; not a pair -- either a constant or a varref 
      (cond
       ((or (looks-like-an-integer? e) (string? e))	; a literal?
	`(as <object> ,e))
       (else				; if all else ctxts, must be a var. ref.
	e))))

(define (mmmoo-repl)
  (start-dpvm-repl
   "mmmoo"
   (lambda (sexp)
     (let ((out ((compose parse-dpvml-exp mmmoo-sexp-to-dpvml-sexp)
		 sexp)))
       (format #t "[~a]~%" (data->sexp out))
       out))
   (parse-dpvml-exp
    `(begin
      (load-dpvml-file "utils.dpvml")
      (load-dpvml-file "mmmoo-runtime.dpvml")))))

;; eof
