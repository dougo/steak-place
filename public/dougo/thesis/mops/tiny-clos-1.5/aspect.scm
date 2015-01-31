(require 'common-list-functions)	; set-difference, remove, remove-if
(require 'hashtable)
(require "tiny-clos")

(define-class <aspectizable> () (aspects)) ;; mixin
(define-class <aspectizable-class> (<class> <aspectizable>) ())
(define-class <aspectizable-generic> (<generic> <aspectizable>) ()
  'metaclass <entity-class>)

(define-method (initialize (this <aspectizable>) initargs)
  (slot-set! this 'aspects '())
  (call-next-method))

(define-method (allocate-instance (this <aspectizable-class>))
  (let ((instance (call-next-method)))
    (for-each (lambda (aspect) (add-instance! aspect instance))
	      (slot-ref this 'aspects))
    instance))

(define-class <aspect> (<class>) (direct-advice introductions))
(define-method (initialize (aspect <aspect>) initargs)
  (slot-set! aspect 'direct-advice '())
  (slot-set! aspect 'introductions '())
  (call-next-method))

(define-generic aspect-direct-advice)
(define-generic aspect-advice)
(define-generic aspect-introductions)
(define-generic add-instance!)
(define-generic aspect-of)

(define-method (aspect-direct-advice (aspect <aspect>))
  (slot-ref aspect 'direct-advice))
(define-method (aspect-advice (class <class>))
  '())
(define-method (aspect-advice (aspect <aspect>))
  ;; TBD: memoize, like 'slots
  (apply append (aspect-direct-advice aspect)
	 (map aspect-advice (class-direct-supers aspect))))
(define-method (aspect-introductions (aspect <aspect>))
  (slot-ref aspect 'introductions))
(define-method (add-instance! (this <aspect>) (obj <object>))
  ;; ignore.
  #f)
(define-method (aspect-of (aspect <aspect>) (obj <object>))
  ;; FIXME: we don't actually need an instance, but we don't have
  ;; static method calls, so we make a dummy instance so we dispatch
  ;; the right method.  (used in advice-applies? and apply-advice)
  (make aspect))

(define-class <aspect-of-eachVM> (<aspect>) (instance))
(define-method (aspect-of (aspect <aspect-of-eachVM>) (obj <object>))
  (aspect-of aspect))
(define-method (aspect-of (aspect <aspect-of-eachVM>))
  (or (slot-ref aspect 'instance)
      (let ((instance (make aspect)))
	(slot-set! aspect 'instance instance)
	instance)))

(define-class <aspect-of-eachobject> (<aspect>) (instances))
(define-method (initialize (this <aspect-of-eachobject>) initargs)
  (call-next-method)
  ;; FIXME: use weak table
  (slot-set! this 'instances (make-hashtable)))
(define-method (add-instance! (this <aspect-of-eachobject>) (obj <object>))
  (hashtable-put! (slot-ref this 'instances) obj (make this)))
(define-method (aspect-of (this <aspect-of-eachobject>) (obj <object>))
  (hashtable-get (slot-ref this 'instances) obj))

(define-generic add-aspect!)
(define-generic remove-aspect!)

(define-method (add-aspect! (aspectizable <aspectizable>) (aspect <aspect>))
  (slot-set! aspectizable 'aspects
	     (adjoin aspect (slot-ref aspectizable 'aspects))))
(define-method (remove-aspect! (aspectizable <aspectizable>) (aspect <aspect>))
  (slot-set! aspectizable 'aspects
	     (remove aspect (slot-ref aspectizable 'aspects))))

(define-class <advice> (<generic>) (aspect pointcut) 'metaclass <entity-class>)
(define-class <before-advice> (<advice>) () 'metaclass <entity-class>)
(define-class <after-advice> (<advice>) () 'metaclass <entity-class>)
(define-class <around-advice> (<advice>) () 'metaclass <entity-class>)

(define-generic advice-aspect)
(define-generic advice-pointcut)

(define-method (advice-aspect (advice <advice>))
  (slot-ref advice 'aspect))
(define-method (advice-pointcut (advice <advice>))
  (slot-ref advice 'pointcut))

(define-generic add-advice!)
(define-generic remove-advice!)

(define-method (add-advice! (aspect <aspect>) (advice <advice>))
  (slot-set! aspect 'direct-advice 
	     (adjoin advice (slot-ref aspect 'direct-advice)))
  (slot-set! advice 'aspect aspect))
(define-method (remove-advice! (aspect <aspect>) (advice <advice>))
  (slot-set! aspect 'direct-advice
	     (remove advice (slot-ref aspect 'direct-advice)))
  (slot-set! advice 'aspect #f))

;; A pointcut is a generic function with two arguments, the aspect
;; instance and the join-point, which returns true or false if the
;; join-point is a member of the pointcut.  TBD: also return the
;; exposed execution context?  which is sort of like unification...
(define-class <pointcut> (<generic>) () 'metaclass <entity-class>)

(define-class <introduction> () ()) ;; TBD

(define-class <join-point> () (generic args returning))
(define-class <call-join-point> (<join-point>) ()) ;; TBD
(define-class <reception-join-point> (<join-point>) ())
(define-class <execution-join-point> (<join-point>) (method))

(define-generic this) ;; the `current' object.  i.e. the `receiver'.
(define-method (this (jp <join-point>))
  (car (slot-ref jp 'args)))

(define-class <signature> () (generic method))
(define-method (initialize (signature <signature>) initargs)
  (call-next-method)
  (slot-set! signature 'generic (getl initargs 'generic #f))
  (slot-set! signature 'method (getl initargs 'method #f)))

(define-method (print-object (signature <signature>) (port <port>))
  (display
   (cons (generic-name (slot-ref signature 'generic))
	 (let* ((method (slot-ref signature 'method))
		(specs (method-specializers method))
		(proc (method-procedure method))
		(args (cdadr (procedure-expression proc))))
	   (let loop ((specs specs) (args args))
	     (if (null? specs)
		 args
		 (cons (list (car args) (class-name (car specs)))
		      (loop (cdr specs) (cdr args)))))))
   port))

(define-generic advice-instance)
(define-generic advice-applies?)
(define-generic before-advice?)
(define-generic after-advice?)
(define-generic around-advice?)
(define-generic apply-advice)

(define-method (advice-applies? (advice <advice>) (aspect <aspect>)
				(jp <join-point>))
  ((advice-pointcut advice) (aspect-of aspect (this jp)) jp))
(define-method (before-advice? (advice <advice>)) #f)
(define-method (before-advice? (advice <before-advice>)) #t)
(define-method (after-advice? (advice <advice>)) #f)
(define-method (after-advice? (advice <after-advice>)) #t)
(define-method (around-advice? (advice <advice>)) #f)
(define-method (around-advice? (advice <around-advice>)) #t)

;; TBD: use fluid-let to assign "this-join-point" and "proceed"
;; TBD: pass the exposed context (gotten from evaluating the pointcut)
(define-method (apply-advice (advice <advice>) (aspect <aspect>)
			     (jp <join-point>))
  (advice (aspect-of aspect (this jp)) jp))
(define-method (apply-advice (advice <around-advice>) (aspect <aspect>)
			     (jp <join-point>) (cont <procedure>))
  (advice (aspect-of aspect (this jp)) jp cont))

(define-method (compute-apply-methods (generic <aspectizable-generic>))
  (lambda (methods args)
    (if (null? methods)
	(gerror "No applicable method for" generic args)
	(let ((jp (make <reception-join-point>)))
	  (slot-set! jp 'generic generic)
	  (slot-set! jp 'args args)
	  (apply-all-advice
	   (slot-ref generic 'aspects)
	   jp
	   (lambda ()
	     (let loop ((tail methods))
	       (if (null? tail)
		   (gerror "No next method for" generic args)
		   (let ((jp (make <execution-join-point>)))
		     (slot-set! jp 'generic generic)
		     (slot-set! jp 'args args)
		     (slot-set! jp 'method (car tail))
		     (apply-all-advice
		      (slot-ref generic 'aspects)
		      jp
		      (lambda ()
			(apply (method-procedure (car tail))
			       (cons (lambda () (loop (cdr tail)))
				     args)))))))))))))

(define (apply-all-advice aspects jp cont)
  (if (null? aspects)
      (cont)
      (let* ((advice (apply append (map (lambda (aspect)
					  (map (lambda (advice)
						 (cons advice aspect))
					       (aspect-advice aspect)))
					aspects)))
	     (applicable-advice
	      (filter-in (lambda (advice)
			   (advice-applies? (car advice) (cdr advice) jp))
			 advice))
	     (befores (filter-in (lambda (advice) (before-advice? (car advice)))
				 applicable-advice))
	     (afters  (filter-in (lambda (advice) (after-advice? (car advice)))
				 applicable-advice))
	     (arounds (filter-in (lambda (advice) (around-advice? (car advice)))
				 applicable-advice)))
	;; FIXME: should sort the advice
	(let loop ((arounds arounds))
	  (if (not (null? arounds))
	      (apply-advice (caar arounds) (cdar arounds) jp
			    (lambda () (loop (cdr arounds))))
	      (begin
		(for-each (lambda (before)
			    (apply-advice (car before) (cdr before) jp))
			  befores)
		(if (null? afters)
		    (cont)
		    (let ((ret (cont)))
		      (slot-set! jp 'returning ret)
		      (for-each (lambda (after)
				  (apply-advice (car after) (cdr after) jp))
				afters)
		      ret))))))))
