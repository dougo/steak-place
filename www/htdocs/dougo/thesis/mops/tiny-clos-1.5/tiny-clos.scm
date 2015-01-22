; Mode: Scheme
;
;
; **********************************************************************
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
; **********************************************************************
;
; EDIT HISTORY:
;
;      10/**/92  Gregor  Originally Written
; 1.0  11/10/92  Gregor  Changed names of generic invocation generics.
;                        Changed compute-getters-and-setters protocol.
;                        Made comments match the code.
;                        Changed maximum line width to 72.
; 1.1  11/24/92  Gregor  Fixed bug in compute-method-more-specific?,
;                        wrt the use of for-each.
;                        Both methods on allocate instance failed to
;                        initialize fields properly.
;                        The specializers and procedure initargs are
;                        now required when creating a method, that is,
;                        they no longer default.  No working program
;                        should notice this change.
; 1.2  12/02/92  Gregor  Fix minor things that improve portability:
;                          DEFINE needs 2 args in R4Rs
;                          Conditionalize printer hooks.
;                          () doesn't evaluate to ()
; 1.3  11/12/99  Doug    Ported to SLIB & R5RS.
;                        Added <nil>, <port>, and
;                          <unknown-primitive-object> primitive classes.
;                        Use gerror.
;                        Added error messages for no applicable method
;                          and no next method.
;                        Changed default slot value from '() to #f.
;                        Renamed add-method to add-method!.
;                        Allow multiple methods on same generic with
;                          different lengths of specializers, regardless
;                          of whether they match.
;                        Removed non-portable "return" in
;                          compute-method-more-specific? (exposed by
;                          above fix).
; 1.4  11/12/99  Doug    Added names to classes and generic functions.
;                        Added 'metaclass option to make-class
;                          and 'generic-class option to make-generic.
;                        Added define-* syntax.
;                        Added print-object.
; 1.5  11/13/99  Doug    Added %instance-fields and %set-instance-fields!
;                          to support things like change-class!.
;
;       
(define tiny-clos-version "1.5")

;
; A very simple CLOS-like language, embedded in Scheme, with a simple
; MOP.  The features of the default base language are:
;
;   * Classes, with instance slots, but no slot options.
;   * Multiple-inheritance.
;   * Generic functions with multi-methods and class specializers only.
;   * Primary methods and call-next-method; no other method combination.
;   * Uses Scheme's lexical scoping facilities as the class and generic
;     function naming mechanism.  Another way of saying this is that
;     class, generic function and methods are first-class (meta)objects.
;
; While the MOP is simple, it is essentially equal in power to both MOPs
; in AMOP.  This implementation is not at all optimized, but the MOP is
; designed so that it can be optimized.  In fact, this MOP allows better
; optimization of slot access extenstions than those in AMOP.
; 
;
;
; In addition to calling a generic, the entry points to the default base
; language are:
;
;   (MAKE-CLASS name list-of-superclasses list-of-slot-names)
;   (MAKE-GENERIC name)
;   (MAKE-METHOD list-of-specializers procedure)
;   (ADD-METHOD! generic method)
;
;   (DEFINE-CLASS name (superclass ...) (slot-name ...) . options)
;                       ;If no superclasses are specified, <object> is
;                       ;assumed as the sole direct superclass.
;                                        ;Note that slot names are NOT
;                                        ;evaluated; if you want slots
;                                        ;with names other than
;                                        ;literal symbols, use
;                                        ;MAKE-CLASS instead.
;   (DEFINE-GENERIC name . options)
;   (DEFINE-METHOD (generic specializer ...) procedure)
;                           ;A specializer can be a list
;                           ;(parameter-name class) or just a
;                           ;parameter name, in which case the
;                           ;specializer is <top>.
;
;   (MAKE class . initargs)
;   (INITIALIZE instance initargs)            ;Add methods to this,
;                                             ;don't call it directly.
;   
;   (SLOT-REF  object slot-name)
;   (SLOT-SET! object slot-name new-value)
;
;
; So, for example, one might do:
;
;   (define-class <position> () (x y))
;   (define-method (initialize (pos <position>) initargs)
;     (for-each (lambda (initarg-name slot-name)
;		  (slot-set! pos
;			     slot-name
;			     (getl initargs initarg-name 0)))
;	        '(x y)
;	        '(x y)))
;
;   (set! p1 (make <position> 'x 1 'y 3))
;
;
;
; NOTE!  Do not use EQUAL? to compare objects!  Use EQ? or some hand
;        written procedure.  Objects have a pointer to their class,
;        and classes are circular structures, and ...
;
;
;
; The introspective part of the MOP looks like the following.  Note that
; these are ordinary procedures, not generics.
;
;   CLASS-OF
;
;   CLASS-NAME
;   CLASS-DIRECT-SUPERS
;   CLASS-DIRECT-SLOTS
;   CLASS-CPL
;   CLASS-SLOTS
;
;   GENERIC-NAME
;   GENERIC-METHODS
;
;   METHOD-SPECIALIZERS
;   METHOD-PROCEDURE
;
;
; The intercessory protocol looks like (generics in uppercase):
;
;   make                        
;     ALLOCATE-INSTANCE
;     INITIALIZE                   (really a base-level generic)
;
;   class initialization
;     COMPUTE-CPL
;     COMPUTE-SLOTS
;     COMPUTE-GETTER-AND-SETTER
;
;   add-method!                    (Notice this is not a generic!)
;     COMPUTE-APPLY-GENERIC
;       COMPUTE-METHODS
;         COMPUTE-METHOD-MORE-SPECIFIC?
;       COMPUTE-APPLY-METHODS
;

;
; OK, now let's get going.  But, as usual, before we can do anything
; interesting, we have to muck around for a bit first.  First, we need  
; to load the support library.
;
;
(load "support")

;
; Then, we need to build what, in a more real implementation, would be
; the interface to the memory subsystem: instances and entities.  The
; former are used for instances of instances of <class>; the latter
; are used for instances of instances of <entity-class>.  In this MOP,
; none of this is visible to base- or MOP-level programmers.
;

; instances
;
;
(define %allocate-instance    ???)
(define %instance?            ???)
(define %instance-class       ???)
(define %set-instance-class!  ???)
(define %instance-fields      ???)
(define %set-instance-fields! ???)
(define %instance-ref         ???)
(define %instance-set!        ???)

(require 'record)

(define (print-object obj port)
  (display "#<instance>" port))	;will become a generic function later

(let ((instance-type (make-record-type 'instance '(class fields))))

  ;; Make objects print a bit more reasonably.
  (define (prt-obj obj port) (print-object obj port))
  (case (scheme-implementation-type)
    ((MITScheme)
     ;; Not sure if this is still valid with records instead of tagged
     ;; vectors, though SLIB records are still tagged vectors
     ;; anyway... --dougo
     (unparser/set-tagged-vector-method! instance-type prt-obj))
    ((larceny)
     ((record-updater (record-type-descriptor instance-type) 'printer)
      instance-type prt-obj))
    ((chez) ???)
    (else ???))

  (set! %allocate-instance
	(lambda (class nfields)
	  (let ((fields (make-vector nfields #f)))
	    ((record-constructor instance-type) class fields))))

  (set! %instance?
	(record-predicate instance-type))	  
		 
  (set! %instance-class
	(record-accessor instance-type 'class))
		 
  (set! %set-instance-class!
	(record-updater instance-type 'class))

  (set! %instance-fields
	(record-accessor instance-type 'fields))

  (set! %set-instance-fields!
	(record-updater instance-type 'fields))
	
  (set! %instance-ref
        (lambda (instance index)
	  (let ((fields (%instance-fields instance)))
	    (vector-ref fields index))))
		  
  (set! %instance-set!
        (lambda (instance index new-value)
	  (let ((fields (%instance-fields instance)))
	    (vector-set! fields index new-value))))
  )


;
; This implementation of entities is almost laughably bad.   Maybe in
; fact it is laughably bad.  But, it works, and keep in mind that this
; level of the stuff should just be assumed as existing, rather than
; having to be understood by students and the like.
;
(define %allocate-entity   ???)
(define %entity?           ???)
(define %set-entity-proc!  ???)
(define %entity-class      ???)
(define %entity-ref        ???)
(define %entity-set!       ???)

(letrec ((entities '())
	 (get-vector
	  (lambda (closure)
	    (let ((cell (assq closure entities)))
	      (if cell (cdr cell) #f))))
	 (default-proc
	     (lambda args
	       (gerror "Called entity without first setting proc."))))

  ;; Make entities print a bit more reasonably-- they're functions
  ;; instead of records so we can't just do what we did for instances.
  (define (prt-obj obj port) (print-object obj port))
  (case (scheme-implementation-type)
    ((MITScheme) ???)
    ((larceny)
     (let ((old-r-p (repl-printer)))
       (repl-printer
	(lambda (obj port)
	  (if (and (procedure? obj) (%entity? obj))
	      (begin (prt-obj obj port) (newline port))
	      (old-r-p obj port))))))
    ((chez) ???)
    (else ???))

  (set! %allocate-entity
	(lambda (class nfields)
	  (letrec ((vector (make-vector (+ nfields 2) #f))
		   (closure (lambda args
			      (apply (vector-ref vector 0) args))))
	    (vector-set! vector 0 default-proc)
	    (vector-set! vector 1 class)
	    (set! entities (cons (cons closure vector) entities))
	    closure)))
		   
  (set! %entity?
        (lambda (x) (get-vector x)))

  (set! %entity-class
	(lambda (closure)
	  (let ((vector (get-vector closure)))
	    (vector-ref vector 1))))
		   
  (set! %set-entity-proc!
        (lambda (closure proc)
	  (let ((vector (get-vector closure)))
	    (vector-set! vector 0 proc)
	    closure)))
	
  (set! %entity-ref
        (lambda (closure index)
	  (let ((vector (get-vector closure)))
	    (vector-ref vector (+ index 2)))))
		  
  (set! %entity-set!
        (lambda (closure index new-value)
	  (let ((vector (get-vector closure)))
	    (vector-set! vector (+ index 2) new-value))))
  )

;
; These next three, plus %allocate-instance and %allocate-entity, are
; the normal interface, from the rest of the code, to the low-level
; memory system.  One thing to take note of is that the protocol does
; not allow the user to add low-level instance representations.  I
; have never seen a way to make that work.
;
; Note that this implementation of class-of assumes the name of a the
; primitive classes that are set up later.
; 
(define (class-of x)
  (cond ((%instance? x)  (%instance-class x))
	((%entity? x)    (%entity-class x))

	((null? x)       <nil>)
	((boolean? x)    <boolean>)
	((symbol? x)     <symbol>)
	((char? x)       <char>)
	((vector? x)     <vector>)
	((pair? x)       <pair>)
	((number? x)     <number>)
	((string? x)     <string>)
	((procedure? x)  <procedure>)
	((port? x)       <port>)
	(else            <unknown-primitive-object>)))

(define (get-field object field)
  (cond ((%instance? object) (%instance-ref object field))
	((%entity?   object) (%entity-ref   object field))
	(else
	 (gerror "Can only get-field of instances and entities."))))

(define (set-field! object field new-value)
  (cond ((%instance? object) (%instance-set! object field new-value))
	((%entity?   object) (%entity-set!   object field new-value))
	(else
	 (gerror "Can only set-field! of instances and entities."))))




;
; Now we can get down to business.  First, we initialize the braid.
;
; For Bootstrapping, we define an early version of MAKE.  It will be
; changed to the real version later on.  String search for ``set! make''.
;

(define (make class . initargs)
  (cond ((or (eq? class <class>)
	     (eq? class <entity-class>))
	 (let* ((new (%allocate-instance
		      class
		      (length the-slots-of-a-class)))
		(name    (getl initargs 'name 'unnamed))
		(dsupers (getl initargs 'direct-supers '()))
		(dslots  (map list
			      (getl initargs 'direct-slots  '())))
		(cpl     (let loop ((sups dsupers)
				    (so-far (list new)))
			   (if (null? sups)
			       (reverse so-far)
			       (loop (class-direct-supers
				      (car sups))
				     (cons (car sups)
					   so-far)))))
		(slots (apply append
			      (cons dslots
				    (map class-direct-slots
					 (cdr cpl)))))
		(nfields 0)
		(field-initializers '())
		(allocator
		 (lambda (init)
		   (let ((f nfields))
		     (set! nfields (+ nfields 1))
		     (set! field-initializers
			   (cons init field-initializers))
		     (list (lambda (o)   (get-field  o f))
			   (lambda (o n) (set-field! o f n))))))
		(getters-n-setters
		 (map (lambda (s)
			(cons (car s)
			      (allocator (lambda () #f))))
		      slots)))

	   (slot-set! new 'name               name)
	   (slot-set! new 'direct-supers      dsupers)
	   (slot-set! new 'direct-slots       dslots)
	   (slot-set! new 'cpl                cpl)
	   (slot-set! new 'slots              slots)
	   (slot-set! new 'nfields            nfields)
	   (slot-set! new 'field-initializers (reverse
					       field-initializers))
	   (slot-set! new 'getters-n-setters  getters-n-setters)
	   new))
	((eq? class <generic>)
	 (let ((new (%allocate-entity class
				      (length (class-slots class)))))
	   (slot-set! new 'name (getl initargs 'name 'unnamed))
	   (slot-set! new 'methods '())
	   new))
	((eq? class <method>)
	 (let ((new (%allocate-instance
		     class
		     (length (class-slots class)))))
	   (slot-set! new
		      'specializers
		      (getl initargs 'specializers))
	   (slot-set! new
		      'procedure
		      (getl initargs 'procedure))
	   new))))


;
; These are the real versions of slot-ref and slot-set!.  Because of the
; way the new slot access protocol works, with no generic call in line,
; they can be defined up front like this.  Cool eh?
;
;
(define (slot-ref object slot-name)
  (let* ((info   (lookup-slot-info (class-of object) slot-name))
	 (getter (list-ref info 0)))
    (getter object)))

(define (slot-set! object slot-name new-value)
  (let* ((info   (lookup-slot-info (class-of object) slot-name))
	 (setter (list-ref info 1)))
    (setter object new-value)))

(define (lookup-slot-info class slot-name)
  (let* ((getters-n-setters
	  (if (eq? class <class>)           ;* This grounds out
	      getters-n-setters-for-class   ;* the slot-ref tower.
	      (slot-ref class 'getters-n-setters)))
	 (entry (assq slot-name getters-n-setters)))
    (if (not entry)
	(gerror "No slot" slot-name "in instances of" class)
	(cdr entry))))



;
; Given that the early version of MAKE is allowed to call accessors on
; class metaobjects, the definitions for them come here, before the
; actual class definitions, which are coming up right afterwards.
;
;
(define (class-name class)
  (slot-ref class 'name))
(define (class-direct-slots class)
  (slot-ref class 'direct-slots))
(define (class-direct-supers class)
  (slot-ref class 'direct-supers))
(define (class-slots class)
  (slot-ref class 'slots))
(define (class-cpl class)
  (slot-ref class 'cpl))

(define (generic-name generic)
  (slot-ref generic 'name))
(define (generic-methods generic)
  (slot-ref generic 'methods))

(define (method-specializers method)
  (slot-ref method 'specializers))
(define (method-procedure method)
  (slot-ref method 'procedure))


;
; The next 7 clusters define the 6 initial classes.  It takes 7 to 6
; because the first and fourth both contribute to <class>.
;
(define the-slots-of-a-class     ;
    '(name			 ;anything, usually a symbol
      direct-supers              ;(class ...)        
      direct-slots               ;((name . options) ...)
      cpl                        ;(class ...) 
      slots                      ;((name . options) ...) 
      nfields                    ;an integer
      field-initializers         ;(proc ...)
      getters-n-setters))        ;((slot-name getter setter) ...)
                                 ;
(define getters-n-setters-for-class      ;see lookup-slot-info
    ;
    ; I know this seems like a silly way to write this.  The
    ; problem is that the obvious way to write it seems to
    ; tickle a bug in MIT Scheme!
    ;
    (let ((make-em (lambda (s f)
		     (list s
			   (lambda (o)   (%instance-ref  o f))
			   (lambda (o n) (%instance-set! o f n))))))
      (map (lambda (s)
	     (make-em s (position-of s the-slots-of-a-class)))
	   the-slots-of-a-class)))
(define <class> (%allocate-instance #f (length the-slots-of-a-class)))
(%set-instance-class! <class> <class>)

(define <top>          (make <class>
			     'name '<top>
			     'direct-supers (list)
			     'direct-slots  (list)))

(define <object>       (make <class>
			     'name '<object>
			     'direct-supers (list <top>)
			     'direct-slots  (list)))

;
; This cluster, together with the first cluster above that defines
; <class> and sets its class, have the effect of:
;
;   (define <class>
;     (make <class>
;           'direct-supers (list <object>)
;           'direct-slots  (list 'direct-supers ...)))
;
(%instance-set! <class> 0 '<class>)	                    ;name
(%instance-set! <class> 1 (list <object>))                  ;d supers
(%instance-set! <class> 2 (map list the-slots-of-a-class))  ;d slots
(%instance-set! <class> 3 (list <class> <object> <top>))    ;cpl
(%instance-set! <class> 4 (map list the-slots-of-a-class))  ;slots
(%instance-set! <class> 5 (length the-slots-of-a-class))    ;nfields
(%instance-set! <class> 6 (map (lambda (s)                  ;field-ini..
				 (lambda () #f))
			       the-slots-of-a-class))
(%instance-set! <class> 7 '())                              ;not needed


(define <procedure-class> (make <class>
				'name '<procedure-class>
				'direct-supers (list <class>)
				'direct-slots  (list)))

(define <entity-class>    (make <class>
				'name '<entity-class>
			        'direct-supers (list <procedure-class>)
			        'direct-slots  (list)))

(define <generic>         (make <entity-class>
				'name '<generic>
			        'direct-supers (list <object>)
			        'direct-slots  (list 'name 'methods)))

(define <method>          (make <class>
				'name '<method>
			        'direct-supers (list <object>)
			        'direct-slots  (list 'specializers
						     'procedure)))



;
; These are the convenient syntax we expose to the base-level user.
;
;
(define (make-class name direct-supers direct-slots . options)
  (apply make (getl options 'metaclass <class>)
	 'name name
	 'direct-supers direct-supers
	 'direct-slots  direct-slots
	 options))

(define (make-generic name . options)
  (apply make (getl options 'generic-class <generic>)
	 'name name
	 options))

(define (make-method specializers procedure)
  (make <method>
    'specializers specializers
    'procedure    procedure))

;
; Even more convenient syntax...
;
(require 'macro) ; R5RS macros

(define-syntax define-class
  (syntax-rules ()
    ((define-class class (direct-super direct-super2 ...) (direct-slot ...)
       . options)
     (define class (make-class 'class
			       (list direct-super direct-super2 ...)
			       (list 'direct-slot ...)
			       . options)))
    ((define-class class () (direct-slot ...) . options)
     (define-class class (<object>) (direct-slot ...) . options))))

(define-syntax define-generic
  (syntax-rules ()
    ((define-generic generic . options)
     (define generic (make-generic 'generic . options)))))

(define (call-next-method . args)
  (gerror "call-next-method: not in a method"))

(require 'fluid-let)

(define-syntax define-method
  (syntax-rules ()
    ((define-method
       (gf parmspec ...)
       body ...)
     (define-method "normalize"
       gf ()                ()                (parmspec ...)
       body ...))

    ((define-method "normalize"
       gf (parm0 ...)       (spec0 ...)       ((parmi speci) parms ...)
       body ...)
     (define-method "normalize"
       gf (parm0 ... parmi) (spec0 ... speci) (parms ...)               
       body ...))

    ((define-method "normalize"
       gf (parm0 ...)       (spec0 ...)       (parmi parms ...)
       body ...)
     (define-method "normalize"
       gf (parm0 ... parmi) (spec0 ... <top>) (parms ...)
       body ...))

    ((define-method "normalize"
       gf (parm ...)        (spec ...)        ()
       body ...)
     (begin
       (add-method! gf
	 (make-method (list spec ...)
	   (lambda (cnm parm ...)
	     (fluid-let ((call-next-method cnm))
	       body ...))))	   
       'gf)
)))

;
; The initialization protocol
;
(define-generic initialize)
	    

;
; The instance structure protocol.
;
(define-generic allocate-instance)
(define-generic compute-getter-and-setter)


;
; The class initialization protocol.
;
(define-generic compute-cpl)
(define-generic compute-slots)

;
; The generic invocation protocol.
;
(define-generic compute-apply-generic)
(define-generic compute-methods)
(define-generic compute-method-more-specific?)
(define-generic compute-apply-methods)




;
; The next thing to do is bootstrap generic functions.
; 
(define generic-invocation-generics (list compute-apply-generic
					  compute-methods
					  compute-method-more-specific?
					  compute-apply-methods))


(define (add-method! generic method)
  (slot-set! generic
	     'methods
	     (cons method
		   (filter-in
		    (lambda (m)
		      (let ((s1 (method-specializers m))
			    (s2 (method-specializers method)))
			(not (and (= (length s1) (length s2))
				  (every eq? s1 s2)))))
		    (slot-ref generic 'methods))))
  (%set-entity-proc! generic (compute-apply-generic generic)))

;
; Adding a method calls COMPUTE-APPLY-GENERIC, the result of which calls
; the other generics in the generic invocation protocol.  Two, related,
; problems come up.  A chicken and egg problem and a infinite regress
; problem.
;
; In order to add our first method to COMPUTE-APPLY-GENERIC, we need
; something sitting there, so it can be called.  The first definition
; below does that.
; 
; Then, the second definition solves both the infinite regress and the
; not having enough of the protocol around to build itself problem the
; same way: it special cases invocation of generics in the invocation
; protocol.
;
;
(%set-entity-proc! compute-apply-generic
     (lambda (generic)             ;The ONE time this is called
				   ;it doesn't get cnm.
       (lambda args
	 (apply (method-procedure (car (generic-methods generic)))
		(cons #f args))))) ;But, the ONE time it is run,
				   ;it needs to pass a dummy
				   ;value for cnm!

(define-method (compute-apply-generic (generic <generic>))
  (lambda args
    (if (and (memq generic generic-invocation-generics)     ;* G  c
	     (memq (car args) generic-invocation-generics)) ;* r  a
	(apply (method-procedure                            ;* o  s
		(car (last-pair (generic-methods generic))));* u  e
	       (cons #f args))                              ;* n
					                    ;* d
	((compute-apply-methods generic)
	 ((compute-methods generic) args)
	 args))))


(define-method (compute-methods (generic <generic>))
  (lambda (args)
    (let ((applicable
	   (filter-in (lambda (method)
			(let ((specs (method-specializers method)))
			  ;;
			  ;; Note that every only goes as far as the
			  ;; shortest list!
			  ;;
;			  (and (>= (length args) (length specs))
			  (and (= (length args) (length specs))
			       (every applicable?
				      (method-specializers method)
				      args))))
		      (generic-methods generic))))
      (gsort (lambda (m1 m2)
	       ((compute-method-more-specific? generic)
		m1
		m2
		args))
	     applicable))))


(define-method (compute-method-more-specific? (generic <generic>))
  (lambda (m1 m2 args)
    (let loop ((specls1 (method-specializers m1))
	       (specls2 (method-specializers m2))
	       (args args))
      (cond ((null? specls1) #t) ;*Maybe these two
	    ((null? specls2) #f) ;*should barf?
	    ((null? args)
	     (gerror "Fewer arguments than specializers."))
	    (else
	     (let ((c1  (car specls1))
		   (c2  (car specls2))
		   (arg (car args)))
	       (if (eq? c1 c2)
		   (loop (cdr specls1)
			 (cdr specls2)
			 (cdr args))
		   (more-specific? c1 c2 arg))))))))


(define-method (compute-apply-methods (generic <generic>))
  (lambda (methods args)
    (if (null? methods)
	(gerror "No applicable method for" generic args)
	(letrec ((one-step (lambda (tail)
			     (lambda ()
			       (if (null? tail)
				   (gerror "No next method for" generic args)
				   (apply (method-procedure (car tail))
					  (cons (one-step (cdr tail))
						args)))))))
	  ((one-step methods))))))

(define (applicable? c arg)
  (or (eq? c <top>) ; this shortcuts, for efficiency, but also
		    ; avoids having to call class-of on a pair before
		    ; <pair> is defined, down below where
		    ; <primitive-class> is made...
      (memq c (class-cpl (class-of arg)))))

(define (more-specific? c1 c2 arg)
  (memq c2 (memq c1 (class-cpl (class-of arg)))))




(define-method (initialize (object <object>) initargs) object)

(define-method (initialize (class <class>) initargs)
  (call-next-method)
  (slot-set! class 'name (getl initargs 'name 'unnamed))
  (slot-set! class
	     'direct-supers
	     (getl initargs 'direct-supers '()))
  (slot-set! class
	     'direct-slots
	     (map (lambda (s)
		    (if (pair? s) s (list s)))
		  (getl initargs 'direct-slots  '())))
  (slot-set! class 'cpl   (compute-cpl   class))
  (slot-set! class 'slots (compute-slots class))
  (let* ((nfields 0)
	 (field-initializers '())
	 (allocator
	  (lambda (init)
	    (let ((f nfields))
	      (set! nfields (+ nfields 1))
	      (set! field-initializers
		    (cons init field-initializers))
	      (list (lambda (o)   (get-field  o f))
		    (lambda (o n) (set-field! o f n))))))
	 (getters-n-setters
	  (map (lambda (slot)
		 (cons (car slot)
		       (compute-getter-and-setter class
						  slot
						  allocator)))
	       (slot-ref class 'slots))))
    (slot-set! class 'nfields nfields)
    (slot-set! class 'field-initializers field-initializers)
    (slot-set! class 'getters-n-setters getters-n-setters)))

(define-method (initialize (generic <generic>) initargs)
  (call-next-method)
  (slot-set! generic 'name (getl initargs 'name 'unnamed))
  (slot-set! generic 'methods '())
  (%set-entity-proc! generic
		     (lambda args (gerror "Has no methods."))))

(define-method (initialize (method <method>) initargs)
  (call-next-method)
  (slot-set! method 'specializers (getl initargs 'specializers))
  (slot-set! method 'procedure    (getl initargs 'procedure)))



(define-method (allocate-instance (class <class>))
  (let* ((field-initializers (slot-ref class 'field-initializers))
	 (new (%allocate-instance
	       class
	       (length field-initializers))))
    (let loop ((n 0)
	       (inits field-initializers))
      (if (pair? inits)
	  (begin
	    (%instance-set! new n ((car inits)))
	    (loop (+ n 1)
		  (cdr inits)))
	  new))))

(define-method (allocate-instance (class <entity-class>))
  (let* ((field-initializers (slot-ref class 'field-initializers))
	 (new (%allocate-entity
	       class
	       (length field-initializers))))
    (let loop ((n 0)
	       (inits field-initializers))
      (if (pair? inits)
	  (begin
	    (%entity-set! new n ((car inits)))
	    (loop (+ n 1)
		  (cdr inits)))
	  new))))


(define-method (compute-cpl (class <class>))
  (compute-std-cpl class class-direct-supers))


(define-method (compute-slots (class <class>))
  (let collect ((to-process (apply append
				   (map class-direct-slots
					(class-cpl class))))
		(result '()))
    (if (null? to-process)
	(reverse result)
	(let* ((current (car to-process))
	       (name (car current))
	       (others '())
	       (remaining-to-process
		(collect-if (lambda (o)
			      (if (eq? (car o) name)
				  (begin
				    (set! others (cons o others))
				    #f)
				  #t))
			    (cdr to-process))))
	  (collect remaining-to-process
		   (cons (append current
				 (apply append (map cdr others)))
			 result))))))


(define-method (compute-getter-and-setter (class <class>) slot allocator)
  (allocator (lambda () #f)))


;
; Now everything works, both generic functions and classes, so we can
; turn on the real MAKE.
;
;
(set! make
      (lambda (class . initargs)
	(let ((instance (allocate-instance class)))
	  (initialize instance initargs)
	  instance)))

;
; Now define what CLOS calls `built in' classes.
;
;
(define-class <primitive-class> (<class>) ())

(define (make-primitive-class name . class)
  (make (if (null? class) <primitive-class> (car class))
    'name name
    'direct-supers (list <top>)
    'direct-slots  (list)))

(define <nil>       (make-primitive-class '<nil>))
(define <boolean>   (make-primitive-class '<boolean>))
(define <symbol>    (make-primitive-class '<symbol>))
(define <char>      (make-primitive-class '<char>))
(define <vector>    (make-primitive-class '<vector>))
(define <pair>      (make-primitive-class '<pair>))
(define <number>    (make-primitive-class '<number>))
(define <string>    (make-primitive-class '<string>))
(define <procedure> (make-primitive-class '<procedure> <procedure-class>))
(define <port>      (make-primitive-class '<port>))
(define <unknown-primitive-object>
                    (make-primitive-class '<unknown-primitive-object>))

;
; An object printer.
;
(set! print-object (make-generic 'print-object))
(define-method (print-object obj (port <port>))
  ;; Primitive Scheme objects.
  (display obj port))
(define-method (print-object (obj <object>) (port <port>))
  (display "#<object>" port))
(define-method (print-object (class <class>) (port <port>))
  (display "#<class " port)
  (display (class-name class) port)
  (display ">" port))
(define-method (print-object (generic <generic>) (port <port>))
  (display "#<generic " port)
  (display (generic-name generic) port)
  (display ">" port))
(define-method (print-object (method <method>) (port <port>))
  (display "#<method " port)
  (display (method-specializers method) port)
  (display ">" port))

;
; All done.
;
;

'tiny-clos-up-and-running
