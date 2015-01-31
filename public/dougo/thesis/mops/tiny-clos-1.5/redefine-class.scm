(require "change-class")
(require 'common-list-functions)	; set-difference, remove, remove-if

;; A redefinable class has to keep track of its subclasses, so their
;; cpl's and slots can be recalculated, and its instances, so their
;; fields can be reallocated.
(define-class <redefinable-class> (<class>) (direct-subclasses instances))

(define-method (initialize (class <redefinable-class>) initargs)
  (call-next-method)
  ;; FIXME: These two should be weak pointers, since there's no way to
  ;; know when to remove elements.
  (slot-set! class 'direct-subclasses '())
  (slot-set! class 'instances '())
  (for-each (lambda (super) (add-direct-subclass! super class))
	    (class-direct-supers class))
  class)

(define-method (update-instance-for-different-class
		(old-class <class>) (new-class <redefinable-class>)
		initargs)
  (slot-set! new-class 'direct-subclasses '())
  (slot-set! new-class 'instances '()))

(define-generic add-direct-subclass!)
(define-generic remove-direct-subclass!)

(define-method (add-direct-subclass! (super <class>) subclass)
  'ignored)
(define-method (remove-direct-subclass! (super <class>) subclass)
  'ignored)

(define-method (add-direct-subclass! (super <redefinable-class>) subclass)
  (slot-set! super 'direct-subclasses
	     (adjoin subclass  (slot-ref super 'direct-subclasses))))
(define-method (remove-direct-subclass! (super <redefinable-class>) subclass)
  (slot-set! super 'direct-subclasses
	     (remove subclass (slot-ref super 'direct-subclasses))))

(define-generic add-instance!)
(define-generic remove-instance!)

(define-method (add-instance! (class <class>) (instance <object>))
  'ignored)
(define-method (remove-instance! (class <class>) (instance <object>))
  'ignored)

(define-method (add-instance! (class <redefinable-class>) (instance <object>))
  (slot-set! class 'instances
	     (adjoin instance (slot-ref class 'instances))))
(define-method (remove-instance! (class <redefinable-class>)
				 (instance <object>))
  (slot-set! class 'instances
	     (remove instance (slot-ref class 'instances))))

(define-method (allocate-instance (class <redefinable-class>))
  (let ((instance (call-next-method)))
    (add-instance! class instance)
    instance))

(define-method (reallocate-instance!
		(instance <object>) (class <redefinable-class>)
		unchanged-slot-values-alist)
  (if (not (eq? class (class-of instance)))
      (begin
	(remove-instance! (class-of instance) instance)
	(add-instance! class instance)))
  (let ((fake-instance (call-next-method)))
    (remove-instance! class fake-instance)
    fake-instance))

(define-generic set-direct-supers!)
(define-generic set-direct-slots!)
;; These add to the end.
(define-generic add-direct-supers!)
(define-generic add-direct-super!)
(define-generic add-direct-slots!)
(define-generic add-direct-slot!)
(define-generic remove-direct-super!)
(define-generic remove-direct-supers!)
;; These take slot names.
(define-generic remove-direct-slot!)
(define-generic remove-direct-slots!)

(define-method (set-direct-supers! (class <redefinable-class>) supers)
  (reinitialize class (list 'direct-supers supers)))
(define-method (set-direct-slots! (class <redefinable-class>) slots)
  (reinitialize class (list 'direct-slots slots)))
(define-method (add-direct-supers! (class <redefinable-class>) supers)
  (set-direct-supers! class (append (class-direct-supers class) supers)))
(define-method (add-direct-super! (class <redefinable-class>) super)
  (add-direct-supers! class (list super)))
(define-method (add-direct-slots! (class <redefinable-class>) slots)
  (set-direct-slots! class (append (class-direct-slots class) slots)))
(define-method (add-direct-slot! (class <redefinable-class>) slot)
  (add-direct-slots! class (list slot)))
(define-method (remove-direct-supers! (class <redefinable-class>) supers)
  (set-direct-supers! class (set-difference (class-direct-supers class)
					    supers)))
(define-method (remove-direct-super! (class <redefinable-class>) super)
  (remove-direct-supers! class (list super)))
(define-method (remove-direct-slots! (class <redefinable-class>) slots)
  (set-direct-slots! class (remove-if (lambda (slot)
					(memq (car slot) slots))
				      (class-direct-slots class))))
(define-method (remove-direct-slot! (class <redefinable-class>) slot)
  (remove-direct-slots! class (list slot)))



(define-generic reinitialize)

(define-method (reinitialize (object <object>) initargs) object)

(define-method (reinitialize (class <redefinable-class>) initargs)
  (define (shallow-equal? l1 l2)
    ;; Can't use (every eq? l1 l2), because that allows lists of
    ;; different length...
    (cond ((null? l1) (null? l2))
	  ((null? l2) #f)
	  (else (and (eq? (car l1) (car l2))
		     (shallow-equal? (cdr l1) (cdr l2))))))
  (define (remove-properties props proplist)
    (cond ((null? proplist) '())
	  ((memq (car proplist) props)
	   (remove-properties props (cddr proplist)))
	  (else
	   (list* (car proplist) (cadr proplist)
		  (remove-properties props (cddr proplist))))))
  (let* ((metaclass (class-of class))
	 (new-metaclass (getl initargs 'metaclass metaclass))
	 (direct-supers (class-direct-supers class))
	 (new-direct-supers (getl initargs 'direct-supers direct-supers))
	 (direct-slots (class-direct-slots class))
	 (new-direct-slots
	  (map (lambda (slot)
		 (if (pair? slot) slot (list slot)))
	       (getl initargs 'direct-slots direct-slots)))
	 (direct-slot-names (map car direct-slots))
	 (new-direct-slot-names (map car new-direct-slots))
	 (slot-names (map car (class-slots class)))
	 (getters-n-setters (slot-ref class 'getters-n-setters))
	 ;; Have to remember these up here because we might be
	 ;; changing the metaclass to something that's not a subclass
	 ;; of <redefinable-class>...
	 (instances (slot-ref class 'instances))
	 (direct-subclasses (slot-ref class 'direct-subclasses)))
    (if (not (eq? new-metaclass metaclass))
	(begin
	  (apply change-class! class new-metaclass initargs)
	  ;; ??
	  ))
    (if (not (shallow-equal? new-direct-supers direct-supers))
	(begin
	  (slot-set! class 'direct-supers new-direct-supers)
	  (for-each (lambda (super) (remove-direct-subclass! super class))
		    (set-difference direct-supers new-direct-supers))
	  (for-each (lambda (super) (add-direct-subclass! super class))
		    (set-difference new-direct-supers direct-supers))
	  (slot-set! class 'cpl #f)	;force cpl to be recalculated
	  ))
    ;; Note only the names of the slots are compared.  Also note
    ;; that it detects changes in ordering.
    (if (not (equal? new-direct-slot-names direct-slot-names))
	(begin
	  (slot-set! class 'direct-slots new-direct-slots)
	  (slot-set! class 'slots #f)	;force slots to be recalculated
	  ))
    (if (not (and (class-cpl class) (class-slots class)))
	(begin
	  (finalize-inheritance class)
	  (make-instances-obsolete class instances 
				   slot-names getters-n-setters initargs)
	  (let ((subclass-initargs
		 (remove-properties '(metaclass direct-supers direct-slots)
				    initargs)))
	    (for-each (lambda (subclass)
			(slot-set! subclass 'cpl #f)
			(reinitialize subclass subclass-initargs))
		      direct-subclasses))))    
    class))

;; This is a misleading name, but it's what CLOS uses.  It actually
;; makes instances current, by reallocating their slots and updating
;; them.
(define-generic make-instances-obsolete)

(define-method (make-instances-obsolete (new-class <class>) instances
					old-slot-names old-getters-n-setters
					initargs)
  (let* ((new-slots (class-slots new-class))
	 (new-slot-names (map car new-slots)))
    (if (not (equal? old-slot-names new-slot-names))
	(let ((added-slot-names
	       (set-difference new-slot-names old-slot-names)))
	  (for-each
	   (lambda (instance)
	     (let loop ((old-slot-names old-slot-names)
			(unchanged-slot-values-alist '())
			(removed-slot-values-alist '()))
	       (if (not (null? old-slot-names))
		   (let* ((old-slot-name (car old-slot-names))
			  (old-getter (cadr (assq old-slot-name
						  old-getters-n-setters)))
			  (old-slot-value (old-getter instance))
			  (pair (cons old-slot-name old-slot-value)))
		     (if (memq old-slot-name new-slot-names)
			 (loop (cdr old-slot-names)
			       (cons pair unchanged-slot-values-alist)
			       removed-slot-values-alist)
			 (loop (cdr old-slot-names)
			       unchanged-slot-values-alist
			       (cons pair removed-slot-values-alist))))
		   (begin
		     (reallocate-instance!
		      instance
		      new-class
		      unchanged-slot-values-alist)
		     (update-instance-for-redefined-class
		      instance
		      added-slot-names
		      removed-slot-values-alist
		      initargs)))))
	   instances)))
    new-class))


;; This should go in the base MOP, and be called from initialize on <class>.
(define-generic finalize-inheritance)
(define-method (finalize-inheritance (class <class>))
  (if (not (class-cpl class))
      (begin
	(slot-set! class 'cpl (compute-cpl class))
	(slot-set! class 'slots #f)))
  (if (not (class-slots class))
      (begin
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
	  (slot-set! class 'getters-n-setters getters-n-setters)))))


(define-generic update-instance-for-redefined-class)

(define-method (update-instance-for-redefined-class
		(instance <object>)
		added-slot-names
		removed-slot-values-alist
		initargs)
  ;; Nothing to do here-- another hook for user extensions.
  instance)

(define-method (update-instance-for-redefined-class
		(class <redefinable-class>)
		added-slot-names
		removed-slot-values-alist
		initargs)
  (call-next-method)
  (if (memq 'direct-subclasses added-slot-names)
      (slot-set! class 'direct-subclasses '()))
  (if (memq 'instances added-slot-names)
      (slot-set! class 'instances '())))


(define-class <shape> () (x y))
(define-class <circle> (<shape>) (width))
(define c (make <circle>))
(slot-set! c 'x 0) (slot-set! c 'y 0) (slot-set! c 'width 10)

(change-class! <shape> <redefinable-class>)
(add-direct-subclass! <shape> <circle>)
(change-class! <circle> <redefinable-class>)
(add-instance! <circle> c)

(define-class <ellipse> (<shape>) (width height)
  'metaclass <redefinable-class>)
(define e (make <ellipse>))
(slot-set! e 'x 0) (slot-set! e 'y 0)
(slot-set! e 'width 10) (slot-set! e 'height 8)

(define-class <colored> () (color))
;; These could all be done with an :initform.
(define-method (initialize (obj <colored>) initargs)
  (call-next-method)
  (slot-set! obj 'color 'black))
(define-method (update-instance-for-different-class
		(old <object>) (new <colored>) initargs)
  (call-next-method)
  (slot-set! new 'color 'black))
(define-method (update-instance-for-redefined-class
		(obj <colored>) added removed initargs)
  (call-next-method)
  (if (memq 'color added)
      (slot-set! obj 'color 'black)))

;(add-direct-super! <shape> <colored>)
(set-direct-supers! <shape> (list <colored>))

(define-method (update-instance-for-redefined-class
		(obj <shape>) added removed initargs)
  (call-next-method)
  (if (memq 'z added)
      (slot-set! obj 'z 0)))

(add-direct-slot! <shape> 'z)
