;; This is meant to be part of the MOP-- it uses backstage functions,
;; e.g. %set-instance-class!.

;; FIXME: modify entity implementation to support a similar function
;; for entities.

(define-generic change-class!)

;; fix this when we have methods with rest args.
;(define-method (change-class! (old-instance <object>) (new-class <class>)
;			     . initargs)
(add-method! change-class!
  (make-method (list <object> <class>)
    (lambda (call-next-method instance new-class . initargs)
      (let ((unchanged-slot-values-alist
	     (remove
	      #f
	      (map (lambda (slot)
		     (let ((slot-name (car slot)))
		       (and (slot-exists? slot-name instance)
			    (cons slot-name
				  (slot-ref instance slot-name)))))
		   (class-slots new-class)))))
	(update-instance-for-different-class
	 (reallocate-instance! instance new-class
			       unchanged-slot-values-alist)
	 instance
	 initargs)
	instance))))

;; Reallocates the fields vector, returning a newly-allocated instance
;; with the old fields vector.
(define-generic reallocate-instance!)
(define-method (reallocate-instance!
		(instance <object>) (class <class>)
		unchanged-slot-values-alist)
  (let* ((instance-with-old-fields (allocate-instance class)))
    (for-each (lambda (pair)
		(slot-set! instance-with-old-fields (car pair) (cdr pair)))
	      unchanged-slot-values-alist)
    (swap-fields! instance instance-with-old-fields
		  %instance-fields %set-instance-fields!)
    (swap-fields! instance instance-with-old-fields
		  %instance-class %set-instance-class!)
    instance-with-old-fields))

;; Like CL's rotatef.
(define (swap-fields! a b field-ref field-set!)
  (let ((a-field (field-ref a)))
    (field-set! a (field-ref b))
    (field-set! b a-field)))

(define-generic slot-exists?)
(define-method (slot-exists? slot-name obj)
  (assq slot-name (class-slots (class-of obj))))

(define-generic update-instance-for-different-class)
(define-method (update-instance-for-different-class
		(old <object>) (new <object>) initargs)
  ;; Nothing to do here, since slots have no 'initform in the base
  ;; MOP.  But it's a good hook to have.
  new)

      
(if #f
    (let ()
(define-class <shape> () (x y))
(define-class <ellipse> (<shape>) (width height))
(define-class <circle> (<shape>) (width))
(define c (make <ellipse>))
(slot-set! c 'x 0) (slot-set! c 'y 0)
(slot-set! c 'width 10) (slot-set! c 'height 10)
(change-class! c <circle>)
(define-method (update-instance-for-different-class
		(old <circle>) (new <ellipse>) initargs)
  (slot-set! new 'height (slot-ref old 'width)))
))