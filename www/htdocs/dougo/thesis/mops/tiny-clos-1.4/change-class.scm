;; An implementation of change-class!, taken almost verbatim from AMOP
;; (sec. 1.5.5).  This is meant to be part of the MOP-- it uses
;; backstage functions, e.g. %instance-class.  See
;; portable-change-class.scm for a portable (in the sec. 5.3.1 sense)
;; definition using a new class and metaclass.

;; FIXME: modify entity implementation to support a similar function
;; for entities.

(define-generic change-class!)

;; fix this when we have methods with rest args.
;(define-method (change-class! (old-instance <object>) (new-class <class>)
;			     . initargs)
(add-method change-class!
  (make-method (list <object> <class>)
    (lambda (call-next-method old-instance new-class . initargs)
      (let ((new-instance (allocate-instance new-class)))
	(for-each (lambda (slot)
		    (let ((slot-name (car slot)))
		      (when (slot-exists? slot-name old-instance)
			    (slot-set! new-instance slot-name
				       (slot-ref old-instance slot-name)))))
		  (class-slots new-class))
	(swap-fields! old-instance new-instance
		      %instance-fields %set-instance-fields!)
	(swap-fields! old-instance new-instance
		      %instance-class %set-instance-class!)
	(apply update-instance-for-different-class
	       new-instance old-instance initargs)
	old-instance))))

;; Like CL's rotatef.
(define (swap-fields! a b field-get field-set!)
  (let ((a-field (field-get a)))
    (field-set! a (field-get b))
    (field-set! b a-field)))

(define-generic slot-exists?)
(define-method (slot-exists? slot-name obj)
  (assq slot-name (class-slots (class-of obj))))

(define-generic update-instance-for-different-class)
;(define-method (update-instance-for-different-class
;		(old <object>) (new <object>) . initargs)
(add-method update-instance-for-different-class
  (make-method (list <object> <object>)
    (lambda (call-next-method old new . initargs)
      ;; Nothing to do here, since there's no re-initialization or
      ;; partial initialization in the base MOP.  But it's a good hook
      ;; to have.
      new)))

      
(when #f
(define-class <shape> () (x y))
(define-class <ellipse> (<shape>) (width height))
(define-class <circle> (<shape>) (width))
(define c (make <ellipse>))
(slot-set! c 'x 0) (slot-set! c 'y 0)
(slot-set! c 'width 10) (slot-set! c 'height 10)
(change-class c <circle>)
(define-method (update-instance-for-different-class
		(old <circle>) (new <ellipse>))
  (slot-set! new 'height (slot-ref old 'width)))
)