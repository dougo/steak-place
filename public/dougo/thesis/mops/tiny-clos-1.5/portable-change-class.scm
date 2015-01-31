;; A portable implementation of change-class.  It uses a new class and
;; metaclass for objects whose class is allowed to be changed.  It
;; doesn't make any assumptions about the underlying instance
;; implementation.


(define <changeable-class-object-class>
  ;; We store the fields of a changeable-class-object in a vector, which
  ;; is the only field of the actual object instance.  The getter &
  ;; setter for this field is stored on the class metaobject.
  (make-class
   '<changeable-class-object-class>
   (list <class>)
   (list fields-vector-getter-and-setter-slot-name)))

(define-method (compute-getter-and-setter
		(class <changeable-class-object-class>)
		slot allocator)
  (let* ((fields-vector-get
	  (let ((old (slot-ref class 'fields-vector-getter-and-setter)))
	    (car (or old
		     (let ((new (allocator
				 (lambda ()
				   (make-vector
				    (length (class-slots class)))))))
		       (slot-set! class 'fields-vector-getter-and-setter new)
		       new)))))
	 (f (position-of slot (class-slots class))))
    (list (lambda (o) (vector-ref (fields-vector-get o) f))
	  (lambda (o new) (vector-set! (fields-vector-get o) f new)))))

(define-generic change-class!)
(define-method (change-class! (object <object>)
			      (class <changeable-class-object-class>)
			      initargs)
  (change-class! object (class-of object) class))
(define-method (change-class! (old-instance <object>)
			      (old-class <changeable-class-object-class>)
			      (new-class <changeable-class-object-class>)
			      initargs)
  ;; Make a new instance, copy the fields, then swap the fiends vector
  ;; of the old and new to preserve object identity of the old.
  (let ((new-instance (allocate-instance new-class)))
    (for-each (lambda (slot)
		(let ((slot-name (car slot)))
		  (when (slot-exists? slot-name old-instance)
			(slot-set! new-instance slot-name
				   (slot-ref old-instance slot-name)))))
	      (class-slots new-class))
    (swap-slots! old-instance new-instance
		 (slot-ref old-class 'fields-vector-getter-and-setter)
		 (slot-ref new-class 'fields-vector-getter-and-setter))
    (swap-slots! old-instance new-instance
		 (lookup-slot-info old-class 'class)
		 (lookup-slot-info new-class 'class)



(define-class <shape> () (x y)
  'metaclass <changeable-class-object-class>)
(define-class <ellipse> (<shape>) (width height)
  'metaclass <changeable-class-object-class>)
(define-class <circle> (<shape>) (width)
  'metaclass <changeable-class-object-class>)
(define c (make <ellipse>))
(slot-set! c 'x 0) (slot-set! c 'y 0)
(slot-set! c 'width 10) (slot-set! c 'height 10)
