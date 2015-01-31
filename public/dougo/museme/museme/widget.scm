;;;----------------------------------------------------------------------------
;;; WIDGET.SCM
;;;
;;; Change log:
;;; * 11/22/94 (lyn)
;;;   + Fixed SET-NAME!
;;;----------------------------------------------------------------------------

; MUSEME widget (the base for everything in MUSEME)

;;; widget definition

(define-record-type MUSEME-widget :widget
  (really-make-widget uid key name aliases handlers)
  widget?
  (uid uid)
  (name name really-set-name!)
  (aliases aliases really-set-aliases!)
  (description description really-set-description!)
  (describer describer really-set-describer!)
  (key widget-key)
  (handlers widget-handlers set-widget-handlers!))

(define-record-discloser :widget
  (lambda (w)
    (cond ((person? w) (list "Person" (uid w) (name w)))
	  ((thing? w) (list "Thing" (uid w) (name w)))
	  ((room? w) (list "Room" (uid w) (name w)))
	  (else (list "Widget" (uid w) (name w))))))

;;; key definition

(define-record-type MUSEME-key :key
  (really-make-key name type limits)
  key?
  (name key-name)
  (type key-type)
  (limits key-limits set-key-limits!))

(define-record-discloser :key
  (lambda (k)
    (case (key-type k)
      ((master) (list "Master Key"))
      ((limited) (list "Limited Key:" (key-name k)))
      (else (list "Key:" (key-name k))))))

(define master-key (really-make-key "master" 'master '() ))

(define (make-key name) (really-make-key name 'normal '() ))

(define (limited widget key)
  (ensure-my-widget widget key)
  (really-make-key widget 'limited '() ))

;;; WIDGET ROUTINES

(define (my-widget? w key)
  (or (eq? key (widget-key w))
      (eq? (key-type key) 'master)))

(define (ensure-my-widget w key)
  (if (not (my-widget? w key)) (error "not owner" w)))

;;; create/fetch/destroy

(define last-uid 0)
(define widgetlist '())

(define (make-widget key name)
  (ensure-is-a string? name "string" "name")
  (ensure-is-a key? key "key")
  (set! last-uid (+ 1 last-uid))
  (let ((w (really-make-widget last-uid key name '() '() )))
    (set! widgetlist (cons w widgetlist))
    w))

(define (fetch-widget num)
  (if (<= num last-uid)
      (list-ref widgetlist (- last-uid num))
      (error "No such widget" num)))

;;; names/uids

(define (set-name! w key nm)
  (ensure-my-widget w key)
  (if (not (string? nm)) (error "name must be a string" w nm))
  (really-set-name! w nm))

(define (set-aliases! w key aliases)
  (ensure-my-widget w key)
  (ensure-is-a list? aliases "list" "aliases")
  (really-set-aliases! w aliases))

(define (cname w)
  (with-no-handler (lambda foo
		     (let ((n (name w)))
		       (string-append (string (char-upcase (string-ref n 0)))
				      (substring n 1 (string-length n))))
		     (lambda () (apply-handler w 'cname)))))
  
(define (aname w)
  (with-no-handler (lambda foo
		     (let ((n (name w)))
		       (if (memq (string-ref n 0)
				 '(#\a #\e #\i #\u #\A #\E #\I #\O #\U))
			   (string-append "an " n)
			   (string-append "a " n))))
		   (lambda () (apply-handler w 'aname))))

(define (thename w)
  (with-no-handler (lambda foo (string-append "the " (name w)))
		   (lambda () (apply-handler w 'thename))))

;;; description

(define (set-description! w key desc)
  (ensure-my-widget w key)
  (ensure-is-a string? desc "string" "description")
  (really-set-description! w desc))

(define (set-describer! w key descer)
  (ensure-my-widget w key)
  (ensure-is-a procedure? descer "procedure" "describer")
  (really-set-describer! w descer))

(define (describe w observer)
  ((describer w) observer))

;;; handlers

(define (ensure-can-use-handler w k tag)
  (cond ((my-widget? w k) #t)
	((eq? w (key-name k)) (or (memq tag (key-limits k))
				  (not (assq tag (widget-handlers w)))))
	(else (error "bad key while trying to reference handler" w k tag))))

(define (maybe-add-to-key-limits k tag)
  (if (and (eq? (key-type k) 'limited)
	   (not (memq tag (key-limits k))))
      (set-key-limits! k (cons tag (key-limits k)))))

(define (set-pub-handler! widget key tag handler)
  (ensure-can-use-handler widget key tag)
  (maybe-add-to-key-limits key tag)
  (let ((h (assq tag (widget-handlers widget))))
    (set-widget-handlers! widget (cons (list tag handler #t)
				       (delq h (widget-handlers widget))))))

(define (set-prv-handler! widget key tag handler)
  (ensure-can-use-handler widget key tag)
  (maybe-add-to-key-limits key tag)
  (let ((h (assq tag (widget-handlers widget))))
    (set-widget-handlers! widget (cons (list tag handler #f)
				       (delq h (widget-handlers widget))))))

(define (apply-handler widget tag . args)
  (ensure-is-a widget? widget "widget")
  (if (key? tag)
      (if (null? args)
	  (error "no handler given")
	  (apply-prv-handler widget tag (car args) (cdr args)))
      (lookup tag (widget-handlers widget)
	      (lambda (handler pub?)
		(if (procedure? handler)
		    (apply handler args)
		    handler))
	      (lambda () ((fluid no-handler) widget tag args)))))

(define (apply-prv-handler widget key tag args)
  (lookup tag (widget-handlers widget)
	  (lambda (handler pub?)
	    (ensure-can-use-handler widget key tag)
	    (if (procedure? handler)
		(apply handler args)
		handler))
	  (lambda () ((fluid no-handler) widget tag args))))


;;; is-a
(define (is-a pred widget)
  (and (widget? widget)
       (with-no-handler (lambda foo #f)
			(lambda () (apply-handler widget pred)))))


;;; no-handler

(define no-handler (make-fluid (lambda (widget handler args)
				 (error "no handler" widget handler args))))

(define (with-no-handler nh thunk)
  (let-fluid no-handler nh
      thunk))
