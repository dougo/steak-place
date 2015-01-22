;;;----------------------------------------------------------------------------
;;; OBJECT.SCM
;;; Encapsulates a Scheme value as frob 
;;; (NOTE: TRANSLATE.SCM already does something like this!)
;;;
;;; Created: 11/18/94 (lyn)
;;; Change log:
;;;----------------------------------------------------------------------------

;; `secret' handler tags
(define Object-Value '(value))

(define (make-object key val name location mobile? . desc)
  ;; VAL is any scheme value
  (let ((w (make-widget key name)))
    (set-description! w key 
		      (if (null? desc) 
			  "an object"
			  (let ((str (car desc)))
			    (if (string? str)
				str
				(error "MAKE-OBJECT: Not a string" str)))))
    (set-describer! w key (lambda (observer) (description w)))
    (set-pub-handler! w key 'object? #t)
    (make-frob w (limited w key) location mobile?)
    (set-pub-handler! w key Object-Value val)
    w))

(define (value obj)
  (with-no-handler (lambda foo (error "Not an object" obj))
		   (lambda () (apply-handler obj Object-Value))))

(define (object? x)
  (is-a 'object? x))
