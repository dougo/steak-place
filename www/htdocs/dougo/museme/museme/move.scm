; trusted routines to oversee moving objects

; `secret' handler tags
(define Move-location '(location))
(define Move-set-location! '(set-location!))
(define Move-contents '(contents))
(define Move-set-contents! '(set-contents!))

(define (make-frob frob key start-loc mobile?)
  (set-pub-handler! frob key Move-location nowhere)
  (set-pub-handler! frob key Move-set-location!
		    (lambda (loc) (set-pub-handler! frob key
						    Move-location loc)))
  (set-pub-handler! frob key 'mobile? mobile?)
  (if (accept? start-loc frob nowhere)
      (Move frob nowhere start-loc)
      (error "could not install frob" frob start-loc)))

(define (location frob)
  (with-no-handler (lambda foo (error "Not a frob" frob))
		   (lambda () (apply-handler frob Move-location))))

(define (frob? widget)
  (with-no-handler (lambda foo #f)
		   (lambda () (apply-handler widget Move-location))))

(define (mobile? frob)
  (apply-handler frob 'mobile?))

(define (make-frob-holder frob-holder key)
  (set-pub-handler! frob-holder key Move-contents '() )
  (set-pub-handler! frob-holder key Move-set-contents!
		    (lambda (cont)
		      (set-pub-handler! frob-holder key Move-contents cont))))

(define (contents frob-holder)
  (with-no-handler (lambda foo (error "Not a frob-holder" frob-holder))
		   (lambda () (apply-handler frob-holder Move-contents))))

(define (frob-holder? widget)
  (with-no-handler (lambda foo #f)
		   (lambda () (apply-handler widget Move-contents))))

(define (transitive-contents fh)
  (let ((c (with-no-handler (lambda foo '() )
			    (lambda () (apply-handler fh Move-contents)))))
    (for-each (lambda (elt) (set! c (append (transitive-contents elt) c))) c)
    c))

(define (inside? frob holder)
  (cond ((not (frob? frob)) #f)
	((eq? (location frob) holder) #t)
	(else (inside? (location frob) holder))))

(define (make-frob-mover frob-mover key)
  (if (frob? frob-mover)
      (set-prv-handler! frob-mover key 'move-self
	(lambda (dest) (move-asking-to frob-mover (location frob-mover) dest))))
  (if (frob-holder? frob-mover)
      (begin
	(set-prv-handler! frob-mover key 'get
			  (lambda (frob) (move-asking-from frob
							   (location frob)
							   frob-mover)))
	(set-prv-handler! frob-mover key 'put
			  (lambda (frob dest)
			    (if (not (inside? frob frob-mover))
				(error "frob not in holder while trying to 'put"
				       frob frob-mover))
			    (move-asking-to frob frob-mover dest))))))

; move what from src to dest, if dest will accept it
(define (move-asking-to what src dest)
  (if (accept? dest what src)
      (Move what src dest)
      #f))

; move what from src to dest, if src will yield it
(define (move-asking-from what src dest)
  (if (yield? src what dest)
      (Move what src dest)
      #f))


; this should be atomic
(define (Move frob from to)
  (if (and (not (mobile? frob))
	   (not (eq? from nowhere)))
      (muserror "You can't move that." frob))
  (if (inside? to frob) (muserror (string-append (thename to)
					      " is already inside "
					      (thename frob))))
  (apply-handler frob Move-set-location! to)
  (apply-handler from Move-set-contents!
	       (delq frob (apply-handler from Move-contents)))
  (apply-handler to Move-set-contents!
	       (cons frob (apply-handler to Move-contents))))



; errorless messages:
(define (accept? dest what src)
  (with-no-handler (lambda foo #t)
		   (lambda () (apply-handler dest 'accept? what src))))

(define (yield? src what dest)
  (with-no-handler (lambda foo #t)
		   (lambda () (apply-handler src 'yield? what dest))))
