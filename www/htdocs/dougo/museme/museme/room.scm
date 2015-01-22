; MUSEME room

(define (make-room key myname)
  (let ((w (make-widget key myname)))
    (set-prv-handler! w key 'exits '() )
    (set-pub-handler! w key 'can-go?
		      (lambda (direction who)
			(let* ((e (assq direction
					(apply-handler w key 'exits)))
			       (ex (if e (cadr e) #f)))
			  (cond ((room? ex) ex)
				((string? ex) (print ex) #f)
				((procedure? ex) (ex who))
				(else (print "You can't go that way.") #f)))))
    (set-description! w key "a room")
    (set-describer! w key
		    (lambda (observer)
		      (print (name w))
		      (print (description w))
		      (list-stuff "You can go " (lambda (e)
						  (symbol->string (car e)))
				  (filter visible-exit?
					  (apply-handler w key 'exits))
				  "." "There are no visible exits.")
		      (let* ((p/t (bifilter person? (delq observer (contents w))))
			     (things (cdr p/t))
			     (people (car p/t)))
			(list-stuff "You see " aname things " here.")
			(list-stuff "" name people (if (> (length people) 1)
						       " are here."
						       " is here.")))
		      (add-context observer (contents w))
		      ))
    (set-pub-handler! w key 'room? #t)

    (set-pub-handler! w key 'aname (lambda () (name w)))
    (set-pub-handler! w key 'thename (lambda () (name w)))

    (make-frob-holder w (limited w key))
    w))

(define (room? x)
  (is-a 'room? x))

(define (can-go? room direction who)
  (apply-handler room 'can-go? direction who))

(define (make-exit room key direction dest . vis?)
  (let* ((vis? (if (null? vis?)
		   #t
		   (car vis?)))
	 (exits (apply-handler room key 'exits))
	 (match (assq direction exits)))
    (set-prv-handler! room key 'exits (cons (list direction dest vis?)
					    (delq match exits)))))

(define visible-exit? caddr)

(define nowhere (make-room (make-key "nowhere") "nowhere"))
