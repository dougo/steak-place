; MUSEME person

(define person?-msg '(person?))

(define (make-person key nm gender home)
  (if (register-person-name nm)
      (let ((w (make-widget key nm)))
	(set-description! w key "a person")
	(set-describer! w key
			(lambda (observer)
			  (print (description w))
			  (list-stuff (string-append (name w) " is carrying ")
				      aname (contents w) ".")
			  (add-context observer (contents w))
			  ))
	(set-pub-handler! w key 'gender gender)
	(set-prv-handler! w key 'output-port #f)
	(set-prv-handler! w key 'write-me
			  (lambda (text)
			    (let ((op (apply-handler w key 'output-port)))
			      (if op
				  (begin
				    (display text op)
				    (newline op))))))

	(set-prv-handler! w key 'context '() )
	(set-pub-handler! w key 'add-context
			  (lambda (context-list)
			    (ensure-is-a list? context-list "list" "context")
			    (set-prv-handler! w key 'context
					      (append context-list (apply-handler w key 'context)))))

	(set-pub-handler! w key person?-msg #t)
	
	(set-pub-handler! w key 'home home)
	
	(set-pub-handler! w key 'aname (lambda () (name w)))
	(set-pub-handler! w key 'thename (lambda () (name w)))
	
	(set-pub-handler! w key 'accept?
			  (lambda (thing src)
			    (if (person? (location thing))
				(apply-handler w key 'write-me
					       (string-append (name src)
							      " gives you "
							      (aname thing)
							      ".")))
			    #t))
	(set-pub-handler! w key 'yield?
			  (lambda (thing dest)
			    (print (name w) " won't let you take it.")
			    (apply-handler w key 'write-me
					   (string-append (name dest)
							  " tries to take your "
							  (name thing) "."))
	    #f))
	
	(make-frob w (limited w key) home #t)
	(make-frob-holder w (limited w key))
	(make-frob-mover w (limited w key))
	(make-listener w (limited w key) (lambda (src text)
					   (apply-handler w key
							  'write-me text)))
	(make-talker w (limited w key))
	w)
      (error "that name is already taken" nm)))

(define (person? x)
  (is-a person?-msg x))

(define all-people-names '() )

(define (register-person-name nm)
  (if (member nm all-people-names)
      #f
      (begin (set! all-people-names (cons nm all-people-names))
	     #t)))

(define male '#(male "he" "his" "his" "him" "himself"))
(define female '#(female "she" "her" "hers" "her" "herself"))
(define neuter '#(neuter "it" "its" "its" "it" "itself"))

(define (context person key)
  (apply-handler person key 'context))

(define (add-context person context)
  (with-no-handler (lambda foo #t)
		   (lambda () (apply-handler person 'add-context context))))

(define (clear-context person key)
  (set-prv-handler! person key 'context '() ))

(define (home person)
  (apply-handler person 'home))

(define (set-home! person key home)
  (set-pub-handler! person key 'home home))
