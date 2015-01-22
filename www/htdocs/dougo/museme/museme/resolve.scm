; resolve strings into object references

(define (resolve-for-person ref person key)
  (cond ((string-ci=? ref "me") person)
	((string-ci=? ref "here") (location person))
	(else
	 (let* ((person-contents (transitive-contents person))
		; ^^^ everything person is carrying
		(person-location (location person))
		(person-context (context person key))
		; ^^^ all of the other things you've noticed in the room
		(all (append person-contents person-context
			     (list person-location)))
		(m (remove-duplicates (filter
				       (lambda (elt)
					 (inside? elt person-location))
				       (matches ref all)))))
	   (cond ((null? m) (muserror "I don't know what this is:" ref))
		 ((pair? m) (if (null? (cdr m))
				(car m)
				(muserror "Which do you mean?" m)))
		 (else m))))))

(define (resolve-in-room ref room)
  (let ((m (matches ref (contents room))))
    (cond ((null? m) (muserror "I don't know what this is:" ref))
	  ((pair? m) (if (null? (cdr m))
			 (car m)
			 (muserror "Which do you mean?" m)))
	  (else m))))

(define (matches ref lst)
  (let ((m (filter (lambda (w) (string-ci=? ref (name w))) lst)))
    (if (null? m)
	(filter (lambda (w) (any-match? ref (aliases w))) lst)
	m)))

(define (any-match? text list)
  (cond ((null? list) #f)
	((string-ci=? text (car list)) #t)
	(else (any-match? text (cdr list)))))

(define (remove-duplicates lst)
  (if (null? lst)
      lst
      (cons (car lst)
	    (remove-duplicates (delq (car lst) (cdr lst))))))
