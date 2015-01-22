 ; trusted routines to echo speech/actions to listeners/watchers

; secret handler tag
(define Echo-write '(write))
(define Echo-listener '(listener))

(define (make-listener listener key listen-method)
  (set-pub-handler! listener key Echo-write listen-method)
  (set-pub-handler! listener key Echo-listener #t))

(define (make-talker talker key)
  (set-prv-handler! talker key 'echo
		    (lambda (text . quiet?)
		      (let ((output (if (eq? (string-ref text 0) #\')
					(string-append (name talker) text)
					(string-append (name talker) " " text))))
			(Echo talker output)
			(if (null? quiet?)
			    (apply-handler talker Echo-write talker output)))))
  (set-prv-handler! talker key 'echo-to
		    (lambda (char text)
		      (apply-handler char Echo-write talker
				     (string-append (name talker) text)))))
      
(define (audience place)
  (filter (lambda (elt)
	    (with-no-handler (lambda foo #f)
			     (lambda () (apply-handler elt Echo-listener))))
	  (contents place)))

; actual routine to write stuff to multiple listeners:
(define (Echo src text)
  (for-each (lambda (obj)
	      (with-no-handler (lambda foo #t)
			       (lambda () (apply-handler obj Echo-write
							 src text))))
	    (delq src (contents (location src)))))
