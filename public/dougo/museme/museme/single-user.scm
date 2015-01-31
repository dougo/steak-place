;;;----------------------------------------------------------------------------
;;; SINGLE-USER.SCM
;;; Run the server in single user mode so that is easier to debug.
;;;
;;; Change log:
;;; * 11/23/94 (BJR)
;;;   + Adapted from server.scm
;;;----------------------------------------------------------------------------

(define (entry-point . args)
  ;; Need to run in seperate thread so locks and all work okay.
  (with-multitasking
   (lambda ()
     (spawn (lambda () 
	      (login (current-input-port) (current-output-port)))))))

;; entry point (OLD -- BJR)
(define (script-entry . args)
  (with-multitasking
   (lambda () (accept-connections (open-socket 6001)))))

(define (accept-connections socket)
  (call-with-values
   (lambda () (socket-listen socket))
   (lambda (in out)
     (if in (spawn (lambda ()
		     (login in out)
		     (if in (close-input-port in))
		     (if out (close-output-port out)))))
     (accept-connections socket))))

(define cu-lock (make-lock))
(define connected-users '() )
(define (who) connected-users) ;; lyn. Should really make a copy!
(define *user-ticks* 5) ;; lyn. Ticks user gets to run before timeout.

(define (museme-herald) 
  (string-append "MUSEME SU-server " *server-version* " " *server-date*))
(define *server-version* "2.0.1")
(define *server-date* "11/23/94")

(define *login-tries* 3) ;; lyn. Number of tries user gets to log in.

(define (login in out)
  (with-current-ports
   in out out
   (lambda ()
     (newline) 
     (print (museme-herald))
     (let loop ((count 1))
       (if (> count *login-tries*) 
	   (begin (close-input-port in)
		  (close-output-port out)
		  (terminate-current-thread)))
;       (display "login: ")
       (let ((user "single-user"))	;; (read-string)
;	 (display "Password: ")
	 (let* ((pass "new")		;; (read-password)
		(status (if (or (equal? pass "new")
				(equal? pass "wizard"))
			    (register user (equal? pass "wizard"))
			    (lookup-user user pass))))
	   (case status
	     ((ok) (user-session user))
	     ((wrong) (print "Password incorrect")
		      (loop (+ 1 count)))
	     ((unknown) (print "User unknown")
			(loop (+ 1 count)))
	     ((dump) (print "Dumping environment...")
		     (dump-museme))
	     (else (loop (+ 1 count))))))))))

(define (user-session user)
  (newline) 
  (print "Welcome to MUSEME")
  (with-lock cu-lock
	     (lambda () (set! connected-users
			      (cons user connected-users))))
  (print "1")
  (call-with-current-continuation
   (lambda (quit) 
     (call-with-current-continuation
      (lambda (next)
	(user-initialize (user-env user) next quit)))
     (eval-repeatedly (user-env user) quit)))
  (print "Goodbye.")
  (with-lock cu-lock
	     (lambda () (set! connected-users
			      (delq user connected-users)))))

(define (user-initialize env next quit)
  (with-handler
   (lambda (cond punt)
     (display-condition cond (current-output-port)) 
     (newline)
     (next))
   (lambda () 
     (user-eval '(become me) env next quit)
     )))

(define (register user wizard?)
;   (newline) (print "Enter a password for " user ":")
;   (let ((p1 (read-password)))
;     (print "Enter the same password again:")
;     (let ((p2 (read-password)))
;       (cond ((equal? p1 p2)
; 	     (add-user! user p1 wizard?)
; 	     'ok)
; 	    (else (print "Passwords do not match.")
; 		  'error))))
  (add-user! user "" wizard?)
  (print "Adding user " user " with no password.")
  'ok
  )

(define eval-lock (make-lock))

(define (eval-repeatedly env quit)
  (display "MUSEME> ")
  (call-with-current-continuation
   (lambda (next)
     (with-handler
      (lambda (cond punt)
	(display-condition cond (current-output-port)) (newline)
	(next))
      (lambda ()
	(maybe-eat-telnet-command)
	(user-eval (museme-read) env next quit)))))
  (eval-repeatedly env quit))

(define (user-eval expr env next quit)
  (if (eof-object? expr)
      (quit)
      (let* ((op (current-output-port))
	     (ip (current-input-port))
	     (result (with-lock eval-lock
				(lambda ()
				  (run-engine *user-ticks*
					      (lambda ()
						(with-current-ports
						 ip op op
						 (lambda ()
						   (eval expr env)))))))))
	(cond ((procedure? result) (handle-engine result))
	      ((quit? result) (quit))
	      ((museme-error? result) (for-each (lambda (e) (print e))
						(cddr result)))
	      ((error? result)
	       (display-condition result (current-output-port)) (newline)
	       (next))
	      (else (for-each (lambda (obj)
				(cond ((not (unspecific? obj))
				       (write obj) (newline))))
			      (cdr result)))))))

(define (handle-engine resume)
  (error "engine ran out of time"))

(define (quit? exp) (equal? exp '(error quit)))
(define (museme-error? exp) (eq? (cadr exp) 'museme))

(define return-char (ascii->char 13))
(define telnet/will (ascii->char 251))
(define telnet/wont (ascii->char 252))
(define telnet/do (ascii->char 253))
(define telnet/dont (ascii->char 254))
(define telnet/IAC (ascii->char 255))
(define telnet/echo (ascii->char 1))

(define (punt-char)
  (let ((c (read-char (current-input-port))))
;   (newline)
;   (display "Punt char: ")
;   (display (char->ascii c))
    c))

(define (maybe-eat-telnet-command)
  (if (eq? (peek-char (current-input-port)) telnet/IAC)
      (begin (punt-char)
	     (punt-char)
	     (punt-char))))

(define (read-password)
  (write-char telnet/IAC) (write-char telnet/will) (write-char telnet/echo)
  (let ((str (read-string)))
    (write-char telnet/IAC) (write-char telnet/wont) (write-char telnet/echo)
    (newline)
    str))

(define (read-string)
  (maybe-eat-telnet-command)
  (let ((c (read-char (current-input-port))))
    (if (eof-object? c)
	c
	(list->string
	 (let loop ((c c))
	   (cond ((or (eof-object? c) (eq? c #\newline))
		  '())
		 ((eq? c return-char)
		  (loop (read-char (current-input-port))))
		 ((eq? c telnet/IAC)
		  (punt-char)
		  (punt-char)
		  (loop (read-char (current-input-port))))
		 (else
		  (cons c
			(loop (read-char (current-input-port)))))))))))

(define unspecific (if #f #f))
(define (unspecific? x) (eq? x unspecific))

(define (dump-museme)
  (build-image script-entry "server.image"))
