;;;----------------------------------------------------------------------------
;;; NAMESERVER.SCM
;;; 
;;; Change log:
;;; * 11/18/94 (lyn)
;;;   + Modified NEW-ENV so that creates a person named ME.
;;; * 1/6/95 (danw)
;;;   + undid lyn's 11/18 change and replaced it with changes to mud-commands
;;;   + removed GET/SET type stuff since no longer used
;;; * 1/9/95 (lyn) 
;;;   + added (eval `(define username ,id) p) back into NEW-ENV.
;;;     because MUD-COMMANDS depends on USERNAME. 
;;;----------------------------------------------------------------------------

;; big list o' data
(define names '() )

;; selectors for name records
(define namef car)
(define passwordf cadr)
(define envf caddr)

;; find a record matching `name'
(define (match name names)
  (if (null? names)
      #f
      (if (equal? name (caar names))
	  (car names)
	  (match name (cdr names)))))

;; yield a list of names
(define (all-users) (map namef names))

;;;; Internal stuff

;; verify a name/password combo
(define (lookup-user name password)
  (if (and (equal? name "dump") (equal? password "foobar"))
      'dump
      (let ((m (match name names)))
	(if m
	    (if (equal? password (passwordf m))
		'ok
		'wrong)
	    'unknown))))

;; return the environment associated with `name'
(define (user-env name)
  (let ((m (match name names)))
    (if m
	(envf m)
	#f)))

(define (add-user! name password wizard?)
  (if password
      (begin (set! names (cons (list name password (new-env name wizard?))
			       names))
	     'ok)
      'wrong))

(define (delete-user! name)
  (let ((m (match name names)))
    (if m (set! names (delq m names)))))

;; make a new user environment
(define new-env
  (let ((basic-structs (list (get-structure 'museme-scheme)
			     (get-structure 'museme-mud-environment)))
	(wizard-struct (get-structure 'museme-wizard-environment))
	(tower (reflective-tower
		(package->environment (interaction-environment)))))
    (lambda (id wizard?)
      (display (if wizard?
		   "Creating new wizard environment."
		   "Creating new environment."))
      (newline)
      (let ((p (make-simple-package (if wizard?
					(cons wizard-struct basic-structs)
					basic-structs)
				    #t tower id)))
	(eval `(define username ,id) p)
	(load-into "mud-commands.scm" p)
	p))))

;; BJR: misc -- should really go some where else... 
(define (scheme->string val)
  (call-with-string-output-port (lambda (op) (write val op))))

;; make a scheme environment
(define new-scheme-env
  (let ((basic-structs (list (get-structure 'museme-scheme)))
	(tower (reflective-tower
		(package->environment (interaction-environment)))))
    (lambda ()
      (let ((p (make-simple-package basic-structs #t tower "foo")))
	p))))

