;;;----------------------------------------------------------------------------
;;; TRANSLATE.SCM
;;; Scheme<->mud object translation
;;;
;;; Change log:
;;; * 11/18/94 (lyn) 
;;;   + Updated MUD-OBJECT to take a key.
;;; * 11/21/94 (lyn)
;;;   + VALUE message now returns thunk, so that procedures can 
;;;     be returned as values rather than be applied.
;;; * 11/22/94 (bjr&lyn)
;;; + Massive overhaul of this file. Interface is now 
;;;   (scheme->mud key scheme-obj) and (mud->scheme mud-widget).
;;;----------------------------------------------------------------------------

(define (scheme->mud key obj)
  (define (set-attribs! thing type-string opt-name aliases desc)
    ;; name = "TYPE #UID"                  ;; if opt-name is false
    ;;        "TYPE #UID: OPT-NAME"        ;; if opt-name is a string
    (let ((name-string (if opt-name (string-append " " opt-name) "")))
      (set-name! thing key (string-append type-string name-string 
					  " {#" (number->string (uid thing)) "}"))
      (set-aliases! thing key (list opt-name))
      (set-description! thing key (string-append "It looks like " desc "."))
      ))

  (let ((thing (make-thing key "<unset>" nowhere #t))
	(stxt (scheme-object->string obj *short-limit*))
	(ltxt (scheme-object->string obj *long-limit*)))
    (set-prv-handler! thing key 'value (lambda () obj))
    (cond ((string? obj) (set-attribs! thing "string" stxt '()
				       (string-append "the string \"" ltxt "\"")))
	  ((number? obj) (set-attribs! thing "number" stxt '()
				       (string-append "the number " ltxt)))
	  ((boolean? obj) (set-attribs! thing "boolean" stxt '()
					(string-append "the boolean " ltxt)))
	  ((char? obj) (set-attribs! thing "character" stxt '()
				     (string-append "the character " ltxt)))
	  ((procedure? obj) (let* ((name (procedure->string obj))
				   (opt-name (if (string=? "" name) #f name)))
			      (set-attribs! thing "procedure"
					    opt-name
					    (if opt-name (list opt-name) '())
					    (if opt-name (string-append "the procedure " opt-name)
						"a procedure"))))
	  ((list? obj) (set-attribs! thing "list" stxt '() (string-append "the list " ltxt)))
	  ((pair? obj) (set-attribs! thing "pair" stxt '() (string-append "the pair " ltxt)))
	  ((vector? obj) (set-attribs! thing "vector" stxt '() (string-append "the vector " ltxt)))
	  (else (set-attribs! thing "unknown" #f '() "a random scheme object")))
    thing))

(define *short-limit* 15)
(define *long-limit* 50)

(define (scheme-object->string obj max-len)
  (define (stringify obj len k) 
    (let* ((s (call-with-string-output-port (lambda (op) (write obj op))))
	   (slen (string-length s)))
      (if (> slen len)
	  (k (string-append (substring s 0 len) "...") 0)
	  (k s (- len slen)))))
  (let loop ((obj obj) (len max-len) (k (lambda (s len) s)))
    (cond ((boolean? obj) (k (if obj "#t" "#f") (- len 2)))
	  ((procedure? obj) (let ((s (procedure->string obj)))
			      (k s (- len (string-length s)))))
	  ((list? obj)    (if (null? obj)
			      (k "()" (- len 2))
			      (let lloop 
				  ((lst obj) 
				   (len (- len 1)) 
				   (k (lambda (str len)
					(k (string-append "(" str) len))))
				(if (< len 0)
				    (k "...)" len)
				    (loop (car lst) len
					  (lambda (car-txt len)
					    (if (null? (cdr lst))
						(k (string-append car-txt ")")
						   (- len 1))
						(lloop 
						 (cdr lst) (- len 1)
						 (lambda (cdr-txt len)
						   (k (string-append
						       car-txt " " cdr-txt)
						      len))))))))))
	  ((pair? obj)    (loop (car obj) len
				(lambda (car-txt len)
				  (if (<= len 0)
				      (k (string-append "(" car-txt " ...)") len)
				      (loop (cdr obj) (- len 2) ;; before cdr
					    (lambda (cdr-txt len)
					      (k (string-append "(" car-txt " " cdr-txt ")") 
						 (- len 1))))))))

	  (else (stringify obj len k)))))

(define (procedure->string p)
  (debug-data-name (template-info (closure-template p))))
;;  (let ((names (template-names (closure-template p))))
;;    (if (or (eq? names #f) (null? names) (equal? (car names) #f))
;;	""
;;	(symbol->string (car names)))))


(define (mud->scheme obj)
  (with-no-handler (lambda foo (error "MUD object has no scheme value" obj))
		   (lambda () (apply-handler obj 'value))))
