(define (dump-env env)
  (table-walk
   (lambda (name binding)
     (if (not (eq? (string-ref (symbol->string name) 0) #\.)))
	 (let ((val (maybe-quote-value (eval name env))))
	   (display "(define ") (display name) (display " ")
	   (write (maybe-quote-value val))
	   (display ")") (newline))))
   (package-definitions env)))

(define (maybe-quote-value obj)
  (if (or (pair? obj) (symbol? obj))
      `',obj
      obj))


