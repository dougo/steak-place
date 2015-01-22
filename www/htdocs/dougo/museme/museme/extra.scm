(define (eval-repeatedly env quit)
  (display "> ")
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
