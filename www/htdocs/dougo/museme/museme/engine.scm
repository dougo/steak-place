;; code for engines

(define call/cc call-with-current-continuation)

(define (timer? c)
  (equal? c '(interrupt 0 7)))

;; this returns one of:
;; (values vals...)
;; (error ...)
;; #{procedure object} (a continuation to the computation)

;; thread-friendly version... doesn't save continuations
(define (run-engine n thunk)
  (let* ((l (make-lock))
	 (result (make-placeholder))
	 (done? #f)
	 (runner (spawn (lambda ()
			  (let ((val (call/cc
				      (lambda (escape)
					(with-handler
					 (lambda (c punt)
					   (if (error? c)
					       (escape c)
					       (punt)))
					 (lambda ()
					   (call-with-values thunk
							     (lambda args (cons 'values args)))))))))
			    (with-lock l
				       (lambda ()
					 (placeholder-set! result val)
					 (set! done? #t)))))))
	 (timer (spawn (lambda ()
			 (sleep (* n (time 0 #f)))
			 (with-lock l
				    (lambda ()
				      (if (not done?)
					  (begin (kill-thread! runner)
						 (placeholder-set! result '(error "out of time"))))))))))
    (placeholder-value result)))

