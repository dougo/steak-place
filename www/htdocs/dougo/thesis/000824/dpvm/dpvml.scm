;	$Id: dpvml.scm,v 1.5 2000/03/07 17:44:33 gregs Exp gregs $	

;; Dynamic Predicate Virtual Machine Language

;; S-expression-based, 
;;   -- no set!, set-car!, set-cdr!
;;   -- predicates on bindings.
;;
;; Exp ::= 
;;      |  (cond (test-exp then-exp) ... [(else else-exp)])
;;      |  (and and-exps)                  -- short circuits
;;      |  (or or-exps)                    -- short circuits
;;      |  (lambda vars fun-pred-exp body-exp)  -- ex: (fun (x y) <top> (+ x y))
;;      |  (pred var body-exp)             -- value-pred-exp 
;;      |  (begin exps)
;;      |  (let [loop-id] ((var init-exp) ...) pred-exp body-exp)
;;      |  (let* ((var init-exp) ...) pred-exp body-exp)
;;      |  (exp exps)                       -- call-exp
;;      |  var
;;      |  number or string literal
;;
;; convenience macros:
;;
;;      |  (if test-exp then-exp [else-exp])
;;      |  (-> args-pred-exp result-pred-exp)   -- fun-pred
;;      |  (define var pred exp)            -- binds variable var in top scope
;;      |  (define (var var*) fun-pred body)  -- binds a function named var in top scope

(define (parse-dpvml-exp e)
  (letrec 
      ((bad-e-maker
	(lambda (e id)
	  (format #t "Invalid ~a expression: ~a" id e)
	  (call-exp-maker
	   (var-exp-maker 'error)
	   (list (literal-exp-maker
		  (format #f "Invalid ~a expression: ~a" id e))))))
       ;; all these checker funs return (i.e. don't invoke succ) on failure
       (ck-cond (lambda (e succ)	; succ(test-then-pairs)
		  (if (> (length e) 1)
		      (let ((test-then-pairs (cdr e)))
			(if (and (list? test-then-pairs)
				 (every? (lambda (m)
					   (and (list? m) (= (length m) 2)))
					 test-then-pairs))
			    (succ test-then-pairs)
			    (bad-e-maker e "cond")))
		      (bad-e-maker e "cond"))))
       (ck-fun (lambda (e succ)
		 (if (and (= (length e) 4)
			  (list? (list-ref e 1))
			  (every? symbol? (list-ref e 1)))
		     (succ (list-ref e 1) (list-ref e 2)
			   (list-ref e 3))
		     (bad-e-maker e "fun"))))
       (ck-let (lambda (e succ)		; succ(loopid/f vars value-exps pred-exp body-exp)
		 (if (or (= 5 (length e)) (= 4 (length e)))
		     (let* ((offset (if (= 5 (length e)) 1 0))
			    (var-val-exps (list-ref e (+ 1 offset)))
			    (pred-exp (list-ref e (+ 2 offset)))
			    (body-exp (list-ref e (+ 3 offset))))
		       (if (and (list? var-val-exps)
				(every? (lambda (p)
					  (and (list? p) (= 2 (length p))))
					var-val-exps))
			   (succ (if (= 5 (length e)) (list-ref e 1) #f)
				 (map car var-val-exps) (map cadr var-val-exps)
				 pred-exp body-exp)
			   (bad-e-maker e "let")))
		     (bad-e-maker e "let"))))
       (ck-let* (lambda (e succ)	; succ(vars value-exps pred-exp body-exp)
		  (if (= 4 (length e))
		      (let ((var-val-exps (list-ref e 1))
			    (pred-exp (list-ref e 2))
			    (body-exp (list-ref e 3)))
			(if (and (list? var-val-exps)
				 (every? (lambda (p)
					   (and (list? p) (= 2 (length p))))
					 var-val-exps))
			    (succ (map car var-val-exps)
				  (map cadr var-val-exps)
				  pred-exp body-exp)
			    (bad-e-maker e "let*")))
		      (bad-e-maker e "let*"))))
       (ck-pred (lambda (e succ)	; succ(var body-exp)
		  (if (and (= 3 (length e))
			   (symbol? (cadr e)))
		      (succ (cadr e) (list-ref e 2))
		      (bad-e-maker e "pred"))))
       (ck-if (lambda (e succ)		; succ(test-exp then-exp else-exp)
		(cond
		 ((= 4 (length e)) (succ (cadr e) (list-ref e 2) (list-ref e 3)))
		 ((= 3 (length e)) (succ (cadr e) (list-ref e 2) '#f))
		 (else (bad-e-maker e "if")))))
       (ck--> (lambda (e succ)		; succ(args-pred-exp result-pred-exp)
		(if (= 3 (length e))
		    (succ (cadr e) (list-ref e 2))
		    (bad-e-maker e "->"))))
       (ck-define (lambda (e succ)	; succ(var-or-vars pred-exp val-exp)
		    (if (= 4 (length e))
			(if (symbol? (cadr e)) ; var def
			    (succ (cadr e) (list-ref e 2) (list-ref e 3))
			    (if (and (list? (cadr e))
				     (not (null? (cadr e)))
				     (every? symbol? (cadr e)))
				(succ (cadr e) (list-ref e 2) (list-ref e 3))
				(bad-e-maker "define")))
			(bad-e-maker e "define"))))
       (ck-call (lambda (e id succ)	; succ(arg-exps)
		  (if (not (null? e))
		      (succ (cdr e))
		      (bad-e-maker e id)))))
    (cond
     ((pair? e)
      (case (car e)
	((cond)
	 (ck-cond
	  e
	  (lambda (test-then-pairs)
	    (let ((last-pair (last test-then-pairs)))
	      (if (eq? (car last-pair) 'else)
		  (cond-exp-maker (map (lambda (p)
					 (cons (parse-dpvml-exp (car p))
					       (parse-dpvml-exp (cadr p))))
				       (non-last test-then-pairs))
				  (parse-dpvml-exp (cadr last-pair)))
		  (cond-exp-maker (map (lambda (p)
					 (cons (parse-dpvml-exp (car p))
					       (parse-dpvml-exp (cadr p))))
				       test-then-pairs)
				  (literal-exp-maker '#f)))))))
	((and)
	 (ck-call
	  e "and"
	  (lambda (conjunct-exps)
	    (and-exp-maker (map parse-dpvml-exp conjunct-exps)))))
	((or)
	 (ck-call e
		  "or"
		  (lambda (disjunct-exps)
		    (or-exp-maker (map parse-dpvml-exp disjunct-exps)))))
	((lambda)
	 (ck-fun e
		 (lambda (vars pred-exp body-exp)
		   (fun-exp-maker vars 
				  (parse-dpvml-exp pred-exp)
				  (parse-dpvml-exp body-exp)))))
	((begin) 
	 (ck-call e "begin"
		  (lambda (exps)
		    (seq-exp-maker (map parse-dpvml-exp exps)))))
	((let)
	 (ck-let e
		 (lambda (loop-id vars value-exps pred-exp body-exp)
		   (let-exp-maker
		    loop-id vars
		    (map parse-dpvml-exp value-exps)
		    (parse-dpvml-exp pred-exp)
		    (parse-dpvml-exp body-exp)))))

	((let*)    ;;; this is not a macro because pred is tricky (applies to whole list of vals)
	 (ck-let* e
		  (lambda (vars value-exps pred-exp body-exp)
		    (let*-exp-maker
		     vars
		     (map parse-dpvml-exp value-exps)
		     (parse-dpvml-exp pred-exp)
		     (parse-dpvml-exp body-exp)))))
	((pred)
	 (ck-pred e
		  (lambda (var body-exp)
		    (parse-dpvml-exp
		     `(value-pred (fun (,var) (-> <top> <top>)
				       ,body-exp))))))
	;; convenience macros
	((if) 
	 (ck-if e
		(lambda (test-exp then-exp else-exp)
		  (parse-dpvml-exp `(cond (,test-exp ,then-exp)
					  (else ,else-exp))))))
	((->)
	 (ck--> e
		(lambda (args-pred-exp result-pred-exp)
		  (parse-dpvml-exp
		   `(fun-pred ,args-pred-exp ,result-pred-exp)))))
	((define)
	 (ck-define e
		    (lambda (var-or-vars pred-exp val-exp)
		      (if (not (list? var-or-vars))
			  ;; var def'n
			  (parse-dpvml-exp
			   `(bind-in-scope (top-scope)
					   ,(symbol->string var-or-vars) ; var name
					   ,pred-exp
					   ,val-exp))
			  ;; function def'n
			  (parse-dpvml-exp
			   `(bind-in-scope (top-scope) 
					   ,(symbol->string (car var-or-vars))
					   ,pred-exp
					   (lambda ,(cdr var-or-vars)
					     ,pred-exp
					     ,val-exp)))))))
	;; here if a pair but no keyword at head
	(else				; must be a call
	 (ck-call e "call"
		  (lambda (arg-exps)
		    (call-exp-maker (parse-dpvml-exp (car e))
				    (map parse-dpvml-exp arg-exps)))))))
     ;; here if not a pair
     ((or (looks-like-an-integer? e) (string? e)) ; a literal?
      (literal-exp-maker e))
     (else				; if all else ctxts, must be a var. ref.
      (var-exp-maker e)))))

;; eof
