(define (initialize-env env)
  (format #t "Initializing DPVM environment...~%")
  (letrec ((get/v (lambda (id)
		    (cond ((assq id (env-rib env)) => cdr)
			  (else (error "Id ~a not found in top rib!" id)))))
	   (get (lambda (id) (value-data (get/v id))))
	   (add! (lambda (var data)	; wraps data into value
		   (update-env-rib! env var (value-maker data))))
	   (add-value! (lambda (var val)
			 (update-env-rib! env var val)))
	   ;; fun takes unwrapped values, always succeeds, returns unwrapped values
	   ;;  -- only use add-raw-primop! if result pred will succeed based
	   ;;     only on the data in the returned value ("by extension")
	   (add-raw-primop!
	    (lambda (var pred fun)
	      (update-env-rib! env var
			       (value-maker
				(make-simple-primop
				 pred
				 (lambda args
				   (value-maker
				    (apply fun (map value-data args))))
				 var)))))
	   (add-raw-bool-primop!
	    (lambda (var pred fun)
	      (update-env-rib! env var
			       (value-maker
				(make-simple-primop
				 pred
				 (lambda args
				   (bool-value
				    (apply fun (map value-data args))))
				 var)))))
	   ;; fun takes values, always succeeds, returns a value
	   (add-values-primop!
	    (lambda (var pred fun)
	      (update-env-rib! env var
			       (value-maker
				(make-simple-primop pred fun var))))))
    (add-value! '#t *true*)
    (add-value! '#f *false*)
    (add! '*undefined* *undefined*)
    (add! '<top> (top-pred-maker))
    (add! '<bottom> (bottom-pred-maker))
    (add! '<pred-list> *pred-list-pred*)

    ;; some primop predicates
    (map (lambda (sym fun)
	   (add! sym (value-pred-maker
		      (make-simple-primop
		       (top-pred-maker)
		       (lambda (v) (bool-value (fun (value-data v))))
		       sym))))
	 '(<list> <primop> <cell> <env> <string> <integer>
		  <list-pred> <value-pred>)
	 (list ro-list? primop? cell? env? string? integer?
	       list-pred? value-pred?))

    (add! '<fun-pred> *fun-pred-pred*)
    (add! '<closure> *closure-pred*)
    (add! '<pred> *pred-pred*)
    (add! '<boolean> *boolean-pred*)
    (add! '<callable> *callable-pred*)

    ;; (int, int) -> int primops
    (map (lambda (var fun)
	   (add-raw-primop! var 
			    (fun-pred-maker
			     (list-pred-maker (list (get '<integer>) (get '<integer>)))
			     (get '<integer>))
			    fun))
	 '(+ - * mod)
	 (list + - * modulo))

    ;; (int, int) -> bool primops
    (map (lambda (var fun)
	   (add-raw-bool-primop! var
				 (fun-pred-maker
				  (list-pred-maker (list (get '<integer>) (get '<integer>)))
				  (get '<boolean>))
				 fun))
	 '(< > =)
	 (list < > =))

    ;; eq? : (top, top) -> bool
    (add-raw-bool-primop! 'eq? (fun-pred-maker (top-pred-maker) (get '<boolean>))
			  eq?)
    ;; string=? : (string, string) -> bool
    (add-raw-bool-primop! 'string=?
			  (fun-pred-maker
			   (list-pred-maker (list (get '<string>) (get '<string>)))
			   (get '<boolean>))
			  string=?)
    ;; string-append : (string, string, ...) -> string
    (add! 'string-append
	  (primop-maker
	   (fun-pred-maker (top-pred-maker) (get '<string>))
	   (lambda (vals env succ fail ctxt)
	     (succ (value-maker (apply string-append (map value-data vals)))))
	   'string-append))
    ;; print top -> #t
    (add! 'print
	  (primop-maker
	   (fun-pred-maker (top-pred-maker) (top-pred-maker))
	   (lambda (vals env succ fail ctxt)
	     (apply format (cons #t (map value-data vals)))
	     (succ *true*))
	   'print))
    ;; list : top -> <list>
    (add! 'list
	  (primop-maker
	   (fun-pred-maker (top-pred-maker) (get '<list>))
	   (lambda (vals env succ fail ctxt)
	     (succ (value-maker (make-ro-list vals env))))))
    ;; list? : top -> bool
    (add-values-primop! 'list? (fun-pred-maker (top-pred-maker) (get '<boolean>))
			(lambda (v) (bool-value (ro-list? (value-data v)))))
    ;; list-ref : (list, int) -> top       ** can fail
    (add! 'list-ref
	  (primop-maker
	   (fun-pred-maker (list-pred-maker (list (get '<list>) (get '<integer>)))
			   (top-pred-maker))
	   (lambda (vals env succ fail ctxt)
	     (let ((lst (ro-list-elts (value-data (car vals))))
		   (n (value-data (cadr vals))))
	       (if (< n (length lst))
		   (succ (list-ref lst n))
		   (fail ctxt "list-ref, index ~a past end of list ~a" n lst))))
	   'list-ref))
    ;; cons : (top, list) -> list
    (add! 'cons
	  (primop-maker
	   (fun-pred-maker
	    (list-pred-maker (list (top-pred-maker) (get '<list>)))
	    (get '<list>))
	   (lambda (vals env succ fail ctxt)
	     (let ((list-pred (list-pred-maker
			       (cons (apply pred-lub env (value-preds (car vals)))
				     (list-pred-preds (ro-list-list-pred (value-data (cadr vals)))))))
		   (elt-pred (apply pred-lub env
				    (cons (ro-list-elt-pred (value-data (cadr vals)))
					  (value-preds (car vals))))))
	       (succ (value-maker
		      (ro-list-maker
		       (cons val (ro-list-elts (value-data (cadr vals))))
		       list-pred elt-pred)))))))



))