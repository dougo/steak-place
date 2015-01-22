;;	$Id: initialize.scm,v 1.9 2000/03/09 17:39:20 gregs Exp $	
;;
;; Initialize top-level environment of DPVM
;; 
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

    (map (lambda (sym fun)
	   (add! sym (value-pred-maker
		      (make-simple-primop
		       (top-pred-maker)
		       (lambda (v) (bool-value (fun (value-data v))))
		       sym))))
	 '(<cond-exp> <and-exp> <or-exp> <fun-exp> <seq-exp> <let-exp>
		      <let*-exp> <call-exp> <var-exp> <literal-exp>)
	 (list cond-exp? and-exp? or-exp? fun-exp? seq-exp? let-exp?
		      let*-exp? call-exp? var-exp? literal-exp?))

    (add! '<exp> (or-pred-maker
		  (map get
		       '(<cond-exp> <and-exp> <or-exp> <fun-exp> <seq-exp> <let-exp>
				    <let*-exp> <call-exp> <var-exp> <literal-exp>))))

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
	     (succ (value-maker (make-ro-list vals env))))
	   'list))
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
		       (cons (car vals)
			     (ro-list-elts (value-data (cadr vals))))
		       list-pred elt-pred)))))
	   'cons))
    ;; car : list -> top  ** can fail
    (add! 'car
	  (primop-maker
	   (fun-pred-maker (list-pred-maker (list (get '<list>))) (top-pred-maker))
	   (lambda (vals env succ fail ctxt)
	     (let ((lst (ro-list-elts (value-data (car vals)))))
	       (if (null? lst)
		   (fail ctxt "car of an empty list")
		   (succ (car lst)))))
	   'car))
    ;; cdr : list -> list  ** can fail
    (add! 'cdr
	  (primop-maker
	   (fun-pred-maker (list-pred-maker (list (get '<list>))) (get '<list>))
	   (lambda (vals env succ fail ctxt)
	     (let* ((lst (value-data (car vals)))
		    (elts (ro-list-elts lst)))
	       (if (null? elts)
		   (fail ctxt "cdr of an empty list")
		   (succ (value-maker (ro-list-maker
				       (cdr elts)
				       (list-pred-maker
					(cdr (list-pred-preds (ro-list-list-pred lst))))
				       (ro-list-elt-pred lst)))))))
	   'cdr))
    ;; cadr : list -> top   ** can fail
    (add! 'cadr
	  (primop-maker
	   (fun-pred-maker (list-pred-maker (list (get '<list>))) (top-pred-maker))
	   (lambda (vals env succ fail ctxt)
	     (let ((lst (ro-list-elts (value-data (car vals)))))
	       (if (< (length lst) 2)
		   (fail ctxt "cadr of too short list ~a" lst)
		   (succ (cadr lst)))))
	   'cadr))
    ;; append : (list, list, ..., list) -> list
    (add! 'append
	  (primop-maker
	   (fun-pred-maker (get '<top>) (get '<list>))
	   (lambda (vals env succ fail ctxt)
	     (let ((ro-lists (map (compose ro-list-elts value-data) vals)))
	       (succ (value-maker (make-ro-list (apply append ro-lists) env)))))
	   'append))
    ;; reverse : list -> list
    (add-values-primop! 'reverse
			(fun-pred-maker (list-pred-maker (list (get '<list>)))
					(get '<list>))
			(lambda (lst/v)
			  (value-maker
			   (ro-list-maker
			    (reverse (ro-list-elts (value-data lst/v)))
			    (list-pred-maker 
			     (reverse (list-pred-preds (ro-list-list-pred (value-data lst/v)))))
			    (ro-list-elt-pred (value-data lst/v))))))
    ;; list-list-pred : list -> pred
    (add-values-primop! 'list-list-pred
			(fun-pred-maker
			 (list-pred-maker (list (get '<list>))) (get '<pred>))
			(lambda (lst/v)
			  (value-maker (ro-list-list-pred (value-data lst/v)))))
    ;; list-elt-pred : list -> pred
    (add-values-primop! 'list-elt-pred
			(fun-pred-maker
			 (list-pred-maker (list (get '<list>))) (get '<pred>))
			(lambda (lst/v)
			  (value-maker (ro-list-elt-pred (value-data lst/v)))))
    ;; length : list -> int
    (add-values-primop! 'length
			(fun-pred-maker (list-pred-maker (list (get '<list>)))
					(get '<integer>))
			(lambda (lst/v)
			  (value-maker
			   (length (ro-list-elts (value-data lst/v))))))
    ;; null? : (list) -> bool
    (add-values-primop! 'null?
			(fun-pred-maker
			 (list-pred-maker (list (get '<list>)))
			 (get '<boolean>))
			(lambda (list/v)
			  (bool-value (null? (ro-list-elts (value-data list/v))))))
    ;; not : val -> bool
    (add-values-primop! 'not
			(fun-pred-maker (top-pred-maker) (get '<boolean>))
			(lambda (v)
			  (bool-value (not (value-data v)))))
    ;; new-cell : (pred, value) -> cell
    (add-values-primop! 'new-cell
			(fun-pred-maker
			 (list-pred-maker (list (get '<pred>) (top-pred-maker)))
			 (get '<cell>))
			(lambda (pred/v val)
			  (value-maker (cell-maker (value-data pred/v)
						   val))))
    ;; deref-cell : cell -> value
    (add-values-primop! 'deref-cell
			(fun-pred-maker
			 (list-pred-maker (list (get '<cell>))) (top-pred-maker))
			(lambda (cell/v)
			  (cell-value (value-data cell/v))))
    ;; set-cell! : (cell, value) -> value    ** can fail (on cell pred)
    (add! 'set-cell!
	  (primop-maker
	   (fun-pred-maker (list-pred-maker (list (get '<cell>) (top-pred-maker)))
			   (top-pred-maker))
	   (lambda (vals env succ fail ctxt)
	     (let* ((cell (value-data (car vals)))
		    (cell-pred (cell-pred cell))
		    (new-val (cadr vals)))
	       (run-pred cell-pred new-val env
			 (lambda (new-val)
			   (set-cell-value! cell new-val)
			   (succ new-val))
			 fail (push-ctxt ctxt 'set-cell1 cell cell-pred new-val))))
	   'set-cell!))
    ;; cell-pred : cell -> pred
    (add-values-primop! 'cell-pred
			(fun-pred-maker
			 (list-pred-maker (list (get '<cell>))) (get '<pred>))
			(lambda (cell/v)
			  (value-maker (cell-pred (value-data cell/v)))))
    ;; value-pred : callable -> value-pred
    (add-values-primop! 'value-pred
			(fun-pred-maker (list-pred-maker (list (get '<callable>)))
					(get '<pred>))
			(lambda (callable/v)
			  (value-maker
			   (value-pred-maker (value-data callable/v)))))
    ;; sub-pred : (preds) -> value-pred
    ;; -- returns a value-pred that answers false but implies preds
    ;; -- string is for debugging
    (add-values-primop! 'sub-pred
			(fun-pred-maker (list-pred-maker (list (get '<pred-list>)))
					(get '<value-pred>))
			(lambda (preds/v)
			  (let ((new-pred
				 (value-pred-maker
				  (make-simple-primop (top-pred-maker)
						      (lambda args *false*)
						      (string->symbol "a-sub-pred")))))
			    (add-implies! new-pred
					  (map value-data (ro-list-elts (value-data preds/v))))
			    (value-maker new-pred))))
    ;; named-sub-pred : (preds, string) -> value-pred
    ;; -- returns a value-pred that answers false but implies preds
    ;; -- string is for debugging
    (add-values-primop! 'named-sub-pred
			(fun-pred-maker (list-pred-maker (list (get '<pred-list>) (get '<string>)))
					(get '<value-pred>))
			(lambda (preds/v name/v)
			  (let ((new-pred
				 (value-pred-maker
				  (make-simple-primop (top-pred-maker)
						      (lambda args *false*)
						      (string->symbol (value-data name/v))))))
			    (add-implies! new-pred
					  (map value-data (ro-list-elts (value-data preds/v))))
			    (value-maker new-pred))))
    ;; and-pred : preds -> pred
    (add-values-primop! 'and-pred
			(fun-pred-maker (get '<list>) (get '<pred>))
			(lambda pred/v/ls
			  (value-maker (and-pred-maker
					(map value-data pred/v/ls)))))
    ;; or-pred : preds -> pred
    (add-values-primop! 'or-pred
			(fun-pred-maker (get '<list>) (get '<pred>))
			(lambda pred/v/ls
			  (value-maker (or-pred-maker
					(map value-data pred/v/ls)))))
    ;; list-pred : preds -> pred
    (add-values-primop! 'list-pred
			(fun-pred-maker (get '<pred-list>) (get '<pred>))
			(lambda pred/v/ls
			  (value-maker (list-pred-maker
					(map value-data pred/v/ls)))))
    ;; list-pred-preds : list-pred -> preds
    (add-values-primop! 'list-pred-preds
			(fun-pred-maker (list-pred-maker (list (get '<list-pred>)))
					(get '<pred-list>))
			(lambda (pred/v)
			  (value-maker (ro-list-maker
					(map value-maker ; have to wrap the preds - yuck!
					     (list-pred-preds (value-data pred/v)))
					(list-pred-maker
					 (map (lambda (ignore) (get '<pred>))
					      (list-pred-preds (value-data pred/v))))
					(get '<pred>)))))
    ;; fun-pred : (pred, pred) -> pred
    ;; called by translation of '-> syntax
    (add-values-primop! 'fun-pred
			(fun-pred-maker
			 (list-pred-maker (list (get '<pred>) (get '<pred>)))
			 (get '<pred>))
			(lambda (args-pred/v result-pred/v)
			  (value-maker (fun-pred-maker
					(value-data args-pred/v)
					(value-data result-pred/v)))))
    ;; fun-pred-args-pred : fun-pred -> pred
    (add-values-primop! 'fun-pred-args-pred
			(fun-pred-maker
			 (list-pred-maker (list (get '<fun-pred>)))
			 (get '<pred>))
			(lambda (fun-pred/v)
			  (value-maker (fun-pred-args-pred (value-data fun-pred/v)))))
    ;; fun-pred-result-pred : fun-pred -> pred
    (add-values-primop! 'fun-pred-result-pred
			(fun-pred-maker
			 (list-pred-maker (list (get '<fun-pred>)))
			 (get '<pred>))
			(lambda (fun-pred/v)
			  (value-maker (fun-pred-result-pred (value-data fun-pred/v)))))
    ;; bottom-pred : () -> pred
    (add-values-primop! 'bottom-pred
			(fun-pred-maker (list-pred-maker '()) (top-pred-maker))
			(lambda ()
			  (value-maker (bottom-pred-maker))))
    ;; eq-pred : val -> pred
    ;; ** seeds implies list of new pred with preds list from val **
    (add-values-primop! 'eq-pred
			(fun-pred-maker (get '<top>) (get '<pred>))
			(lambda (val)
			  (let ((new-pred (eq-pred-maker val)))
			    (if (not (null? (value-preds val)))
				(add-implies! new-pred (value-preds val)))
			    (value-maker new-pred))))
    ;; pred-implies? : (pred, pred) -> bool
    (add! 'pred-implies?
	  (primop-maker
	   (fun-pred-maker (list-pred-maker
			    (list (get '<pred>) (get '<pred>)))
			   (get '<boolean>))
	   (lambda (vals env succ fail ctxt)
	     (succ (bool-value
		    (pred-implies? (value-data (car vals))
				   (value-data (cadr vals)) env))))
	   'pred-implies?))
    ;; elt-pred : pred -> pred
    (add-values-primop! 'elt-pred
			(fun-pred-maker (list-pred-maker (list (get '<pred>)))
					(get '<pred>))
			(lambda (pred/v)
			  (value-maker (elt-pred-maker (value-data pred/v)))))

    ;; add-implies! : (value-pred, pred-list) -> pred
    ;; -- extends list of implied preds for 1st pred with 2nd pred.
    ;; *** list of implied predicates for a given predicate can only grow ***
    (add-values-primop! 'add-implies!
			(fun-pred-maker (list-pred-maker
					 (list (get '<value-pred>) (get '<pred-list>)))
					(get '<pred>))
			(lambda (pred/v implied-preds/v)
			  (add-implies! (value-data pred/v)
					(map value-data (ro-list-elts (value-data implied-preds/v))))
			  pred/v))
    ;; as : (pred, val) -> val     (adds pred to val's preds)
    (add-values-primop! 'as
			(fun-pred-maker (list-pred-maker
					 (list (get '<pred>) (top-pred-maker)))
					(top-pred-maker))
			(lambda (pred/v val)
			  (update-value-preds! val (value-data pred/v))
			  val))
    ;; check-pred : (pred, val) -> bool
    (add! 'check-pred
	  (primop-maker
	   (fun-pred-maker (list-pred-maker (list (get '<pred>) (top-pred-maker)))
			   (get '<boolean>))
	   (lambda (vals env succ fail ctxt)
	     (let ((pred/v (car vals))
		   (val (cadr vals)))
	       (run-pred (value-data pred/v) val env
			 (lambda (val)
			   (succ *true*))
			 (lambda fail-args ; succeed (with #f) on failure
			   (succ *false*))
			 (push-ctxt ctxt 'check-pred1 pred/v val))))
	   'check-pred))
    ;; error : (string, args) -> *fail*
    (add! 'error
	  (primop-maker
	   (fun-pred-maker (top-pred-maker) (top-pred-maker))
	   (lambda (vals env succ fail ctxt)
	     (apply fail ctxt (map value-data vals)))
	   'error))
    ;; assert : (val string args) -> #t or *fail*
    (add! 'assert
	  (primop-maker
	   (fun-pred-maker (top-pred-maker) (top-pred-maker))
	   (lambda (vals env succ fail ctxt)
	     (if (eq? *true* (car vals))
		 (succ *true*)
		 (apply fail ctxt (map value-data (cdr vals)))))
	   'assert))
    ;; push-scope : () -> env
    (add! 'push-scope
	  (primop-maker
	   (fun-pred-maker (list-pred-maker '()) (get '<env>))
	   (lambda (vals env succ fail ctxt)
	     (succ (value-maker (env-maker (list) env))))
	   'push-scope))
    ;; current-scope : () -> env
    (add! 'current-scope
	  (primop-maker
	   (fun-pred-maker (list-pred-maker '()) (get '<env>))
	   (lambda (vals env succ fail ctxt)
	     (succ (value-maker env)))
	   'current-scope))
    ;; top-scope : () -> env
    (add! 'top-scope
	  (primop-maker
	   (fun-pred-maker (list-pred-maker '()) (get '<env>))
	   (lambda (vals env succ fail ctxt)
	     (succ (value-maker (top-level-env env))))
	   'top-scope))
    ;; in-scope? (string, env) -> bool
    (add! 'in-scope? 
	  (primop-maker
	   (fun-pred-maker (list-pred-maker (list (get '<string>) (get '<env>)))
			   (get '<boolean>))
	   (lambda (vals env succ fail ctxt)
	     (let* ((var-name-string (value-data (car vals)))
		    (var-sym (string->symbol var-name-string))
		    (env (value-data (cadr vals))))
	       (lookup-var var-sym env (top-pred-maker)
			   (lambda (v)
			     (succ *true*))
			   (lambda fail-args ; succeed w. #f if lookup fails
			     (succ *false*))
			   (push-ctxt ctxt 'in-scope1 var-sym))))
	   'in-scope?))
    ;; deref-var : (string, scope) -> val
    (add! 'deref-var
	  (primop-maker
	   (fun-pred-maker (list-pred-maker (list (get '<string>) (get '<env>)))
			   (top-pred-maker))
	   (lambda (vals env succ fail ctxt)
	     (let* ((var-name-string (value-data (car vals)))
		    (var-sym (string->symbol var-name-string))
		    (env (value-data (cadr vals))))
	       (lookup-var var-sym env (top-pred-maker)
			   succ fail (push-ctxt ctxt 'deref-var1 var-sym))))
	   'deref-var))
    ;; bind-in-scope (env, name-string, pred, val) -> val
    (add! 'bind-in-scope
	  (primop-maker
	   (fun-pred-maker
	    (list-pred-maker (list (get '<env>) (get '<string>)
				   (get '<pred>) (top-pred-maker)))
	    (top-pred-maker))
	   (lambda (vals env succ fail ctxt)
	     (let* ((env (value-data (car vals)))
		    (var-name-string (value-data (list-ref vals 1)))
		    (var-sym (string->symbol var-name-string))
		    (pred (value-data (list-ref vals 2)))
		    (val (list-ref vals 3)))
	       (run-pred pred val env
			 (lambda (val)
			   (update-env-rib! env var-sym val)
			   (succ val))
			 fail (push-ctxt ctxt 'bind-in-scope1 pred val))))
	   'bind-in-scope))
    ;; apply : (callable val val ... vals) -> val
    (add! 'apply
	  (primop-maker
	   (fun-pred-maker (top-pred-maker) (top-pred-maker))
	   (lambda (vals env succ fail ctxt)
	     (let ((callable (value-data (car vals)))
		   (arg-vals (cdr vals)))
	       (let ((apply-vals/v
		      (if (not (ro-list? (value-data (last arg-vals))))
			  (fail ctxt "primop apply - args must end with a list: ~a" vals)
			  ;; too much consing / wrapping!
			  (value-maker
			   (make-ro-list (append (non-last arg-vals)
						 (ro-list-elts (value-data (last arg-vals))))
					 env)))))
		 ;; check that args pass callable's args pred
		 (run-pred (callable-args-pred callable)
			   apply-vals/v env
			   (lambda (apply-vals/v)
			     (run-callable callable env apply-vals/v
					   (top-pred-maker)
					   succ fail
					   (push-ctxt ctxt 'apply1 callable vals)))
			   fail (push-ctxt ctxt 'apply2 (callable-args-pred callable) callable arg-vals)))))
	   'apply))
    ;; closure-fun-pred : closure -> pred
    (add-values-primop! 'closure-fun-pred
			(fun-pred-maker
			 (list-pred-maker (list (get '<closure>))) (get '<pred>))
			(lambda (clos/v)
			  (value-maker (closure-fun-pred (value-data clos/v)))))
    ;; callable-fun-pred : closure -> pred
    (add-values-primop! 'callable-fun-pred
			(fun-pred-maker
			 (list-pred-maker (list (get '<callable>))) (get '<pred>))
			(lambda (fun/v)
			  (value-maker (callable-fun-pred (value-data fun/v)))))
    ;; callable-args-pred : closure -> pred
    (add-values-primop! 'callable-args-pred
			(fun-pred-maker
			 (list-pred-maker (list (get '<callable>))) (get '<pred>))
			(lambda (fun/v)
			  (value-maker (callable-args-pred (value-data fun/v)))))
    ;; callable-result-pred : closure -> pred
    (add-values-primop! 'callable-result-pred
			(fun-pred-maker
			 (list-pred-maker (list (get '<callable>))) (get '<pred>))
			(lambda (fun/v)
			  (value-maker (callable-result-pred (value-data fun/v)))))
    ;; curry : (callable arg arg ...) -> fun
    ;; *** needs work on constructing result-function predicate.
    (add! 'curry
	  (primop-maker
	   (fun-pred-maker (top-pred-maker) (get '<callable>))
	   (lambda (vals env succ fail ctxt)
	     (let* ((callable (value-data (car vals)))
		    (arg-vals (cdr vals))
		    (num-curried-args (length arg-vals))
		    (orig-args-pred (callable-args-pred callable))
		    ;; try to derive predicate for result function
		    (curried-args-pred
		     (if (list-pred? orig-args-pred)
			 (list-pred-maker
			  (list-tail (list-pred-preds orig-args-pred)
				     num-curried-args))
			 (top-pred-maker)))) ; yuck
	       (succ (value-maker
		      (primop-maker
		       (fun-pred-maker curried-args-pred
				       (callable-result-pred callable))
		       (lambda (vals env succ fail ctxt)
			 (run-callable
			  callable env
			  ;; too much consing / wrapping!
			  (value-maker
			   (make-ro-list (append arg-vals vals) env))
			  (top-pred-maker)
			  succ fail
			  (push-ctxt ctxt 'curry1 callable arg-vals vals)))
		       'a-curried-fun)))))
	   'curry))
    ;; rcurry : (callable arg arg ...) -> fun
    ;; *** needs work on constructing result-function predicate.
    (add! 'rcurry
	  (primop-maker
	   (fun-pred-maker (top-pred-maker) (get '<callable>))
	   (lambda (vals env succ fail ctxt)
	     (let* ((callable (value-data (car vals)))
		    (arg-vals (cdr vals))
		    (num-curried-args (length arg-vals))
		    (orig-args-pred (callable-args-pred callable))
		    ;; try to derive predicate for result function
		    (curried-args-pred
		     (if (list-pred? orig-args-pred)
			 (list-pred-maker
			  (first-n (- (length (list-pred-preds orig-args-pred))
				      num-curried-args)
				   (list-pred-preds orig-args-pred)))
			 (top-pred-maker)))) ; yuck
	       (succ (value-maker
		      (primop-maker
		       (fun-pred-maker curried-args-pred
				       (callable-result-pred callable))
		       (lambda (vals env succ fail ctxt)
			 (run-callable
			  callable env
			  ;; too much consing / wrapping!
			  (value-maker (make-ro-list (append vals arg-vals) env))
			  (top-pred-maker)
			  succ fail
			  (push-ctxt ctxt 'rcurry1 callable arg-vals vals)))
		       'an-rcurried-fun)))))
	   'rcurry))
    ;; compose : (fun, fun) -> fun 
    ;;   ((compose f g) vals) == (f (g vals))
    (add! 'compose
	  (primop-maker
	   (fun-pred-maker
	    (list-pred-maker (list (get '<callable>) (get '<callable>)))
	    (get '<callable>))
	   (lambda (vals env succ fail ctxt)
	     (let* ((f (value-data (car vals)))
		    (g (value-data (cadr vals)))
		    (f-args-pred (callable-args-pred f))
		    (f-result-pred (callable-result-pred f))
		    (g-args-pred (callable-args-pred g))
		    (g-result-pred (callable-result-pred g))
		    (new-fun-pred	; g's args, f's result
		     (fun-pred-maker g-args-pred f-result-pred)))
	       (succ
		(value-maker
		 (primop-maker
		  new-fun-pred
		  (lambda (vals env succ fail ctxt)
		    (run-callable g env
				  ;; too much consing / wrapping!
				  (value-maker (make-ro-list vals env))
				  (top-pred-maker)
				  (lambda (v)
				    (run-callable
				     f env
				     ;; too much consing / wrapping!
				     (value-maker (make-ro-list (list v) env))
				     (top-pred-maker)
				     succ fail (push-ctxt ctxt 'compose1 f v vals)))
				  fail (push-ctxt ctxt 'compose2 g vals)))
		  'a-composed-fun)))))
	   'compose))
    ;; typed-compose : (fun, fun) -> fun 
    ;; Same as compose, but checks that f and g hook up type-correctly.
    (add! 'typed-compose
	  (primop-maker
	   (fun-pred-maker
	    (list-pred-maker (list (get '<callable>) (get '<callable>)))
	    (get '<callable>))
	   (lambda (vals env succ fail ctxt)
	     (let* ((f (value-data (car vals)))
		    (g (value-data (cadr vals)))
		    (f-args-pred (callable-args-pred f))
		    (f-result-pred (callable-result-pred f))
		    (g-args-pred (callable-args-pred g))
		    (g-result-pred (callable-result-pred g))
		    (new-fun-pred	; g's args, f's result
		     (fun-pred-maker g-args-pred f-result-pred)))
	       ;; check that g's result-pred is compatible w. f's args-pred
	       (if (pred-implies? g-result-pred f-args-pred env)
		   (succ
		    (value-maker
		     (primop-maker
		      new-fun-pred
		      (lambda (vals env succ fail ctxt)
			(run-callable g env
				      ;; too much consing / wrapping!
				      (value-maker (make-ro-list vals env))
				      (top-pred-maker)
				      (lambda (v)
					(run-callable f env
						      ;; too much consing / wrapping!
						      (value-maker (make-ro-list (list v) env))
						      (top-pred-maker)
						      succ fail (push-ctxt ctxt 'tcompose1 f v vals)))
				      fail (push-ctxt ctxt 'tcompose2 g vals)))
		      'a-typed-composed-fun)))
		   (fail ctxt "Cannot compose ~a and ~a -- type mismatch" f g))))
	   'typed-compose))
    ;; map : (fun, list, list, ...) -> list
    (add! 'map
	  (primop-maker
	   (fun-pred-maker (get '<top>) (get '<list>))
	   (lambda (vals env succ fail ctxt)
	     ;; should check that cdr vals are all lists, and that all lists are same length
	     (let ((fun (value-data (car vals)))
		   (list-of-elt-lists (map (compose ro-list-elts value-data) (cdr vals))))
	       (let loop ((list-list list-of-elt-lists) (out '()))
		 (if (null? (car list-list))
		     (let ((out-list
			    (ro-list-maker
			     (reverse out)
			     ;; seed list preds using callable result pred
			     (list-pred-maker
			      (map (lambda (ig) (callable-result-pred fun)) out))
			     (callable-result-pred fun)))) ; elt-pred
		       (succ (value-maker out-list)))
		     ;; too much wrapping!
		     (run-callable fun env
				   (value-maker (make-ro-list (map car list-list) env))
				   (top-pred-maker)
				   (lambda (v)
				     (loop (map cdr list-list) (cons v out)))
				   fail (push-ctxt ctxt 'map1 fun (map car list-list)))))))
	   'map))
    ;; every? : (fun, list, list, ...) -> bool
    (add! 'every?
	  (primop-maker
	   (fun-pred-maker (top-pred-maker) (get '<boolean>))
	   ;; should check compatibility of fun
	   (lambda (vals env succ fail ctxt)
	     (let ((fun (value-data (car vals)))
		   (lists (map (compose ro-list-elts value-data)
			       (cdr vals))))
	       (let loop ((lists lists))
		 (if (null? (car lists))
		     (succ *true*)
		     (run-callable fun env
				   (value-maker (make-ro-list (map car lists) env))
				   (top-pred-maker)
				   (lambda (v)
				     (if (eq? v *true*)
					 (loop (map cdr lists))
					 (succ *false*)))
				   fail (push-ctxt ctxt 'every?1 fun (map car lists)))))))
	   'every?))
    ;; any-pred? : (pred, list) -> bool
    (add! 'any-pred?
	  (primop-maker
	   (fun-pred-maker
	    (list-pred-maker (list (get '<pred>) (get '<list>)))
	    (get '<boolean>))
	   (lambda (vals env succ fail ctxt)
	     (let ((pred (value-data (car vals)))
		   (lstvals (ro-list-elts (value-data (cadr vals)))))
	       (let loop ((lstvals lstvals))
		 (if (null? lstvals)
		     (succ *false*)
		     (run-pred pred (car lstvals) env
			       (lambda (v) (succ *true*)) ; once is enough
			       (lambda fail-args (loop (cdr lstvals)))
			       (push-ctxt ctxt 'any-pred1 pred (car lstvals)))))))
	   'any-pred?))
    ;; any? : (fun, list, list, ...) -> bool
    (add! 'any?
	  (primop-maker
	   (fun-pred-maker (top-pred-maker) (get '<boolean>))
	   ;; should check compatibility of fun with lists
	   (lambda (vals env succ fail ctxt)
	     (let ((fun (value-data (car vals)))
		   (lists (map (compose ro-list-elts value-data)
			       (cdr vals))))
	       (let loop ((lists lists))
		 (if (null? (car lists))
		     (succ *false*)
		     (run-callable fun env
				   (value-maker (make-ro-list (map car lists) env))
				   (top-pred-maker)
				   (lambda (v)
				     (if (eq? *true* v)
					 (succ *true*)
					 (loop (map cdr lists))))
				   fail (push-ctxt ctxt 'any?1 fun (map car lists)))))))
	   'any?))
    ;; load-dpvml-file : string -> val
    (add! 'load-dpvml-file
	  (primop-maker
	   (fun-pred-maker (list-pred-maker (list (get '<string>)))
			   (top-pred-maker))
	   (lambda (vals env succ fail ctxt)
	     (load-dpvml-file (value-data (car vals)) env succ fail ctxt))
	   'load-dpvml-file))


    ;; and return the freshly stocked environment

    env))
