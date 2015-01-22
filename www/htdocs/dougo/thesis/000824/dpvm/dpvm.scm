;; $Id: dpvm.scm,v 1.24 2000/03/09 17:39:20 gregs Exp gregs $	
;; 
;; DPVM -- The Dynamic Predicate Virtual Machine
;;
;; variable naming conventions:
;;    foo/f  -- variable will either be #f or contain a foo
;;    foo/ls -- list of foo
;;    foo/v  -- value 
;;
;; TO DO:  
;;  *** unwind-protect
;;  *** threads
;; * symbols
;; * should not cache predicates that update state
;; * could implement and-pred's using only implication (i.e. they 
;;   don't need their own data structure)
;; * implement closures more efficiently -- at least only
;;   stash bindings of free var.s, not whole environment.
;; * namespace & client mgmt: each client should have its own 
;;            namespace/environment.  
;;   -- support explicit sharing btwn (mutually agreeable) clients.
;; * it would be nice if map seeded the value-preds of its result based on
;;   the result-pred of the mapped function.  Could use some list-predicate 
;;   construction tools.

(define-record-type dpvm (root-env) ())

;; preds is a list of predicates that data is known to pass
(define-record-type value (data) ((preds '())))
(define-record-type cell (pred (value)) ()) ; pred is a guard for updating the cell
;; lists are read-only (immutable).  But of course they may contain mutable cells.
;; * efficiency hack:  pure version would be (head-val tail-ro-list list-pred elt-pred), 
;;     with ro-lists (and their assoc'ed preds) all the way down.  When a sublist is exposed, 
;;     create new ro-list with pred's filled in.
;; * elt-pred and list-pred are conveniences - basically cache known pred's.
(define-record-type ro-list (elts (list-pred) (elt-pred)) ())
;; fun-pred could be updated as more info. known
(define-record-type closure (vars (fun-pred) body-exp creation-env) ())
;; primop fun takes 5 args: arg-value-list, env, succ, fail, context, & returns a value
(define-record-type primop (fun-pred fun id) ())
(define-record-type env ((rib) parent-env) ())
;; Expressions
(define-record-type cond-exp (test-then-exps else-exp/f) ()) ; test-thens are pairs
(define-record-type and-exp (conjunct-exps) ())	; short circuits
(define-record-type or-exp (disjunct-exps) ()) ; short circuits
(define-record-type fun-exp (vars fun-pred-exp body-exp) ())
(define-record-type seq-exp (exps) ())
(define-record-type let-exp (loop-id/f vars value-exps pred-exp body-exp) ())
(define-record-type let*-exp (vars value-exps pred-exp body-exp) ())
(define-record-type call-exp (fun-exp arg-exps) ())
(define-record-type var-exp (source-var) ())
(define-record-type literal-exp (literal) ())

;; succ : value -> ans.
;; fail : (context . format-args) -> ans
(define (run-exp exp env pred succ fail ctxt)
  (let ((runner
	 (cond
	  ((cond-exp? exp) run-cond-exp)
	  ((and-exp? exp) run-and-exp)
	  ((or-exp? exp) run-or-exp)
	  ((fun-exp? exp) run-fun-exp)
	  ((seq-exp? exp) run-seq-exp)
	  ((let-exp? exp) run-let-exp)
	  ((let*-exp? exp) run-let*-exp)
	  ((call-exp? exp) run-call-exp)
	  ((var-exp? exp) run-var-exp)
	  ((literal-exp? exp) run-literal-exp)
	  (else
	   (error "run-exp, unrecognized expression ~a" exp)))))
    (runner exp env pred succ fail ctxt)))
	     
(define (run-exps exps env pred succ fail ctxt) ; ok => succ(ro-list/v)
  (let loop ((exps exps) (vals '()))
    (if (null? exps)
	(run-pred pred
		  (value-maker (make-ro-list (reverse vals) env))
		  env succ fail
		  (push-ctxt ctxt 'run-exps1 pred vals exps))
	(run-exp (car exps) env
		 (top-pred-maker)	; don't check individual exp. values
		 (lambda (v)
		   (loop (cdr exps) (cons v vals)))
		 fail (push-ctxt ctxt 'run-exps2 (car exps) exps)))))

(define (run-call-exp exp env pred succ fail ctxt)
  (run-exp (call-exp-fun-exp exp) env	; get fun-value : callable/v
	   (or-pred-maker (list *callable-pred* *pred-pred*))
	   (lambda (callable/v)
	     (run-exps (call-exp-arg-exps exp) env ; get arg values
		       (callable-args-pred (value-data callable/v)) ; and check 
		       (lambda (lst/v)
			 (run-callable	; do the call
			  (value-data callable/v)
			  env lst/v pred succ
			  fail (push-ctxt ctxt 'call-exp1 callable/v lst/v)))
		       fail (push-ctxt ctxt 'call-exp2 exp)))
	   fail (push-ctxt ctxt 'call-exp3 exp)))

;; called from run-call-exp
(define (run-callable callable env lst/v pred succ fail ctxt)
  (cond
   ((pred? callable)
    (if (not (= 1 (length (ro-list-elts (value-data lst/v)))))
	(fail ctxt "Cannot run predicate ~a on more than one value (~a)."
	      callable lst/v)
	(run-pred callable (car (ro-list-elts (value-data lst/v)))
		  env
		  succ
		  fail
		  (push-ctxt ctxt "Applying predicate ~a to ~a" callable
			     (car (ro-list-elts (value-data lst/v)))))))
   ((primop? callable)			; primop
    ((primop-fun callable)  ; primops take Scheme list of arg-values, env, succ, fail, ctxt
     ;; note that this loses the list-pred and elt-pred info fo the arg value list. 
     (ro-list-elts (value-data lst/v)) env
     (lambda (v)			; succ
       (run-pred (make-and-pred env pred (callable-result-pred callable))
		 v env succ fail 
		 (push-ctxt ctxt 'callable1 pred callable lst/v)))
     fail (push-ctxt ctxt 'callable3 callable lst/v)))
   ((closure? callable)			; closure
    (let ((rib (map cons		; run body in extended creation environment
		    (closure-vars callable)
		    (ro-list-elts (value-data lst/v)))))
      (run-exp (closure-body-exp callable)
	       (env-maker rib (closure-creation-env callable))
	       (make-and-pred env pred (callable-result-pred callable))
	       succ fail
	       (push-ctxt ctxt 'callable2 callable lst/v))))
   (else
    (fail ctxt "run-callable, not a callable: ~a" callable))))

;; called from run-pred.  Note that vals is not wrapped in (value (ro-list vals))
(define (run-callable-unchecked callable env vals succ fail ctxt)
  (cond
   ((primop? callable)			; primop
    ((primop-fun callable)		; primops take arg-value-list, succ, fail
     vals env succ fail
     (push-ctxt ctxt 'unchecked1 callable vals)))
   ((closure? callable)			; "regular" closure 
    (let ((rib (map cons		; run body in extended creation environment
		    (closure-vars callable)
		    vals)))
      (run-exp (closure-body-exp callable)
	       (env-maker rib (closure-creation-env callable))
	       (top-pred-maker)		; unchecked!
	       succ fail (push-ctxt ctxt 'unchecked2 callable vals))))
   (else
    (error "foo ~a, ~a, ~a" callable env vals))))
;;    (fail ctxt 'unchecked3 callable))))

(define (callable-fun-pred c)
  (cond
   ((closure? c) (closure-fun-pred c))
   ((primop? c) (primop-fun-pred c))
   ((pred? c) (fun-pred-maker (top-pred-maker) (top-pred-maker)))
   (else (error "callable-fun-pred -- not a callable: ~a" c))))

(define (callable-args-pred c)
  (cond
   ((closure? c) (fun-pred-args-pred (closure-fun-pred c)))
   ((primop? c) (fun-pred-args-pred (primop-fun-pred c)))
   ((pred? c) (top-pred-maker))
   (else (error "callable-args-pred -- not a callable: ~a" c))))

(define (callable-result-pred c)
  (cond
   ((closure? c) (fun-pred-result-pred (closure-fun-pred c)))
   ((primop? c) (fun-pred-result-pred (primop-fun-pred c)))
   ((pred? c) (top-pred-maker))
   (else (error "callable-result-pred -- not a callable: ~a" c))))

(define (run-cond-exp exp env pred succ fail ctxt)
  (let loop ((test-then-exps (cond-exp-test-then-exps exp)))
    (if (null? test-then-exps)
	(if (cond-exp-else-exp/f exp)
	    (run-exp (cond-exp-else-exp/f exp)
		     env pred succ fail
		     (push-ctxt ctxt 'cond-exp-else exp))
	    ;; fail if no else
	    (fail ctxt "run-cond-exp, no else, exp = ~a" exp))
	(let ((this-test-exp (car (car test-then-exps)))
	      (this-then-exp (cdr (car test-then-exps))))
	  (run-exp this-test-exp env *boolean-pred*
		   (lambda (test/v)
		     (if (eq? *true* test/v)
			 (run-exp this-then-exp env pred succ fail
				  (push-ctxt ctxt 'cond-exp2 this-then-exp))
			 (loop (cdr test-then-exps))))
		   fail (push-ctxt ctxt 'cond-exp3 this-test-exp))))))

(define (run-and-exp exp env pred succ fail ctxt) ; => <boolean>
  (let loop ((exps (and-exp-conjunct-exps exp)))
    (if (null? exps)
	(succ *true*)
	(run-exp (car exps) env
		 *boolean-pred*
		 (lambda (v)
		   (if (eq? v *true*)
		       (loop (cdr exps))
		       (succ *false*))) ; if #f, succ with #f
		 fail (push-ctxt ctxt 'and-exp1 (car exps) exps)))))

(define (run-or-exp exp env pred succ fail ctxt) ; => <boolean>
  (let loop ((exps (or-exp-disjunct-exps exp)))
    (if (null? exps)
	(succ *false*)
	(run-exp (car exps) env
		 *boolean-pred*
		 (lambda (v)
		   (if (eq? v *true*)
		       (succ *true*) ; once is enough
		       (loop (cdr exps))))
		 fail (push-ctxt ctxt 'or-exp1 (car exps) exps)))))

(define (run-fun-exp exp env pred succ fail ctxt)
  (run-exp (fun-exp-fun-pred-exp exp) env ; get closure-fun-pred : fun-pred
	   *fun-pred-pred*
	   (lambda (fun-pred/v)
	     (let ((closure (closure-maker (fun-exp-vars exp)
					   (value-data fun-pred/v)
					   (fun-exp-body-exp exp)
					   env)))
	       (run-pred pred (value-maker closure) env succ fail 
			 (push-ctxt ctxt 'fun-exp1 pred closure))))
	   fail (push-ctxt ctxt 'fun-exp2 exp)))

(define (run-seq-exp exp env pred succ fail ctxt)
  (if (null? (seq-exp-exps exp))
      (fail ctxt "Empty sequence, ~a" exp)
      (let loop ((exps (seq-exp-exps exp)))
	(if (not (null? (cdr exps)))
	    (run-exp (car exps) env	; non-last exp
		     (top-pred-maker)	; don't check non-last exps
		     (lambda (ignored) (loop (cdr exps))) ; succ -- loop
		     fail (push-ctxt ctxt 'seq-exp1 (car exps) exp))
	    ;; last exp:
	    (run-exp (car exps) env pred succ fail
		     (push-ctxt ctxt 'seq-exp2 (car exps) exp))))))

(define (run-let-exp exp env pred succ fail ctxt)
  (run-exp (let-exp-pred-exp exp) env	; get values predicate
	   *pred-pred*
	   (lambda (pred/v)
	     (run-exps
	      (let-exp-value-exps exp) env ; get ro-list/v of values
	      (value-data pred/v)	; and check them
	      (lambda (lst/v)
		(let* ((let-vars-rib (map cons
				      (let-exp-vars exp)
				      (ro-list-elts (value-data lst/v))))
		       (let-vars-env (env-maker let-vars-rib env))
		       (body-env
			;; is this a loop?
			(if (let-exp-loop-id/f exp)
			    ;; if so, create closure bound to loop id
			    (let* ((loop-rib (list (cons (let-exp-loop-id/f exp) *false*)))
				   (loop-env (env-maker loop-rib let-vars-env))
				   (closure (closure-maker (let-exp-vars exp)
							   (fun-pred-maker (value-data pred/v)
									   (top-pred-maker))
							   (let-exp-body-exp exp)
							   loop-env)))
			      (update-env-rib! loop-env (let-exp-loop-id/f exp)
					       (value-maker closure))
			      loop-env)
			    ;; not a loop -- just let-vars-env
			    let-vars-env)))
		  ;; run the body
		  (run-exp (let-exp-body-exp exp)
			   body-env
			   pred succ fail
			   (push-ctxt ctxt 'let-exp1 exp lst/v))))
	      fail (push-ctxt ctxt 'let-exp2 exp)))
	   fail (push-ctxt ctxt 'let-exp3 exp)))

(define (run-let*-exp exp env pred succ fail ctxt)
  (run-exp (let*-exp-pred-exp exp) env	; get values predicate
	   *pred-pred*
	   (lambda (pred/v)
	     (let* ((rib (map (lambda (id) (cons id *undefined*))
			      (let*-exp-vars exp)))
		    (new-env (env-maker rib env)))
	       (let loop ((val-exps (let*-exp-value-exps exp))
			  (vars (let*-exp-vars exp))
			  (vals '()))	; keep for running pred later
		 (if (not (null? val-exps))
		     (run-exp (car val-exps) new-env (top-pred-maker)
			      (lambda (v) ; update rib as we go
				(update-env-rib! new-env (car vars) v)
				(loop (cdr val-exps) (cdr vars)
				      (cons v vals)))
			      fail (push-ctxt ctxt 'let*-exp1 (car val-exps)))
		     ;; run pred
		     (run-pred
		      (value-data pred/v)
		      (value-maker (make-ro-list (reverse vals) env))
		      env
		      (lambda (ignored-lst/v)
			;; run the body
			(run-exp (let*-exp-body-exp exp)
				 new-env
				 pred succ fail
				 (push-ctxt ctxt 'let*-exp2 exp)))
		      fail (push-ctxt ctxt 'let*-exp3 (reverse vals)))))))
	   fail (push-ctxt ctxt 'let*-exp4 exp)))

(define (run-var-exp exp env pred succ fail ctxt)
  (lookup-var (var-exp-source-var exp) env pred succ fail
	      (push-ctxt ctxt 'var-exp1 exp)))

(define (run-literal-exp exp env pred succ fail ctxt)
  (literal-to-value (literal-exp-literal exp) env pred succ fail
		    (push-ctxt ctxt 'lit-exp1 exp)))

(define (literal-to-value literal env pred succ fail ctxt)
  (cond
   ((string? literal)
    (run-pred pred (value-maker literal) env succ fail
	      (push-ctxt ctxt 'lit2val1 pred literal)))
   ((looks-like-an-integer? literal)
    (run-pred pred (value-maker literal) env succ fail 
	      (push-ctxt ctxt 'lit2val2 pred literal)))
   (else
    (fail ctxt "Don't know what to make of literal ~a" literal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ENVIRONMENTS

(define (lookup-var var env pred succ fail ctxt)
  (let loop ((env env))
    (cond
     ((assq var (env-rib env)) =>
      (lambda (p)
	(run-pred pred (cdr p) env succ fail
		  (push-ctxt ctxt 'lookup-var1 pred (cdr p)))))
     (else
      (cond
       ((env-parent-env env) => loop)	; otherwise, look in parent
       (else				; or error if no parent
	(fail ctxt "could not find var ~a" var)))))))

(define (top-level-env env)
  (let loop ((env env))
    (if (env-parent-env env)
	(loop (env-parent-env env))
	env)))

(define (update-env-rib! env var val)
  (cond
   ((assq var (env-rib env))
    => (lambda (p)
	 (set-cdr! p val)))
   (else
    (set-env-rib! env 
		  (cons (cons var val)
			(env-rib env))))))

;; return id(s), or '() if no matches found.
(define (reverse-lookup env val)
  (let loop ((env env) (ids '()))
    (if (not env)
	ids
	(let rib-loop ((l (env-rib env)) (rib-ids '()))
	  (if (null? l)
	      (loop (env-parent-env env) (append ids rib-ids))
	      (if (eq? val (cdr (car l)))
		  (rib-loop (cdr l) (cons (car (car l)) rib-ids)) ; found it
		  (rib-loop (cdr l) rib-ids)))))))

(define (reverse-lookup-data env data)
  (let loop ((env env) (ids '()))
    (if (not env)
	ids
	(let rib-loop ((l (env-rib env)) (rib-ids '()))
	  (if (null? l)
	      (loop (env-parent-env env) (append ids rib-ids))
	      (if (eq? data (value-data (cdr (car l))))
		  (rib-loop (cdr l) (cons (car (car l)) rib-ids)) ; found it
		  (rib-loop (cdr l) rib-ids)))))))


;;; PREDEFINED VALUES

(define *true* (value-maker #t))
(define *false* (value-maker #f))
(define (bool-value b) (if b *true* *false*))
(define *undefined* (value-maker #f))

;;; MISC.

;; deduce the list and elt preds for new list
(define (make-ro-list vals env)		; => ro-list
  (let* ((list-pred
	  (list-pred-maker
	   (map (lambda (v) (apply pred-lub env (value-preds v))) vals)))
	 (elt-pred (apply pred-lub env (list-pred-preds list-pred))))
    (ro-list-maker vals list-pred elt-pred)))

(define (default-fail ctxt . args)
  (format #t "~%Failure.  Context: ~%")
  (dump-context (context-maker args ctxt)))

(define (load-dpvml-file fname env succ fail ctxt)
  (format #t "Loading dpvml file ~a...~%" fname)
  (call-with-input-file fname
    (lambda (port)
      (let loop ((sexp (read port)) (last-val *true*))
	(if (eof-object? sexp)
	    (succ last-val)
	    (let ((exp (parse-dpvml-exp sexp)))
	      (run-exp exp env (top-pred-maker)
		       (lambda (v)
			 ;; (format #t "~a~%" v)
			 (loop (read port) v))
		       fail ctxt)))))))

;; for debugging
(define *the-dpvm* #f)

(define (dbg-id/v val)
  (reverse-lookup (dpvm-root-env *the-dpvm*) val))

(define (dbg-id data)
  (reverse-lookup-data (dpvm-root-env *the-dpvm*) data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; public interface to vm

(define (start-dpvm)			; => vm
  (let ((vm (dpvm-maker (initialize-env (env-maker '() #f)))))
    (set! *the-dpvm* vm)
    vm))

(define (run-dpvm-exp-top-level vm exp)	; => value | context
  (run-exp exp
	   (dpvm-root-env vm)
	   (top-pred-maker)
	   (lambda (v) v)
	   (lambda (ctxt . args) (context-maker args ctxt))
	   (context-maker (list "Top") #f)))
  
;; use dpvml as the native language
(define (start-dpvm-repl name xlate1 init-exp)
  (format #t "Type bye to exit (or hit Ctrl-d to send an EOF).~%")
  (let ((vm (start-dpvm)))
    (let ((init-val (run-dpvm-exp-top-level vm init-exp)))
      (if (value? init-val)
	  (let repl ((xlate xlate1) (prompt name))
	    (format #t "~%~a> " prompt)
	    (let ((s-exp (read)))
	      (cond
	       ((equal? s-exp 'bye) (format #t "~%Good bye.~%"))
	       ((equal? s-exp 'dpvml)
		(repl parse-dpvml-exp "dpvml")
		(repl xlate1 prompt))
	       ((eof-object? s-exp)
		(format #t "Leave DPVM? [y to confirm] ")
		(let ((c (read-char)))
		  (if (equal? c #\y)
		      (format #t "~%Good bye.~%")
		      (repl xlate prompt))))
	       (else
		(let* ((exp (xlate s-exp))
		       ;; (iggy (format #t "Got: ~a~%" exp))
		       (v (run-dpvm-exp-top-level vm exp)))
		  (if (value? v)
		      (format #t "~%=> ~a~%" v)
		      (begin
			(format #t "~%Failed: ~%")
			(dump-context v)))
		  (repl xlate prompt))))))
	  (begin
	    (format #t "~%Initial Expression ~a failed: ~%" init-exp)
	    (dump-context init-val))))))

;; default repl uses dpvml for syntax
(define (dpvm-repl)
  (start-dpvm-repl
   "dpvml"
   parse-dpvml-exp
   (parse-dpvml-exp
    `(load-dpvml-file "utils.dpvml"))))

;; Notes:
;; * argument against mushing all let's w/in a fn. into one scope:
;;   (let x ... (let y (fun z ... x ...) (let x ...)...))
;;   The ref to x inside the fun should resolve to the leftmost x, but if
;;   the latter binding to x is in the same env as y, it will get that one.

;; debugging
(define (value->sexp val)
  `(val ,(data->sexp (value-data val))))

(define (data->sexp data)		; => sexp
  (cond
   ((cell? data) `(cell ,(value->sexp (cell-value data)) ,(dbg-id (cell-pred data))))
   ((ro-list? data) `(list ,@(map value->sexp (ro-list-elts data))))
;;   ((closure? data) `(closure ,(closure-vars data) ,(dbg-id (closure-fun-pred data))
;;			      ,(data->sexp (closure-body-exp data))))
   ((closure? data) `(closure ,(dbg-id (closure-fun-pred data)) "..."))
   ((primop? data) `(primop ,(primop-id data)))
   ((env? data) `(env ,(dbg-id data)))
   ((top-pred? data) '(top-pred))
   ((bottom-pred? data) '(bottom-pred))
   ((value-pred? data) `(value-pred ,(data->sexp (value-pred-callable data))))
   ((list-pred? data) `(list-pred ,(map data->sexp (list-pred-preds data))))
   ((elt-pred? data) `(elt-pred ,(data->sexp (elt-pred-pred data))))
   ((fun-pred? data) `(fun-pred ,(data->sexp (fun-pred-args-pred data))
				,(data->sexp (fun-pred-result-pred data))))
   ((eq-pred? data) `(eq-pred ,(data->sexp (eq-pred-value data))))
   ((and-pred? data) `(and-pred ,(map data->sexp (and-pred-preds data))))
   ((or-pred? data) `(or-pred ,(map data->sexp (or-pred-preds data))))
   ((not-pred? data) `(not-pred ,(data->sexp (not-pred-pred data))))
   ((cond-exp? data) `(cond ,(map (lambda (p)
				    (list (data->sexp (car p))
					  (data->sexp (cdr p))))
				  (cond-exp-test-then-exps data))
			    (else ,(data->sexp (cond-exp-else-exp/f data)))))
   ((and-exp? data) `(and ,(map data->sexp (and-exp-conjunct-exps data))))
   ((or-exp? data) `(or ,(map data->sexp (or-exp-disjunct-exps data))))
   ((fun-exp? data) `(lambda ,(fun-exp-vars data)
		       ,(data->sexp (fun-exp-fun-pred-exp data))
		       ,(data->sexp (fun-exp-body-exp data))))
   ((seq-exp? data) `(begin ,@(map data->sexp (seq-exp-exps data))))
   ((let-exp? data) `(let ,(let-exp-loop-id/f data) 
		       ,(map list
			     (let-exp-vars data)
			     (map data->sexp (let-exp-value-exps data)))
		       ,(data->sexp (let-exp-pred-exp data))
		       ,(data->sexp (let-exp-body-exp data))))
   ((let*-exp? data) `(let* ,(map list
				  (let*-exp-vars data)
				  (map data->sexp (let*-exp-value-exps data)))
			,(data->sexp (let*-exp-pred-exp data))
			,(data->sexp (let*-exp-body-exp data))))
   ((call-exp? data) `(,(data->sexp (call-exp-fun-exp data))
		       ,@(map data->sexp (call-exp-arg-exps data))))
   ((var-exp? data) (var-exp-source-var data))
   ((literal-exp? data) `(lit ,(literal-exp-literal data)))
   (else
    `(const ,data))))

;; Scheme48 record format stuff
(define-record-discloser type/value value->sexp)
(define-record-discloser type/cell data->sexp)
(define-record-discloser type/ro-list data->sexp)
(define-record-discloser type/closure (lambda (e) `(closure ,(dbg-id e))))
(define-record-discloser type/primop data->sexp)
(define-record-discloser type/cond-exp data->sexp)
(define-record-discloser type/and-exp data->sexp)
(define-record-discloser type/or-exp data->sexp)
(define-record-discloser type/fun-exp data->sexp)
(define-record-discloser type/seq-exp data->sexp)
(define-record-discloser type/let-exp data->sexp)
(define-record-discloser type/let*-exp data->sexp)
(define-record-discloser type/call-exp data->sexp)
(define-record-discloser type/var-exp (lambda (e) `(var ,(data->sexp e))))
(define-record-discloser type/literal-exp data->sexp)

;; eof
