;;	$Id: predicates.scm,v 1.8 2000/03/09 17:39:21 gregs Exp $	
;;
;; Predicates (types) for the Dynamic Predicate Virtual Machine (DPVM)

(define-record-type top-pred () ())	; every pred implies top
(define-record-type bottom-pred () ())	; no pred implies bottom
;; value-preds are applicable to values  -- callables are closures or primops
(define-record-type value-pred (callable) ())
;; list-preds are applicable to list values -- test length and then each elt. separately.
(define-record-type list-pred (preds) ())
;; elt-preds are applicable to list values -- apply same pred to each elt. 
(define-record-type elt-pred (pred) ())	
;; fun-preds are applicable to callable (closures or primops) values
(define-record-type fun-pred (args-pred result-pred) ())
;; eq-preds are applicable to values
(define-record-type eq-pred (value) ())
;; predicate algebra:
(define-record-type and-pred (preds) ())
(define-record-type or-pred (preds) ())
(define-record-type not-pred (pred) ())

;; *implies* is reflexive, transitive and antisymmetric (a partial order) 
;; *implies* is a subset of value-preds * preds

(define *implies* (list))
;; p1 : value-pred,  p2 : value-pred | not-pred
(define (value-pred-implies? p1 p2)
  (assert (value-pred? p1) "value-pred-implies, p1 not a value-pred: ~a" p1)
  (cond ((assoc p1 *implies*)
	 => (lambda (pr) (member p2 (cdr pr))))
	(else #f)))

(define (value-pred-implied-preds p)	; => preds
  (cond ((assoc p *implies*)
	 => (lambda (pr) (cdr pr)))
	(else '())))

(define (add-implies! p1 preds)
  (cond
   ((assoc p1 *implies*)
    => (lambda (pair)
	 (let ((old-preds (cdr pair)))
	   (let ((new-preds		; Oh, and BTW, only add new preds.
		  (filter (lambda (pred) (not (member pred old-preds)))
			  preds)))
	     (set-cdr! pair (append old-preds new-preds))))))
   (else
    (set! *implies* (cons (cons p1 preds) *implies*)))))

;; *** note that caching passed predicates only works if predicates do not
;; *** access mutable state.  
;; ***** Not yet checked *****
;;
;; Note that passed list-pred's are not cached.  The idea is that list values are
;; more transient than individual element values.  Should prolly be tuneable.
;; What about and & or preds, fun-preds?
;; Should we cache pred.s that pass by implication?

;; if true, invoke succ w/ v
;; if fails, invoke fail.
(define (run-pred pred v env succ fail ctxt)
  (run-pred-internal pred v env succ fail ctxt '()))

(define (run-pred-internal pred v env succ fail ctxt implies-stack)
  (cond
   ((top-pred? pred)
    (succ v))
   ((not (value? v))			; this should never happen
    (error "Not a value for predicate, v: ~a, pred: ~a" v pred))
   ((memq pred (value-preds v))		; passed this already?
    (succ v))
   ;; it's important to test implies before running pred function
   ;; -- so that can support explicit implied preds and have a 
   ;;    pred function that always fails
   ((preds-imply?-internal
     (value-preds v) pred env implies-stack) ; passed by implication?
    (succ v))				; cache success?????
   ((bottom-pred? pred)
    (fail ctxt "Nobody passes bottom! Pred: ~a, val: ~a" pred v))
   ((and-pred? pred)
    (if (null? (and-pred-preds pred))
	(fail ctxt "Empty pred list for and-pred ~a, v: ~a" pred v)
	(let loop ((preds (and-pred-preds pred)))
	  (if (null? (cdr preds))
	      (run-pred-internal (car preds) v env succ fail
				 (push-ctxt ctxt 'and-pred1 (car preds))
				 implies-stack)
	      (run-pred-internal (car preds) v env
				 (lambda (v) (loop (cdr preds))) ; success -> loop
				 fail (push-ctxt ctxt 'and-pred2 (car preds))
				 implies-stack)))))
   ((or-pred? pred)
    (if (null? (or-pred-preds pred))
	(fail ctxt "Empty pred list for or-pred ~a, v: ~a" pred v)
	(let loop ((preds (or-pred-preds pred)))
	  (if (null? (cdr preds))
	      (run-pred-internal (car preds) v env succ fail
				 (push-ctxt ctxt 'or-pred2 (car preds))
				 implies-stack)
	      (run-pred-internal (car preds) v env succ 
				 (lambda ignored (loop (cdr preds))) ; fail -> loop
				 (push-ctxt ctxt 'or-pred1 (car preds))
				 implies-stack)))))
   ((not-pred? pred)
    (run-pred-internal (not-pred-pred pred) v env
		       (lambda (v) (fail ctxt 'not-pred1 pred v)) ; success is bad
		       (lambda ignored (succ v)) ; failure is good
		       (push-ctxt ctxt 'not-pred2 pred)
		       implies-stack))
   ((list-pred? pred)
    (cond
     ((not (ro-list? (value-data v)))
      (fail ctxt "cannot run list-pred on non-list, pred: ~a, v: ~a" pred v))
     ((not (= (length (list-pred-preds pred))
	      (length (ro-list-elts (value-data v)))))
      (fail ctxt "List ~a not same length as list-pred ~a" v pred))
     (else				; a right-sized list
      ;; 1st check list-pred of list
      (cond				; not entirely clear if this is a good idea
       ((pred-implies?-internal (ro-list-list-pred (value-data v)) pred
				env implies-stack)
	(succ v))
       ((pred-implies?-internal (ro-list-list-pred (value-data v)) (make-not-pred pred)
				env implies-stack)
	(fail ctxt "List pred ~a for list ~a proves list-pred ~a false."
	      (ro-list-list-pred (value-data v)) v pred))
       (else				; need to actually check
	(let loop ((preds (list-pred-preds pred))
		   (vals (ro-list-elts (value-data v))))
	  (if (null? preds)		; note:  empty list of preds is OK
	      (begin
		(set-ro-list-list-pred! (value-data v) pred) ; update list-list-pred
		(succ v))
	      (run-pred-internal (car preds) (car vals) env
				 (lambda (ig) (loop (cdr preds) (cdr vals))) ; succ -> loop
				 fail (push-ctxt ctxt 'list-pred1 (car preds))
				 implies-stack))))))))
   ((fun-pred? pred)
    (if (not (callable? v))
	(fail ctxt "cannot run fun-pred on non-callable, pred: ~a, v: ~a" pred v)
	(if (pred-implies?-internal (callable-fun-pred (value-data v)) pred
				    env implies-stack)
	    (succ v)
	    (fail ctxt "fun pred: pred of closure ~a does not imply pred ~a"
		  v pred))))
   ((elt-pred? pred)
    (if (not (ro-list? (value-data v)))
	(fail ctxt "elt-pred ~a does not apply to non-list value ~a"
	      pred v)
	;; 1st check elt-pred of list
	(cond
	 ((pred-implies?-internal (ro-list-elt-pred (value-data v))
				  (elt-pred-pred pred) env implies-stack)
	  (succ v))
	 ((pred-implies?-internal (ro-list-elt-pred (value-data v))
				  (make-not-pred (elt-pred-pred pred))
				  env implies-stack)
	  (fail ctxt "List elt pred ~a for list ~a proves elt-pred ~a false."
		(ro-list-elt-pred v) v pred))
	 ;; otherwise, check each elt, and if success, update list's elt-pred
	 (else
	  (let loop ((elts (ro-list-elts (value-data v))))
	    (if (null? elts)		; finished with no mishaps
		(begin			; success
		  (set-ro-list-elt-pred! (value-data v) (elt-pred-pred pred))
		  (succ v))
		(run-pred-internal (elt-pred-pred pred) (car elts) env
				   (lambda (v) (loop (cdr elts)))
				   fail (push-ctxt ctxt 'elt-pred2 pred (car elts))
				   implies-stack)))))))
   ((eq-pred? pred)
    (if (eq? (value-data v) (value-data (eq-pred-value pred)))
	(succ v)
	(fail ctxt "Eq pred. ~a failed on value ~a" pred v)))
   ((value-pred? pred) 
    (run-callable-unchecked
     (value-pred-callable pred) env (list v)
     (lambda (ans)
       (if (eq? *true* ans)
	   (begin
	     (update-value-preds! v pred) ; cache success
	     (succ v))
	   (fail ctxt "Predicate failed, pred: ~a, v: ~a" pred v)))
     fail (push-ctxt ctxt 'val-pred1 pred v)))
   (else
    (fail ctxt "Run-pred, unknown type of pred ~s" pred))))

(define (pred-implies? p1 p2 env)
  (pred-implies?-internal p1 p2 env '()))

(define (pred-implies?-internal p1 p2 env stack)
  (if (member (cons p1 p2) stack)
      #f
      (let ((stack (cons (cons p1 p2) stack)))
	(cond
	 ((eq? p1 p2) #t)		; implies is reflexive
	 ((top-pred? p2) #t)		; everybody implies top
	 ((top-pred? p1) #f)		; top implies nobody else
	 ((bottom-pred? p1) #t)		; bottom implies everybody
	 ((bottom-pred? p2) #f)		; nobody implies bottom (who's not bottom)

	 ;; decompose connectives
	 ;; p1_1 & p1_2 & ... & p1_n =?=> p2    -- true if any p1_i implies p2
	 ((and-pred? p1)
	  (any? (rcurry pred-implies?-internal p2 env stack)
		(and-pred-preds p1)))
	 ;; p1_1 | p1_2 | ... | p1_n =?=> p2    -- true if all p1_i imply p2
	 ((or-pred? p1)
	  (every? (rcurry pred-implies?-internal p2 env stack)
		  (or-pred-preds p1)))
	 ((not-pred? p1)
	  (pred-implies?-internal (not-pred-pred p1)
				  (make-not-pred p2) env stack))
	 ((and-pred? p2)
	  (every? (lambda (p) (pred-implies?-internal p1 p env stack))
		  (and-pred-preds p2)))
	 ((or-pred? p2)
	  (any? (lambda (p) (pred-implies?-internal p1 p env stack))
		(or-pred-preds p2)))
	 ((eq-pred? p1)
	  ;; Note: can't do: (preds-imply? (value-preds (eq-pred-value p1)) p2))
	  ;;   because (imply? (eq-pred v) p2) will loop if (value-preds v)
	  ;;   contains (eq-pred v) -- which is the case for #t, for example.
	  (run-pred-internal p2 (eq-pred-value p1) env
			     (lambda (v) #t)
			     (lambda fail-args #f)
			     (context-maker (list "For (pred-implies? ~a ~a), running p2 on ~a"
						  p1 p2 (eq-pred-value p1)) #f)
			     stack))
	 ((eq-pred? p2)
	  (and (eq-pred? p1) (equal? (eq-pred-value p1)
				     (eq-pred-value p2))))
	 ((list-pred? p1)
	  (and (list-pred? p2)
	       (= (length (list-pred-preds p1))
		  (length (list-pred-preds p2)))
	       (every? identity
		       (map (rcurry pred-implies?-internal env stack)
			    (list-pred-preds p1)
			    (list-pred-preds p2)))))
	 ((fun-pred? p1)
	  (and (fun-pred? p2)		; note: fun-preds are contra-positive in args
	       (pred-implies?-internal (fun-pred-args-pred p2)
				       (fun-pred-args-pred p1) env stack)
	       (pred-implies?-internal (fun-pred-result-pred p1)
				       (fun-pred-result-pred p2) env stack)))
	 ((elt-pred? p1)
	  (and (elt-pred? p2)
	       (pred-implies?-internal (elt-pred-pred p1) (elt-pred-pred p2)
				       env stack)))
	 ((value-pred? p1)
	  ;; base case for p1
	  (cond
	   ((value-pred-implies? p1 p2) #t) ; use the table
	   ((and (not-pred? p2)		; special case for nots
		 (value-pred-implies? (not-pred-pred p2)
				      (make-not-pred p1)))
	    #t)
	   (else
	    (preds-imply?-internal (value-pred-implied-preds p1) p2 env stack))))))))
     
(define (preds-imply?-internal preds p2 env implies-stack)
  (any? (lambda (p1)
	  (pred-implies?-internal p1 p2 env implies-stack))
	preds))

;; least upper bound of preds 
(define pred-lub			; => pred
  (lambda (env . preds)
    (let ((lub2 (lambda (p1 p2)
		  (cond ((pred-implies? p1 p2 env) p2)
			((pred-implies? p2 p1 env) p1)
			(else (make-or-pred env p1 p2))))))
      (reduce lub2 (top-pred-maker) preds))))

(define make-and-pred
  (lambda (env . preds)
    ;; first get flat list of conjuncts (preserving order when possible)
    (let ((preds (let loop ((preds preds) (new '()))
		   (cond
		    ((null? preds)
		     (reverse new))
		    ((and-pred? (car preds))
		     (loop (cdr preds)
			   (append (reverse (and-pred-preds (car preds))) new)))
		    (else
		     (loop (cdr preds) (cons (car preds) new)))))))
      ;; next see if can merge any conjuncts
      (let loop ((preds preds) (new '()))
	(if (null? preds)
	    (if (= (length new) 1)
		(car new)
		(and-pred-maker new))
	    (let ((this-pred (car preds)))
	      ;; merge this-pred into new
	      (if (any? (rcurry pred-implies? this-pred env) new) ; can we ignore this-pred?
		  (loop (cdr preds) new) ; fuggedaboutit
		  (loop (cdr preds)	; can we remove some other preds?
			(cons this-pred
			      (filter (lambda (p) (not (pred-implies? this-pred p env)))
				      new))))))))))

(define make-or-pred
  (lambda (env . preds)
    ;; first get flat list of disjuncts (preserving order when possible)
    (let ((preds (let loop ((preds preds) (new '()))
		   (cond
		    ((null? preds)
		     (reverse new))
		    ((or-pred? (car preds))
		     (loop (cdr preds)
			   (append (reverse (or-pred-preds (car preds))) new)))
		    (else
		     (loop (cdr preds) (cons (car preds) new)))))))
      ;; next see if can merge any disjuncts
      (let loop ((preds preds) (new '()))
	(if (null? preds)
	    (if (= (length new) 1)
		(car new)
		(or-pred-maker new))
	    (let ((this-pred (car preds)))
	      ;; merge this-pred into new
	      (if (any? (lambda (p) (pred-implies? this-pred p env))
			new)		; can we ignore this-pred?
		  (loop (cdr preds) new) ; fuggedaboutit
		  (loop (cdr preds) ; can we remove some other preds?
			(cons this-pred
			      (filter (lambda (p) (not (pred-implies? p this-pred env)))
				      new))))))))))

(define (make-not-pred p)
  (cond
   ((not-pred? p) p)			; not(not(p)) = p
   ((and-pred? p)
    (or-pred-maker (map make-not-pred (and-pred-preds p))))
   ((or-pred? p)
    (and-pred-maker (map make-not-pred (or-pred-preds p))))
   (else (not-pred-maker p))))

(define (update-value-preds! v pred)
  (if (not (or (top-pred? pred)
	       (member pred (value-preds v))))
      (set-value-preds! v (cons pred (value-preds v)))))

;; simple primop always invokes succ
;; -- fun takes a (Scheme) list of values and returns a value
(define (make-simple-primop pred fun id)
  (primop-maker pred
		(lambda (vals env succ fail ctxt)
		  (succ (apply fun vals)))
		id))

;; Predefined predicates

(define *true-pred* (eq-pred-maker *true*))
(define *false-pred* (eq-pred-maker *true*))
(define *boolean-pred* (or-pred-maker (list *true-pred* *false-pred*)))
(set-value-preds! *true* (list *true-pred* *boolean-pred*))
(set-value-preds! *false* (list *true-pred* *boolean-pred*))

(define *closure-pred*
  (value-pred-maker 
   (make-simple-primop
    (top-pred-maker)
    (lambda (v) (bool-value (closure? (value-data v))))
    '*closure-pred*)))

(define (callable? v)
  (or (closure? (value-data v)) (primop? (value-data v))))

(define *callable-pred*			; a closure or a primop
  (value-pred-maker 
   (make-simple-primop
    (top-pred-maker)
    (lambda (v)
      (bool-value (callable? v)))
    '*callable-pred*)))

(define *fun-pred-pred*
  (value-pred-maker 
   (make-simple-primop
    (top-pred-maker)
    (lambda (v) (bool-value (fun-pred? (value-data v))))
    '*fun-pred-pred*)))

(define (pred? d)
  (or (top-pred? d) (bottom-pred? d)
      (value-pred? d) (list-pred? d)
      (elt-pred? d) (fun-pred? d)
      (eq-pred? d) (not-pred? d)
      (and-pred? d) (or-pred? d)))

(define *pred-pred*
  (value-pred-maker 
   (make-simple-primop
    (top-pred-maker)
    (lambda (v)
      (bool-value (pred? (value-data v))))
    '*pred-pred*)))

(define *pred-list-pred*
  (elt-pred-maker *pred-pred*))

(define-record-discloser type/top-pred data->sexp)
(define-record-discloser type/bottom-pred data->sexp)
(define-record-discloser type/value-pred data->sexp)
(define-record-discloser type/list-pred data->sexp)
(define-record-discloser type/elt-pred data->sexp)
(define-record-discloser type/fun-pred data->sexp)
(define-record-discloser type/eq-pred data->sexp)
(define-record-discloser type/and-pred data->sexp)
(define-record-discloser type/or-pred data->sexp)
(define-record-discloser type/not-pred data->sexp)

;; eof
