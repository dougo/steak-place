(module memo mzscheme
  (provide (all-defined))

  (define (memoize proc)
    (memoize* proc (make-hash-table 'equal)))

  (define (memoize* proc ht)
    (lambda args
      (hash-table-get
       ht args
       (lambda ()
	 (let ((answer (apply proc args)))
	   (hash-table-put! ht args answer)
	   answer)))))

  (define-syntax define/memo
    (syntax-rules ()
      ((_ (name . formals) . body)
       (define name (memoize (lambda formals . body))))))

  (define-syntax define/memo*
    (syntax-rules ()
      ((_ (name . formals) ht . body)
       (define name (memoize* (lambda formals . body) ht)))))

  (define-syntax memoize!
    (syntax-rules ()
      ((_ proc)
       (set! proc (memoize proc)))))

  (define-syntax memoize!*
    (syntax-rules ()
      ((_ proc ht)
       (set! proc (memoize* proc ht)))))
)
