(module try mzscheme
  (provide try)

  (define-syntax try
    (syntax-rules (finally)
      ((_ expr catch-clause ... (finally finally-expr ...))
       (let ((finalizer (lambda () finally-expr ...)))
	 (with-handlers ((void (lambda (exn) (finalizer) (raise exn))))
	   (begin0 (try expr catch-clause ...) (finalizer)))))
      ((_ expr ((exn? exn) catch-expr ...) ...)
       (with-handlers ((exn? (lambda (exn) catch-expr ...))
		       ...)
	 expr))))

)
