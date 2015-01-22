;; Add bindings to a web-server response.

(module add-bindings mzscheme
  (require (lib "servlet-sig.ss" "web-server")
	   (lib "response.ss" "web-server")
	   (lib "xml.ss" "xml"))
  (provide add-bindings)

  ;; add-bindings : response (listof (cons symbol string)) -> response
  (define (add-bindings response bindings)
    (cond ((response/full? response)
	   ((if (response/incremental? response)
		make-response/incremental
		make-response/full)
	    (response/basic-code response)
	    (response/basic-message response)
	    (response/basic-seconds response)
	    (response/basic-mime response)
	    (append bindings (response/basic-extras response))
	    (response/full-body response)))
	  ((and (list? response) (string? (car response)))
	   (make-response/full
	    200 "Okay" (current-seconds) (car response)
	    bindings
	    (cdr response)))
	  (else
	   (make-response/full
	    200 "Okay" (current-seconds) "text/html"
	    bindings
	    (list (with-handlers ([void (lambda (exn)
					  (if (exn? exn)
					      (exn-message exn)
					      (format "~s" exn)))])
				 (xexpr->string response)))))))
)
