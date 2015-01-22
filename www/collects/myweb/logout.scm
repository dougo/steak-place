(module logout mzscheme
  (require (lib "servlet-sig.ss" "web-server"))
  (require (lib "etc.ss"))		;opt-lambda, compose
  (require (lib "list.ss" "srfi" "1"))	;append-map
  (require "cookie.scm")		;cookie-headers
  (require "session.scm")		;expire-session, extract-sessions

  (provide logout)

  (define logout
    (opt-lambda (request (k-url (let ((bindings (request-bindings request)))
				  (if (exists-binding? 'k bindings)
				      (extract-binding/single 'k bindings)
				      "/"))))
      (redirect-to* k-url
		    (append-map (compose cookie-headers expire-session)
				(extract-sessions request)))))

  ;; WORKAROUND: there's no way to add headers to the redirect-to
  ;; response, so we have to make one by hand.
  ;; 
  (require (lib "response.ss" "web-server")) ;make-response/full
  (require (lib "xml.ss" "xml"))	;xexpr->string

  (define (redirect-to* uri env)
    ;; (add-bindings (redirect-to uri see-other) env)
    (make-response/full 303 "See Other" (current-seconds) #"text/html"
			(append env `((location . ,uri)))
			(list (redirect-page uri))))

  ;; Copied from servlet-helpers.ss
  ; : str -> str
  (define (redirect-page url)
    (xexpr->string `(html (head (meta ((http-equiv "refresh") (url ,url)))
                                "Redirect to " ,url)
                          (body (p "Redirecting to " (a ([href ,url]) ,url))))))
)
