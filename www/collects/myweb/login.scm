(module login mzscheme
  (require (lib "servlet-sig.ss" "web-server")) ;struct request
  (require (lib "url.ss" "net"))	;url-path, url->string
  (require "password.scm" "email.scm" "session.scm" "html.scm")
  (require "cookie.scm")		;cookie-headers

  (provide (all-defined))

  ;; Continue with the requester's uid and an environment of headers
  ;; to be added to the response.
  ;; login-if-needed : request (uid env -> response) -> response
  (define (login-if-needed request k)
    (let ((uid (extract-uid request)))
      (if uid
	  (k uid null)
	  (login request k))))

  ;; Ask the user to login, or extract the uid and environment and continue.
  ;; login : request (uid env -> response) -> response
  (define (login request k)
    (let ((bindings (request-bindings request))
	  (uri (request-relative-uri-string request)))
      (if (and (exists-binding? 'email bindings)
	       (exists-binding? 'pass bindings))
	  (try-login
	   uri
	   (extract-binding/single 'email bindings)
	   (extract-binding/single 'pass bindings)
	   k)
	  (make-login-page
	   uri "Welcome to the Steak Place!  Please login." ""))))

  ;; Continue with a valid uid and an environment containing a new
  ;; session cookie, or ask again.
  ;; try-login : uri str str (uid env -> response) -> response
  (define (try-login uri email pass k)
    (let ((uid (email->uid email)))
      (if (and uid (correct-password? uid pass))
	  (k uid (cookie-headers (make-session-cookie uid)))
	  (make-login-page
	   uri
	   "Sorry, there is no account with that email address and password; try again."
	   email))))

  ;; Make a form asking the user for login information, continuing on to uri.
  ;; make-login-page : uri xexpr str -> response
  (define (make-login-page uri prompt email)
    (make-page
     "Login"
     `((p ,prompt
	  "  (Or " (a ((href ,(string-append "/servlets/account.scm?k=" uri)))
		      "create a new account")
	  ".)")
       (form ((action ,uri) (method "post"))
	     (p (table
		 ,(text-field "Email address: " "email" email)
		 ,(password-field "Password: " "pass")))
	     (p (input ((type "submit") (value "Login")))
		(input ((type "reset")))))
       )))

  ;; Extract just the relative path from a URL, discarding the query
  ;; and fragment.
  ;; FIXME: this should be moved to some other module.
  ;; url->relative-url : url -> url
  (define (url->relative-url url)
    (make-url #f #f #f #f (url-path url) null #f))

  (define (request-relative-uri-string request)
    (url->string (url->relative-url (request-uri request))))
)
