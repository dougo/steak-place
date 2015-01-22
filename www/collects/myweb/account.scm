(module account mzscheme
  (require (lib "servlet-sig.ss" "web-server")
	   (lib "url.ss" "net")		;url-path
	   (lib "list.ss")		;filter
	   "session.scm"
	   "email.scm"
	   "password.scm"
	   "names.scm"
	   "html.scm"
	   "cookie.scm")
  (require "login.scm")			;url->relative-url

  (provide edit-account)

  ;; If the user is currently logged in (i.e. has a session), edit his
  ;; account, otherwise create a new one.
  (define (edit-account request)
    (let ((uid (extract-uid request))
	  (bindings (request-bindings request))
	  (url (request-uri request)))
      (let ((relative-url (url->relative-url url))
	    (query (url-query url)))
	(set-url-query! relative-url query)
	(let ((uri (url->string relative-url))
	      (k-url (cond ((assq 'k query) => cdr) (else "/"))))
	  (if uid
	      (update-account uid bindings uri k-url)
	      (if (exists-binding? 'email bindings)
		  ;; Assume we got here from a form submission.
		  (let ((email (extract-binding/single 'email bindings))
			(name (extract-binding/single 'name bindings))
			(pass (extract-binding/single 'pass bindings))
			(pass2 (extract-binding/single 'pass2 bindings)))
		    (let ((result (make-account email name pass pass2)))
		      (if (string? result)
			  (make-account-page #f uri result email name)
			  (make-success-page k-url result))))
		  ;; Otherwise, the form has not been submitted yet.
		  (make-account-page
		   #f uri "Welcome to the Steak Place!" "" "")))))))

  ;; Return the uid of the new account, or an error message string.
  (define (make-account email name pass pass2)
    (or (problem-with-email? email)
	(problem-with-passwords? pass pass2)
	(let ((uid (make-user pass)))
	  (unless (string-empty? name)
	    (set-fullname! uid name))
	  (set-email! uid email)
	  uid)))

  (define (update-account uid bindings uri k-url)
    (make-account-page
     #t
     uri
     `(span
       ,@(make-lines
	  `(,@(if (exists-binding? 'email bindings)
		  (filter string? (list (update-name uid bindings)
					(update-email uid bindings)
					(update-password uid bindings)))
		  '())
	    (a ((href ,k-url)) "Go back to where you came in"))))
     (get-email uid)
     (get-fullname uid (lambda () ""))))

  (define (update-email uid bindings)
    (let ((email (extract-binding/single 'email bindings)))
      (or (equal? email (get-email uid))
	  (problem-with-email? email)
	  (begin
	    (set-email! uid email)
	    "Your email address has been updated, and mail has been sent."))))

  (define (update-name uid bindings)
    (let ((name (extract-binding/single 'name bindings)))
      (or (equal? name (get-fullname uid (lambda () "")))
	  (begin
	    (set-fullname! uid name)
	    "Your full name has been updated."))))

  (define (update-password uid bindings)
    (let ((pass (extract-binding/single 'pass bindings))
	  (pass2 (extract-binding/single 'pass2 bindings)))
      (or (and (string-empty? pass) (string-empty? pass2))
	  (problem-with-passwords? pass pass2)
	  (begin
	    (set-password! uid pass)
	    "Your password has been updated."))))

  (define (make-account-page edit? uri prompt email name)
    (make-page
     (if edit? "Edit Account" "New Account")
     `((p ,prompt)
       (form ((action ,uri) (method "post"))
	     (p (table
		 ,(text-field "Your full name: " "name" name)
		 ,(text-field "Your email address: " "email" email)
		 ,(password-field (if edit?
				      "New password: "
				      "Select a password: ") "pass")
		 ,(password-field "Repeat the password: " "pass2")))
	     (p (input ((type "submit")
			(value ,(if edit? "Save changes" "Create account"))))
		(input ((type "reset"))))))
     ))

  (define (make-success-page k-url uid)
    (make-page*
     "Account Created"
     `((p "Your account has been created." (br)
	  "Mail has been sent to " ,(get-email uid) "." (br)
	  "Welcome to the Steak Place, " ,(get-fullname uid) "!" (br)
	  "You can " (a ((href ,k-url)) "go back to where you came in."))))
    (cookie-headers (make-session-cookie uid)))


  (define (problem-with-email? email)
    (or (and (string-empty? email)
	     "An email address is required.")
	(and (email->uid email)
	     "There is already a user with that email address.")
	(and (not (memq #\@ (string->list email)))
	     "That is not a valid email address.")))

  (define (problem-with-passwords? pass pass2)
    (or (and (not (string=? pass pass2))
	     "Those passwords do not match.")
	(and (string-empty? pass)
	     "Please choose a password.")))

  (define (string-empty? str)
    (string=? str ""))

)
