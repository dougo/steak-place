#lang scheme

(require web-server/http)		;request
(require web-server/http/response-structs) ;response/full
(require "names.scm")
(require "email.scm")
;(require "cookie.scm")		        ;cookie-headers
(require xml)				;xexpr->string
(provide (all-defined-out))

(define (make-page title body)
  (make-page* title body null))

;; TO DO: use PLT cookie procs
;(define (make-page/cookie title body cookie)
;  (make-page* title body (cookie-headers cookie)))

(define (make-page* title body extras)
  (xexpr->response/full
   (make-page-xexpr title body)
   (append extras (list (make-header #"Cache-Control" #"no-cache")))))

(define (xexpr->response/full xexpr extras)
  (response/full
   200 #"Okay" (current-seconds) #"text/html;charset=utf-8" extras
   (list (with-handlers ([void (lambda (exn)
				 (if (exn? exn)
				     (exn-message exn)
				     (format "~s" exn)))])
           (string->bytes/utf-8
            (xexpr->string xexpr))))))

(define *footer*
  '((a ((href "/")) "The Steak Place")
    " powered by "
    (a ((href "http://www.plt-scheme.org/")) "PLT")))

(define (make-page-xexpr title body)
  (make-page-xexpr*
   title `(,title) body *footer*))

(define (make-page-xexpr* title head body foot)
  `(html
    (head (title ,title))
    (body (h1 (hr) ,@head)
	  ,@body
	  (hr)
	  (address ,@foot))))

(define (make-error-response code name message)
  (response/full
   code (string->bytes/utf-8 name) (current-seconds)
   #"text/html;charset=utf-8" '()
   (list (string->bytes/utf-8
	  (xexpr->string
	   `(html
	     (head (title ,name))
	     (body (p ,message) (address ,@*footer*))))))))

(define (make-forbidden-response)
  (make-error-response
   403 "Forbidden"
   "You don't have permission to access that directory on this server."))

(define (make-not-found-response)
  (make-error-response
   404 "Not Found"
   "The file you were looking for was not found on this server."))

(define (make-session-page uid title uri k-url body headers)
  (make-page*
   title
   `((p "You are: " ,(get-fullname uid) " <" ,(get-email uid) "> "
	(a ((href ,(string-append "/servlets/account.scm?k=" uri)))
	   "Edit account")
	" | "
	(a ((href ,(string-append "/servlets/logout.scm?k=" k-url)))
	   "Logout"))
     ,@body)
   headers))

(define (input-field type prompt name value . attrs)
  `(tr (td (label ((for ,name)) ,prompt))
       (td (input ((type ,type) (name ,name) (value ,value) ,@attrs)))))

(define (text-field prompt name value . attrs)
  (apply input-field "text" prompt name value attrs))
(define (password-field prompt name)
  (input-field "password" prompt name ""))

(define (textarea-field prompt name rows cols value . attrs)
  `(tr (td (label ((for ,name)) ,prompt))
       (td (textarea ((name ,name)
		      (rows ,(number->string rows))
		      (cols ,(number->string cols))
		      ,@attrs)
		     ,value))))

#|
;; Insert line breaks.
(define (make-lines l)
  (if (or (null? l) (null? (cdr l)))
      l
      `(,(car l) (br) ,@(make-lines (cdr l)))))

(define (display-headers request)
  (display-environment (request-headers request)))

(define (display-bindings request)
  (display-environment (request-bindings request)))

(define (display-environment environment)
  (make-lines (map display-binding environment)))

(define (display-binding binding)
  (string-append (symbol->string (car binding)) ": " (cdr binding)))
|#