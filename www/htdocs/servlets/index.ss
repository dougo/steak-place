#lang web-server

(provide interface-version start)
(define interface-version 'stateless)

(require myweb/html       ;*footer*
	 net/url
	 xml/xml
	 srfi/1
	 srfi/13)

;; TO DO: use request-bindings/raw
(require web-server/http/bindings)

;;; TODO: parent directory, column sorting


(define *htdocs* "../../../public")

(define (access-allowed? root path)
  (and (path? path)
       (not (equal? root path))
       (or (file-exists? (build-path path ".htaccess"))
	   (access-allowed?
	    root
	    (let-values (((base name must-be-dir?) (split-path path)))
	      base)))))

(define *months* #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
		   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define (month->string month)
  (vector-ref *months* (sub1 month)))

(define (add-zero n)
  (if (< n 10)
      (string-append "0" (number->string n))
      n))

(define (last-modified path)
  (match-let (((struct date (second minute hour day month year
			     week-day year-day dst? time-zone-offset))
	       (seconds->date (file-or-directory-modify-seconds path))))
    (format "~a-~a-~a ~a:~a"
	    (add-zero day) (month->string month) year
	    (add-zero hour) (add-zero minute))))

(define (url-path->path base url-path)
  (apply build-path base (regexp-split #rx"/" url-path)))

(define (directory-xexprs url-path env)
  (let ((dir-path (url-path->path *htdocs* url-path)))
    (unless (directory-exists? dir-path)
      (make-not-found-response))
    (unless (and (relative-path? url-path)
		 (access-allowed? *htdocs* dir-path))
      (make-forbidden-response))
    (map
     (lambda (path)
       (let* ((file-path (build-path dir-path path))
	      (path (if (directory-exists? file-path)
			(string-append path "/")
			path))
	      (url-path (if (directory-exists? file-path)
			    (string-append "?d=" url-path "/" path)
			    (string-append "/" url-path "/" path)))
	      (size (if (file-exists? file-path)
			(string-append
			 (number->string
			  (ceiling (/ (file-size file-path) 1024)))
			 "k")
			"-"))
	      (exists? (or (file-exists? file-path)
			   (directory-exists? file-path)))
	      (link (if exists? `(a ((href ,url-path)) ,path) path))
	      (modified (if exists?
			    (last-modified file-path)
			    "<broken link>")))
	 `(tr (td ,link "        ")
	      (td ,modified "  ")
	      (td ((align "right")) ,size))))
     (sort (remove (lambda (s) (string-prefix? "." s))
		   (map path->string (directory-list dir-path)))
	   string<?))))

(define (start initial-request)
  (let* ((env (request-bindings initial-request))
	 (dir (extract-binding/single 'd env))
	 (dir (if (string-suffix? "/" dir)
		  (substring dir 0 (sub1 (string-length dir)))
		  dir))
	 (index (directory-xexprs dir env)))
    (response/xexpr
     (make-page-xexpr*
      dir `("Index of /" ,dir)
      `((hr)
        (pre
         (table (tr ((align "left"))
                    (th "Name        ")
                    (th "Last Modified    ")
                    (th ((align "right")) "Size"))
                ,@index)
         ))
      *footer*)
     )))
