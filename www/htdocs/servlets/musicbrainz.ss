(require (lib "unitsig.ss"))
(require (lib "servlet-sig.ss" "web-server"))
(require (lib "response.ss" "web-server"))
(require (lib "resource.ss" "musicbrainz"))
(require (lib "mb-lookup.ss" "chugchanga"))
(require (lib "html.scm" "myweb"))	;make-page

(unit/sig ()
  (import servlet^)

;  (report-errors-to-browser send/back)

  (define (exn->string exn)
    (if (exn? exn)
        (parameterize ([current-error-port (open-output-string)])
	  ((error-display-handler) (exn-message exn) exn)
          (get-output-string (current-error-port)))
        (format "~s\n" exn)))

  (current-exception-handler
   (lambda (exn)
     (send/back
      `(html (head (title "Servlet Error"))
	     (body ([bgcolor "white"])
		   (p "The following error occured: "
		      (pre ,(exn->string exn))))))))

  (define bindings (request-bindings initial-request))

  (define (extract-optional-binding/single sym)
    (if (exists-binding? sym bindings)
	(extract-binding/single sym bindings)
	""))

  (define artist (extract-optional-binding/single 'artist))
  (define album (extract-optional-binding/single 'album))

  (define (make-search-form)
    `(form
      ((action ""))
      (table
       ,(text-field "Artist:" "artist" artist)
       ,(text-field "Album:" "album" album))))

  (define (make-search-page title body)
    (make-page title `((p ,(make-search-form)) ,@body)))

  (define (resource->xexprs resource)
    `((a ((href ,(resource-url resource)))
	 ,@(resource-content->xexprs resource))))

  (define (resource-content->xexprs resource)
    (cond ((artist? resource)
	   `(,(artist-name resource)
	     " (" ,(artist-sortname resource) ")"))
	  ((album? resource)
	   `(,@(resource-content->xexprs (album-artist resource))
	     " " (cite ,(album-title resource))))
	  (else
	   `(,(resource-title resource)))))

  (define (lookup-result->xexpr lookup-result)
    `(tr ,@(if (pair? lookup-result)
	       `((td ,(number->string (car lookup-result)) "%"))
	       `())
	 (td ,@(resource->xexprs
		(if (pair? lookup-result) (cdr lookup-result) lookup-result)))))

  (define (result->xexprs result)
    (cond ((resource? result) (resource->xexprs result))
	  ((list? result) `((table ,@(map lookup-result->xexpr result))))
	  (else null)))

  (current-output-port (open-output-string))

  (define search-result (time (mb-lookup artist album)))

  (define result-xexprs
    (map (lambda (result-part)
	   (cons 'p (result->xexprs result-part)))
	 search-result))

  (make-search-page
   (if (null? result-xexprs) "Search" "Search results")
   (append result-xexprs
	   `((p (pre ,(get-output-string (current-output-port)))))))
)
