(require (lib "unitsig.ss"))
(require (lib "servlet-sig.ss" "web-server"))
(require (lib "resource.ss" "musicbrainz"))
(require (lib "mb-lookup.ss" "chugchanga"))
(require (lib "poll.scm" "chugchanga"))
(require (lib "canon.ss" "chugchanga"))
(require (lib "html.scm" "myweb"))
(require (lib "db.ss" "db"))

(unit/sig ()
  (import servlet^)

  (report-errors-to-browser send/back)

  (define bindings (request-bindings initial-request))

  (define uid (string->number (extract-binding/single 'uid bindings)))
  (define number (string->number (extract-binding/single 'number bindings)))

  (define fullname
    (with-connection-to-db
     "chugchanga" "dougo"
     (lambda () (get-fullname uid (lambda () (format "User #~a" uid))))))

  (define title (format "~a's Favorite Record #~a" fullname number))

  (define entry
    (with-connection-to-db
     "chugchanga" "dougo"
     (lambda () (get-entry uid))))

  (define record (list-ref (entry-records entry) number))


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

  (define-values (albums album-result artist-result)
    (time (mb-lookup (record-artist record) (record-name record))))

  (define result-xexprs
    (map (lambda (result-part)
	   (cons 'p (result->xexprs result-part)))
	 (list albums album-result artist-result)))

  (make-page
   title
   `((p (a ((href ,(format "?uid=~a;number=~a" uid (sub1 number)))) "previous")
	" | "
	(a ((href ,(format "?uid=~a;number=~a" uid (add1 number)))) "next"))
     (p (ul ,@(record->xexprs record)))
     ,@result-xexprs
     (p (pre ,(get-output-string (current-output-port))))
     ))
)
