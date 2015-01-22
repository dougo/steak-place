(module mb-persist mzscheme
  (require (lib "plt-match.ss"))	;match-lambda
  (require "persist.ss")		;read-from-file, write-to-file
  (require "mb.ss")			;struct result, artist, album

  (define (save-mb)
    (save-search *artist-search* "artist-search")
    (save-search *album-search* "album-search"))

  (define (load-mb)
    (set! *artist-search* (load-search "artist-search"))
    (set! *album-search* (load-search "album-search")))


  (define (save-search search path)
    (write-to-file (search->datum search) path))

  (define (load-search path)
    (datum->search (read-from-file path)))


  (define (search->datum search)
    (hash-table-map search (lambda (k v) (cons k (result->datum v)))))

  (define (datum->search datum)
    (let ((ht (make-hash-table 'equal)))
      (for-each
       (match-lambda
	 ((list-rest search-term search-result-datum)
	  (hash-table-put! ht search-term (datum->result search-result-datum))))
       datum)
      ht))

  (define (result->datum result)
    (cond ((error-result? result)
	   (list 'error (error-result-message result)))
	  ((exact-result? result)
	   (resource->datum (exact-result-resource result)))
	  ((result-list? result)
	   (map (lambda (result)
		  (cons (lookup-result-relevance result)
			(resource->datum (lookup-result-resource result))))
		(result-list-lookup-results result)))
	  (else
	   (error 'result->datum "unknown result type: ~v" result))))

  (define datum->result
    (match-lambda
      ((list 'error message)
       (make-error-result message))
      ((list (list-rest relevance resource-datum) ...)
       (make-result-list
	"OK"
	(map (lambda (relevance resource-datum)
	       (make-lookup-result relevance (datum->resource resource-datum)))
	     relevance resource-datum)))
      (resource-datum
       (make-exact-result "OK" (datum->resource resource-datum)))))

  (define (resource->datum resource)
    (cond ((artist? resource)
	   (list (artist-id resource)
		 (artist-name resource)
		 (artist-sortname resource)))
	  ((album? resource)
	   (list (album-id resource)
		 (album-title resource)))
	  (else
	   (error 'resource->datum "unknown resource type: ~v" resource))))

  (define datum->resource
    (match-lambda
      ((list id name sortname)
       (make-artist (artist-id->uri id) name sortname))
      ((list id title)
       (make-album (album-id->uri id) title #f))))
)
