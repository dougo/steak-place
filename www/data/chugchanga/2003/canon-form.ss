(module canon-form mzscheme
  (require (rename (lib "list.ss") mergesort mergesort))
  (require (all-except (lib "html.ss" "html") span)) ;html-empty-tags
  (require (lib "xml.ss" "xml"))	;xexpr->xml, write-xml/content
  (require "canon.ss")
  (require "canon-persist.ss")
  (provide (all-defined))

  (define (write-form-page)
    (parameterize ((empty-tag-shorthand html-empty-tags))
      (with-output-to-file "form.html"
	(lambda () (write-xml/content (xexpr->xml (make-form-page))))
	'text 'truncate/replace)))

  (define (make-form-page)
    `(html (head) (body ,(canon-form-xexpr))))

  (define (canon-form-xexpr)
    (let ((ht (make-hash-table)))
      (for-each (lambda (crecord)
		  (let* ((cartist (crecord-artist crecord))
			 (l (hash-table-get ht cartist (lambda () null))))
		    (hash-table-put! ht cartist (cons crecord l))))
		(crecords))
      (let ((cartists (hash-table-map ht (lambda (k v) k))))
	`(form (ul ,@(map (lambda (cartist)
			   (let ((crecords (hash-table-get ht cartist)))
			     (cartist-form-xexpr
			      cartist
			      (mergesort crecords crecord-title<?))))
			  (mergesort cartists cartist<?)))))))

  (define (cartist-form-xexpr cartist crecords)
    `(li (input ((type "text") (value ,(cartist-name cartist))))
	 (input ((type "text") (value ,(cartist-sortname cartist))))
	 (input ((type "text") (value ,(or (cartist-mb cartist) ""))))))
       
)
