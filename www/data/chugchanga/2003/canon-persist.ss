(module canon-persist mzscheme
  (require (lib "list.ss" "srfi" "1"))	;cons*, append-map
  (require (rename (lib "list.ss") mergesort mergesort))
  (require (lib "pconvert.ss"))		;print-convert
  (require (lib "plt-match.ss"))	;match-lambda
  (require (lib "pretty.ss"))		;pretty-print
  (require (lib "shared.ss"))		;shared (for output of print-convert)
  (require "canon.ss")			;struct cartist, crecord
  (require "mb.ss")			;struct result, artist, album
  (require "mb-persist.ss")		;search->datum, datum->search
  (provide (all-defined))

  (define (save-canon)
    (with-output-to-file "canon"
      (lambda () (pretty-print (crecords->datum (crecords))))
      'text 'truncate/replace))

  (define (load-canon)
    (for-each
     (lambda (crecord)
       (hash-table-put! *crecords* (crecord-key crecord) crecord)
       (let ((cartist (crecord-artist crecord)))
	 (hash-table-put! *cartists* (cartist-key cartist) cartist))
       (hash-table-for-each (crecord-search crecord)
	 (lambda (k v)
	   (hash-table-put! *crecord-search* k crecord))))
     (datum->crecords (call-with-input-file "canon" read 'text))))

  (define (new-crecords->datum crecords)
    (print-convert crecords))

  (define (crecords->datum crecords)
    (let ((ht (make-hash-table)))
      (for-each (lambda (crecord)
		  (let* ((cartist (crecord-artist crecord))
			 (l (hash-table-get ht cartist (lambda () null))))
		    (hash-table-put! ht cartist (cons crecord l))))
		crecords)
      (let ((cartists (hash-table-map ht (lambda (k v) k))))
	(map (lambda (cartist)
	       (let ((crecords (hash-table-get ht cartist)))
		 (cons (cartist->datum cartist)
		       (map crecord->datum 
			    (mergesort crecords crecord-title<?)))))
	     (mergesort cartists cartist<?)))))

  (define (datum->crecords datum)
    (append-map
     (match-lambda 
       ((list-rest cartist-datum crecords-datum)
	(let ((cartist (datum->cartist cartist-datum)))
	  (map
	   (lambda (crecord-datum)
	     (datum->crecord crecord-datum cartist))
	   crecords-datum))))
     datum))

  (define (cartist->datum cartist)
    (cons* (cartist-name cartist)
	   (cartist-sortname cartist)
	   (cartist-mb cartist)
	   (search->datum (cartist-search cartist))))

  (define datum->cartist
    (match-lambda
      ((list-rest name sortname mb search-datum)
       (make-cartist name sortname mb (datum->search search-datum)))))

  (define (crecord->datum crecord)
    (cons* (crecord-title crecord)
	   (crecord-mb crecord)
	   (crecord-label crecord)
	   (crecord-labels crecord)
	   (search->datum (crecord-search crecord))))

  (define (datum->crecord datum cartist)
    (match-let (((list-rest title mb label labels search-datum) datum))
      (make-crecord cartist title mb (datum->search search-datum)
		    label labels)))

  (load-canon)
)
