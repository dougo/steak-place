;; Persistent thread-synchronized tables.  Keys are always compared
;; with equal?.

(module table mzscheme
  (require (lib "list.ss")		;remove, remove*
	   (lib "etc.ss")		;opt-lambda
	   "data.scm")
  (provide make-table table-get
	   table-put! table-put*!
	   table-remove! table-remove*! table-remove-if!
	   table-update! table-update*!
	   table-map table-for-each)

  (define (make-table file)
    (make-datum file '()))

  (define table-get
    (opt-lambda (table key (failure-thunk
			    (lambda ()
			      (raise-mismatch-error
			       'table-get
			       "no value found for key: " key))))
      (assoc-else key (datum-value table) failure-thunk)))

  (define (table-put! table key value)
    (update-datum-value! table (lambda (alist)
				 (replace-entry (cons key value) alist))))

  (define (table-put*! table entries)
    (update-datum-value! table (lambda (alist)
				 (replace-entries entries alist))))

  (define (table-remove! table key)
    (update-datum-value! table (lambda (alist)
				 (remove-from-alist key alist))))

  (define (table-remove*! table keys)
    (update-datum-value! table (lambda (alist)
				 (remove*-from-alist keys alist))))

  (define (table-remove-if! table remove?)
    (update-datum-value! table (lambda (alist)
				 (remove-from-alist-if remove? alist))))

  ;; proc is a function from the old value to the new value.
  (define table-update!
    (opt-lambda (table key proc (failure-thunk
				 (lambda ()
				   (raise-mismatch-error
				    'table-update!
				    "no value found for key: " key))))
      (define (update alist)
	(let ((value (proc (assoc-else key alist failure-thunk))))
	  (replace-entry (cons key value) alist)))
      (update-datum-value! table update)))
				 
  ;; proc is a function from (key, old value) to the new value.
  ;; failure-proc is a function from key to the new value.
  (define table-update*!
    (opt-lambda (table keys proc (failure-proc
				  (lambda (key)
				    (raise-mismatch-error
				     'table-update*!
				     "no value found for key: " key))))
      (define (update alist)
	(replace-entries
	 (map 
	  (lambda (key)
	    (define (fail) (failure-proc key))
	    (cons key (proc key (assoc-else key alist fail))))
	  keys)
	 alist))
      (update-datum-value! table update)))


  (define (table-map table proc)
    (map (lambda (entry) (proc (car entry) (cdr entry)))
	 (datum-value table)))

  (define (table-for-each table proc)
    (for-each (lambda (entry) (proc (car entry) (cdr entry)))
	      (datum-value table)))

  (define (assoc-else key alist failure-thunk)
    (cond ((assoc key alist) => cdr)
	  (else (failure-thunk))))

  (define (key-for? key entry)
    (equal? key (car entry)))

  ;; Replace the matching-key entry in alist, preserving order,
  ;; prepending if not present.
  (define (replace-entry entry alist)
    (let loop ((head '()) (tail alist))
      (cond ((null? tail)
	     (cons entry alist))
	    ((key-for? (car entry) (car tail))
	     (append (reverse head) (cons entry (cdr tail))))
	    (else
	     (loop (cons (car tail) head) (cdr tail))))))

  (define (replace-entries entries alist)
    (if (null? entries)
	alist
	(replace-entries (cdr entries) (replace-entry (car entries) alist))))

  (define (remove-from-alist key alist)
    (remove key alist key-for?))
  (define (remove*-from-alist keys alist)
    (remove* keys alist key-for?))
  (define (remove-from-alist-if remove? alist)
    (remove remove? alist (lambda (remove? entry)
			    (remove? (car entry) (cdr entry)))))
)
