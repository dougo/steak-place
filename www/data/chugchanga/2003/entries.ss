(module entries mzscheme
  (require (lib "plt-match.ss"))	;match, match-let
  (require (rename (lib "list.ss") mergesort mergesort))
  (require (lib "list.ss" "srfi" "1"))	;remove, filter-map
  (require (lib "string.ss" "srfi" "13")) ;string-*
  (require "utils.ss")			;read-from-file, index-for-each
  (provide (all-defined))

  (define *voter-names*
    (let* ((alist (read-from-file "names"))
	   (vec (make-vector (add1 (apply max (map car alist))))))
      (for-each (lambda (p) (vector-set! vec (car p) (cdr p))) alist)
      vec))

  (define (voter-name uid)
    (vector-ref *voter-names* uid))

  ;; Guess the sortname for a person's name.
  (define (fullname->sortname name)
    (cond ((string-index-right name char-whitespace?)
	   => (lambda (i)
		(string-append (string-drop name (add1 i)) ", "
			       (string-take name i))))
	  (else name)))

  (define *counter* 0)
  (define (anonymous-name)
    (set! *counter* (add1 *counter*))
    (format "Anonymous Chugchanga-L Member #~a" *counter*))


  (define-struct entry (fullname sortname preamble records postamble))
  (define (datum->entry datum)
    (match-let (((list-rest uid preamble records postamble flags) datum))
      (let-values (((fullname sortname)
		    (if (memq 'anonymous flags)
			(let ((name (anonymous-name)))
			  (values name name))
			(let ((name (voter-name uid)))
			  (values name (fullname->sortname name))))))
	(make-entry fullname sortname
		    preamble
		    (remove record-empty? (map datum->record records))
		    postamble))))

  (define (entry-empty? entry)
    (and (null? (entry-records entry))
	 (string-null? (entry-preamble entry))
	 (string-null? (entry-postamble entry))))

  (define-struct record (artist title label comments))
  (define (datum->record datum)
    (apply make-record (map string-trim-both datum)))

  (define (record-empty? record)
    (and (string-null? (record-artist record))
	 (string-null? (record-title record))
	 (string-null? (record-label record))
	 (string-null? (record-comments record))))

  (define (compare-entries entry1 entry2)
    (string-ci<=? (entry-sortname entry1) (entry-sortname entry2)))


  (define *entries*
    (mergesort (remove entry-empty?
		       (map datum->entry (read-from-file "entries")))
	       compare-entries))

  (define *entry-ids* (make-hash-table))
  (index-for-each (lambda (i entry) (hash-table-put! *entry-ids* entry i))
		  *entries*)
  (define (entry-id entry)
    (hash-table-get *entry-ids* entry))
)
