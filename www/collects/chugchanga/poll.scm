(module poll mzscheme
  (define *year* "2003")
  (define *poll-name* (string-append "Chugchanga-L Record Poll " *year*))
  (define *anonymous-name* "An Anonymous Chugchanga Member")

  (require (lib "etc.ss"))		;build-list
  (require (lib "url.ss" "net"))		;url
  (require (lib "servlet-sig.ss" "web-server")) ;request
  (require (lib "xml.ss" "xml"))		;xml->xexpr
  (require (rename (lib "html.ss" "html") read-html-as-xml read-html-as-xml))
  (require (all-except (lib "list.ss" "srfi" "1") delete)) ;split-at, append-map
  (require (lib "table.scm" "myweb"))
  (require (lib "counter.scm" "myweb"))
  (require (lib "html.scm" "myweb"))
;;;	   (lib "names.scm" "myweb")
  (require (lib "login.scm" "myweb"))
  (require (lib "data.scm" "myweb"))
  (require (planet "spgsql.ss" ("schematics" "spgsql.plt" 1)))
  (require (lib "class.ss"))		;send
  (require (lib "db.ss" "db"))		;with-connection-to-db
  (require (lib "sql.ss" "db"))		;select, insert, update, delete, where
  (require (lib "plt-match.ss"))	;match-let

  (provide (all-defined))

  (define (edit-poll-entry request) (make-poll-page request edit-entry-page))
  (define (view-poll-entry request) (make-poll-page request view-entry-page))

  (define (make-poll-page request page-maker)
    (if (poll-open?)
	(login-if-needed
	 request
	 (lambda (uid headers)
	   (with-connection-to-db
	    "chugchanga" "dougo"
	    (lambda ()
	      (let ((uri (request-relative-uri-string request))
		    (entry (process-entry-form uid (request-bindings request))))
		(make-session-page
		 uid *poll-name* uri "/chugchanga/"
		 (page-maker uid uri (or entry (get-entry uid)))
		 headers))))))
	(make-page (string-append *poll-name* " Is Closed")
		   `("The poll is currently closed."))))

  (define (edit-entry-page uid uri entry)
    `((p "Please enter your favorite records released (or rereleased) in "
	 ,*year* ".  "
	 "You may list as many or as few records as you like, "
	 "but only the first twenty will be tallied in the poll.")
      (p "The form below has room for your comments about the list, "
	 "both at the top and bottom and for each individual record.  "
	 "The contents of these fields will be interpreted as HTML.  "
	 "In particular, this means that line breaks "
	 "will be ignored; use <p> or <br> if needed.")
      (p (a ((href "view.scm")) "Preview your entry"))
      (p ,(make-entry-form uri entry))))

  (define (view-entry-page uid uri entry)
    `((p (a ((href "edit.scm")) "Edit your entry"))
      ,@(entry->xexprs uid entry)))

  (define (make-path file)
    (build-path "chugchanga" *year* file))

  (define *poll-open?-path* (build-path *data-path* (make-path "open")))

  (define (poll-open?)
    (file-exists? *poll-open?-path*))

  (define-struct entry (anonymous? preamble records postamble))
  (define-struct record (artist name label comments))

  (define (get-entry uid)
    (match-let (((vector anonymous? preamble postamble)
		 (send (current-connection) query-tuple
		       (select (anonymous? preamble postamble) (entries)
			 (where (= uid ,uid))))))
      (make-entry
       anonymous?
       preamble
       (send (current-connection) map
	     (order-by (select (artist title label comments) (votes)
			 (where (= uid ,uid)))
		       ((number asc)))
	     make-record)
       postamble)))

  (define (set-entry! uid entr)
    (begin-transaction
     (match-let (((struct entry (anonymous? preamble records postamble)) entr))
       (send (current-connection) exec
	     (update entries ((anonymous? ,anonymous?)
			      (preamble ,preamble)
			      (postamble ,postamble))
		     (where (= uid ,uid))))
       (send (current-connection) exec
	     (delete votes (where (= uid ,uid))))
       (do ((number 1 (add1 number)) (records records (cdr records)))
	   ((null? records))
	 (match-let (((struct record (artist title label comments))
		      (car records)))
	   (send (current-connection) exec
		 (insert votes
			 ((uid ,uid)
			  (number ,number)
			  (artist ,artist)
			  (title ,title)
			  (label ,label)
			  (comments ,comments)))))))))

  (define (make-empty-entry)
    (make-entry #f "" (build-list 20 (lambda (i) (make-empty-record))) ""))
  (define (bindings->entry bindings)
    (make-entry (exists-binding? 'anonymous bindings)
		(extract-binding/single 'preamble bindings)
		(bindings->records bindings)
		(extract-binding/single 'postamble bindings)))

  (define (process-entry-form uid bindings)
    (and (exists-binding? 'preamble bindings)
	 (let ((entry (bindings->entry bindings)))
	   (set-entry! uid entry)
	   entry)))

  (define (make-entry-form uri entry)
    `(form ((action ,uri) (method "post"))
	   (p (input ((type "submit") (name "save") (value "Save changes")))
	      (input ((type "reset"))))
	   (p (input ((type "checkbox") (name "anonymous")
		      ,@(if (entry-anonymous? entry)
			    `((checked "checked"))
			    '())))
	      "Withhold your name from the poll")
	   (p ,(textarea-field "" "preamble" 5 80 (entry-preamble entry)
			       `(tabindex "1")))
	   (p (table ;((border "1"))
		     (tr (th "")
			 (th "Artist") (th "Record name") (th "Record label"))
		     ,@(map make-record-field
			    (build-list (length (entry-records entry)) add1)
			    (entry-records entry))
		     (tr (td) (td (input ((type "submit") (name "more")
					  (value "Add more records")))))))
	   (p ,(textarea-field "" "postamble" 5 80 (entry-postamble entry)
			       `(tabindex "1")))
	   (p (input ((type "submit") (name "save") (value "Save changes")))
	      (input ((type "reset"))))
	   ))

  (define (entry->xexprs uid entry)
    `((h1 ,(entry->title uid entry))
      (p ,@(string->xexprs (entry-preamble entry)))
      (ol ,@(append-map record->xexprs (entry-records entry)))
      (p ,@(string->xexprs (entry-postamble entry)))))

  (define (get-fullname uid thunk)
    (let ((names (send (current-connection) query-list
		       (select (name) (users) (where (= uid ,uid))))))
      (if (null? names)
	  (thunk)
	  (car names))))

  (define (entry->title uid entry)
    (string-append
     (if (entry-anonymous? entry)
	 *anonymous-name*
	 (get-fullname uid (lambda () *anonymous-name*)))
     "'s Favorite Records Of " *year*))

  (define (make-empty-record)
    (make-record "" "" "" ""))
  (define (bindings->records bindings)
    (let ((records (apply map make-record
			  (map (lambda (sym) (extract-bindings sym bindings))
			       '(artist name label comments)))))
      (let-values (((submit-type submit-value) (extract-submit bindings)))
	(case submit-type
	  ((add)
	   (let-values (((before after) (split-at records submit-value)))
	     (append before (cons (make-empty-record) after))))
	  ((delete)
	   (let-values (((before after) (split-at records submit-value)))
	     (append before (cdr after))))
	  ((more)
	   (append records (build-list 10 (lambda (i) (make-empty-record)))))
	  (else
	   records)))))

  (define (extract-submit bindings)
    (let loop ((b bindings))
      (if (null? b)
	  (error 'extract-submit "No submit?! ~a" bindings)
	  (let ((key (caar b)))
	    (cond ((regexp-match "^(add|delete)([0-9]+)$" (symbol->string key))
		   => (lambda (matches)
			(values (string->symbol (cadr matches))
				(sub1 (string->number (caddr matches))))))
		  ((memq key '(more save))
		   (values key #f))
		  (else
		   (loop (cdr b))))))))

  (define (make-record-field i record)
    (let ((i (number->string i)))
      `(div (tr (td) (td) (td) (td)
		(td (input ((type "submit")
			    (name ,(string-append "add" i))
			    (value "<< Add a record here")))))
	    (tr (td ((align "right")) ,i ".")
		(td ,(make-artist-field (record-artist record)))
		(td ,(make-name-field (record-name record)))
		(td ,(make-label-field (record-label record)))
		(td (input ((type "submit")
			    (name ,(string-append "delete" i))
			    (value "<< Delete this record")))))
	    (tr (td)
		(td ((colspan "4"))
		    ,(make-comments-field (record-comments record)))))))

  (define (make-artist-field artist)
;;;    `(table (tr (td (input ((type "radio") (name "atype")
;;;			    (value "band") (checked "checked"))))
;;;		(td (label "Band name: "
;;;			   (input ((name "artist") (value ,artist))))))
;;;	    (tr (td (input ((type "radio") (name "atype")
;;;			    (value "individual"))))
;;;		(td (table (tr (td (label ((for "fname")) "First name: "))
;;;			       (td (input ((name "fname") (value "")))))
;;;			   (tr (td (label ((for "lname")) "Last name: "))
;;;			       (td (input ((name "lname") (value ""))))))))
;;;	    (tr (td (input ((type "radio") (name "atype")
;;;			    (value "various"))))
;;;		(td "Various Artists"))))
    `(input ((size "30") (name "artist") (value ,artist)
	     (tabindex "1"))))

  (define (make-name-field name)
;;;    `(div (input ((type "radio") (name "rtype")
;;;		   (value "name") (checked "checked")))
;;;	   (input ((name "name") (value ,name)))
;;;	   (br)
;;;	   (input ((type "radio") (name "rtype")
;;;		   (value "selftitled")))
;;;	   "Self-titled"))
    `(input ((size "40") (name "name") (value ,name)
	     (tabindex "1"))))

  (define (make-label-field label)
    `(input ((name "label") (value ,label) (tabindex "1"))))

  (define (make-comments-field comments)
    `(textarea ((name "comments") (rows "2") (cols "80") (tabindex "1"))
	       ,comments))

  (define (record->xexprs record)
    (let ((artist (record-artist record))
	  (name (record-name record))
	  (label (record-label record))
	  (comments (record-comments record)))
      (let ((xexprs
	     (append
	      (record-info->xexprs artist name label)
	      (if (string-empty? comments)
		  '()
		  `((br) ,@(string->xexprs comments))))))
	(if (null? xexprs)
	    '()
	    `((li ,@xexprs))))))

  (define (record-info->xexprs artist name label)
    (append
     (cond ((string-empty? name)
	    (if (string-empty? artist)
		'()
		`((strong ,artist))))
	   ((string-empty? artist) `((cite ,name)))
	   (else `((strong ,artist) ", " (cite ,name))))
     (if (string-empty? label) '() `(" (" ,label ")"))))

  (define (string->xexprs str)
    (map xml->xexpr (read-html-as-xml (open-input-string str))))

  (define (string-empty? str)
    (string=? str ""))

)
