(module output mzscheme
  (require (lib "date.ss"))		;date->string
  (require (rename (lib "list.ss") mergesort mergesort))
  (require (all-except (lib "html.ss" "html") span)) ;read-html-as-xml
  (require (lib "xml.ss" "xml"))	;xml->xexpr, write-xml/content
  (require (lib "list.ss" "srfi" "1"))	;append-map
  (require (lib "string.ss" "srfi" "13")) ;string-empty?
  (require "mb.ss")			;artist/album-id->uri
  (require "canon.ss")
  (require "entries.ss")		;*entries*
  (require "votes.ss")
  (require "memo.ss")			;define/memo
  (require "utils.ss")			;singleton-list?
  (provide (all-defined))

  (define *year* "2003")
  (define *poll-name* (string-append "Chugchanga-L Record Poll " *year*))
  (define *web-root* "/home/web/www/")
  (define *html-path*
;;; Final version:
;;;    (build-path *web-root* "htdocs" "poll" *year*))
    (build-path *web-root* "htdocs" "chugchanga" "poll" *year*))


  (define (write-results)
    (set! *anchor* 0)
    (write-voters)
    (for-each write-entry *entries*)
    (write-votes))

  (define *anchor* 0)
  (define/memo (canon-anchor canon)
    (begin0
      (number->string *anchor*)
      (set! *anchor* (add1 *anchor*))))

  ;; Write the index of voter names.
  (define (write-voters)
    (write-html-xexpr-to-file
     "voters.html"
     (make-page-xexpr
      (string-append *poll-name* " Voters")
      (list (format "Chugchanga-L Members Who Voted in the ~a Poll" *year*))
      (html-lines
       (map (lambda (entry)
	      `(a ((href ,(entry-path (entry-id entry))))
		  ,(entry-fullname entry)))
	    *entries*)))))
      
  ;; Write an entry page.
  (define (write-entry entry)
    (let ((i (entry-id entry)))
      (write-html-xexpr-to-file
       (entry-path i)
       (let ((navbar `(,(entry-link (sub1 i) "prev") " | "
		       (a ((href "voters.html")) "list") " | "
		       ,(entry-link (add1 i) "next")))
	     (name (entry-fullname entry))
	     (preamble (entry-preamble entry))
	     (records (entry-records entry))
	     (postamble (entry-postamble entry)))
	 `(html
	   (head (title ,(format "~a - ~a" *poll-name* name)))
	   (body ,@navbar (hr)
		 (h1 ,(format "~a's Favorite Records of ~a" name *year*))
		 (p ,@(string->xexprs preamble))
		 (ol ,@(append-map record->linked-xexprs records))
		 (p ,@(string->xexprs postamble))
		 (hr) ,@navbar))))))

  (define (record->linked-xexprs record)
    (let* ((artist (record-artist record))
	   (title (record-title record))
	   (label (record-label record))
	   (comments (record-comments record))
	   (rx (record-info->xexprs artist title label))
	   (xexprs
	    (append
	     (if (null? rx)
		 '()
		 (let ((anchor (canon-anchor (canonicalize record))))
		   `((a ((name ,anchor)))
		     (a ((href ,(string-append "byartistwithcomments.html#"
					       anchor)))
			,@rx))))
	     (if (string-null? comments)
		 '()
		 `((br) ,@(string->xexprs comments))))))
      (if (null? xexprs)
	  '()
	  `((li ,@xexprs)))))

  ;; Write the tables of votes sorted by artist and number of votes,
  ;; with and without comments.
  (define (write-votes)
    (let* ((byartist (crecords-by-artist))
	   (byvotes (crecords-by-votes))
	   (table-header '(tr (th "Rank")
			      (th "Record")
			      (th ((colspan "2")) "Votes (Mentions)")))
	   (head `((a ((href "voters.html")) "Chugchanga-L Members")
		   "' Favorite Records of " ,*year*)))

      (write-html-xexpr-to-file
       "byartist.html"
       (make-page-xexpr
	(string-append *poll-name* " By Artist") head
	`((a ((href "byvotes.html")) "Sort by votes") " | "
	  (a ((href "byartistwithcomments.html")) "Show comments")
	  (table ,table-header ,@(append-map crecord->table-rows byartist)))))

      (write-html-xexpr-to-file
       "byartistwithcomments.html"
       (make-page-xexpr
	(string-append *poll-name* " By Artist With Comments") head
	`((a ((href "byvoteswithcomments.html")) "Sort by votes") " | "
	  (a ((href "byartist.html")) "Hide comments")
	  (table ,table-header
		 ,@(append-map crecord->table-rows/comments byartist)))))

      (write-html-xexpr-to-file
       "byvotes.html"
       (make-page-xexpr
	(string-append *poll-name* " By Votes") head
	`((a ((href "byartist.html")) "Sort by artist") " | "
	  (a ((href "byvoteswithcomments.html")) "Show comments")
	  (table ,table-header ,@(append-map crecord->table-rows byvotes)))))

      (write-html-xexpr-to-file
       "byvoteswithcomments.html"
       (make-page-xexpr
	(string-append *poll-name* " By Votes With Comments") head
	`((a ((href "byartistwithcomments.html")) "Sort by artist") " | "
	  (a ((href "byvotes.html")) "Hide comments")
	  (table ,table-header
		 ,@(append-map crecord->table-rows/comments byvotes)))))

      (write-html-xexpr-to-file
       "index.html"
       (make-page-xexpr
	*poll-name* head
	`("The short list:"
	  (ul ,@(crecords->short-list byvotes))
	  "The long list:"
	  (ul (li (a ((href "byvotes.html")) "by number of votes"))
	      (li (a ((href "voters.html")) "by voter"))
	      (li (a ((href "byartist.html")) "by artist")))
	  "Some statistics:"
	  (ul (li "Number of voters: "
		  (strong ,(number->string (length *entries*))))
	      (li "Number of records receiving at least one vote: "
		  (strong ,(number->string
			    (count (lambda (crecord)
				     (not (null? (crecord-votes crecord))))
				   (crecords)))))
	      (li "Number of records receiving exactly one vote: "
		  (strong ,(number->string
			    (count (lambda (crecord)
				     (singleton-list? (crecord-votes crecord)))
				   (crecords)))))
	      (li "Total number of records mentioned: "
		  (strong ,(number->string (length (crecords))))))
	  (a ((href "..")) "Other music polls")
	  (hr) (address (a ((href "/dougo/")) "Doug Orleans")
			(a ((href "mailto:dougo@place.org"))
			   "<dougo@place.org>"))
	  ,(make-comment "hhmts start")
	  "Last modified: " ,(current-time-string)
	  ,(make-comment "hhmts end")
	  )))
      ))


  (define (crecord->table-rows crecord)
    (crecord->table-rows/comments* crecord #f))
  (define (crecord->table-rows/comments crecord)
    (crecord->table-rows/comments* crecord #t))
  (define (crecord->table-rows/comments* crecord comments?)
    (let ((anchor (canon-anchor crecord))
	  (votes (mergesort (crecord-votes crecord) compare-votes))
	  (mentions (mergesort (crecord-mentions crecord) compare-votes))
	  (v+m (mergesort (append (crecord-votes crecord) 
				  (crecord-mentions crecord))
			  compare-votes)))
      (define (voter-links votes)
	(comma-list (lambda (vote) (vote->xexpr vote anchor)) votes))
      (define (voter-comments votes)
	(append-map (lambda (vote) (vote->xexprs/comments vote anchor)) votes))
      `((tr (td ,(number->string (crecord-rank crecord)) ".")
	    (td (a ((name ,anchor)))
		,@(crecord->xexprs crecord))
	    (td ,(if (and (null? votes) (not (null? mentions)))
		     ""
		     (number->string (length votes)))
		,(if (null? mentions)
		     ""
		     (format "(~a)" (length mentions)))
		":")
	    (td (font ((size "-2"))
		      ,@(voter-links votes)
		      ,@(if (null? mentions)
			    '()
			    `(" (" ,@(voter-links mentions) ")")))))
	,(if comments?
	     `(tr (td)
		  (td ((colspan "3"))
		      (font ((size "-1"))
			    ,@(voter-comments v+m))))
	     ""))))

  (define (comma-list f l)
    (cond ((null? l) l)
	  ((null? (cdr l)) (list (f (car l))))
	  (else (cons* (f (car l)) ", " (comma-list f (cdr l))))))

  (define (vote->xexpr v anchor)
    (let ((entry (vote-entry v)))
      `(a ((href ,(string-append (entry-path (entry-id entry)) "#" anchor)))
	  ,(entry-fullname entry))))

  (define (vote->xexprs/comments v anchor)
    (let ((comments (record-comments (vote-record v))))
      (if (string-null? comments)
	  '()
	  `(,(vote->xexpr v anchor) ": "
	    ,@(string->xexprs (trim comments)) (br)))))

  (define (trim s)
    (trim-left (trim-right s)))

  (define (trim-left s)
    (let ((s (string-trim s)))
      (if (string-prefix-ci? "<br>" s)
	  (trim-left (substring s 4 (string-length s)))
	  s)))

  (define (trim-right s)
    (let ((s (string-trim-right s)))
      (if (string-suffix-ci? "<br>" s)
	  (trim-right (substring s 0 (- (string-length s) 4)))
	  s)))

  (define (crecords->short-list byvotes)
    (append-map crecord->short-list-line
		(take-while (lambda (crecord) (<= (crecord-rank crecord) 20))
			    byvotes)))

  (define (crecord->short-list-line crecord)
    (let ((v (length (crecord-votes crecord)))
	  (m (length (crecord-mentions crecord))))
      `(,(number->string (crecord-rank crecord)) ". "
	,@(crecord->xexprs crecord)
	" - " ,(plural v "vote")
	,(if (zero? m)
	     ""
	     (format " (~a)" (plural m "mention")))
	(br))))

  (define (plural n s)
    (format "~a ~a~a" n s (if (= n 1) "" "s")))

  ;; FIXME: use emacs time format
  (define (current-time-string)
    (parameterize ((date-display-format 'rfc2822))
      (date->string (seconds->date (current-seconds)) #t)))

  ;; An X-expression for text hyperlinked to the ith entry (if it exists).
  (define (entry-link i text)
    (if (and (> i 0) (< i (length *entries*)))
	`(a ((href ,(entry-path i))) ,text)
	text))

  ;; The filename for the ith entry.
  (define (entry-path i)
    (format "~a.html" i))

  (define (crecord->xexprs crecord)
    (let* ((artist-xexprs (cartist->xexprs (crecord-artist crecord)))
	   (title (crecord-title crecord))
	   (title-xexprs
	    (if (string-null? title) null `((cite ,title))))
	   (id (crecord-mb crecord))
	   (title-xexprs
	    (if id
		`((a ((href ,(album-id->uri id))) ,@title-xexprs))
		title-xexprs))
	   (label (crecord-label crecord))
	   (label-xexprs
	    (if (string-null? label) null `(" (" ,label ")"))))
      (append
       artist-xexprs
       (if (or (null? artist-xexprs) (null? title-xexprs)) null `(" "))
       title-xexprs
       label-xexprs)))

  (define (cartist->xexprs cartist)
    (let* ((name (cartist-name cartist))
	   (name-xexprs
	    (if (string-null? name) null `((strong ,name))))
	   (id (cartist-mb cartist)))
      (if (and id (not (null? name-xexprs)))
	  `((a ((href ,(artist-id->uri id))) ,@name-xexprs))
	  name-xexprs)))

  (define (record-info->xexprs artist name label)
    (append
     (if (string-null? artist) null `((strong ,artist)))
     (if (or (string-null? artist) (string-null? name)) null `(" "))
     (if (string-null? name) null `((cite ,name)))
     (if (string-null? label) null `(" (" ,label ")"))))


  (define (write-html-xexpr-to-file path xexpr)
    (parameterize ((empty-tag-shorthand html-empty-tags))
      (with-output-to-file (build-path *html-path* path)
	(lambda () (write-xml/content (xexpr->xml xexpr)))
	'text 'truncate/replace))
    (printf "Wrote to ~v.~%" path))

  (define (make-page-xexpr title head body)
    `(html
      (head (title ,title))
      (body (h1 (hr) ,@head)
	    ,@body
	    (hr)
	    (address (a ((href ".")) ,*poll-name*)))))

  (define (string->xexprs str)
    (map xml->xexpr (read-html-as-xml (open-input-string str))))

  ;; Add a HTML line break after each xexpr.
  (define (html-lines xexprs)
    (append-map (lambda (xexpr) (list xexpr '(br))) xexprs))
)
