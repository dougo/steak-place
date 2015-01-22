;; This file was run from /home/web, but was moved here for archival purposes.

(require-for-syntax "web-collects.scm")
(require (lib "poll.scm" "chugchanga")
	 (lib "html.scm" "myweb")
	 (lib "names.scm" "myweb")
	 (lib "xml.ss" "xml")
	 (lib "list.ss" "srfi" "1")
	 (lib "string.ss" "srfi" "13")
	 (lib "match.ss")
	 (lib "etc.ss")
	 (lib "pretty.ss")
	 (rename (lib "list.ss") mergesort mergesort))

(define *input-dir* (string-append "www/data/chugchanga/" *year*))
(define *output-dir* (string-append "www/htdocs/chugchanga/poll/" *year*))

(define *foot* `((a ((href ".")) ,*poll-name*)))

(define-struct voter (uid last-name name entry))

(define (compare-voters voter1 voter2)
  (string-ci<=? (voter-last-name voter1) (voter-last-name voter2)))

(define *voters* #f)

(define (read-voters)
  (set! *counter* 0)
  (set! *voters*
	(mergesort
	 (map (match-lambda
		((uid . entry)
		 (let ((name (entry-name uid entry)))
		   (make-voter uid (last-name name) name entry))))
	      (get-entries))
	 compare-voters)))

(define (entry-name uid entry)
  (if (entry-anonymous? entry)
      (anonymous-name)
      (string-trim-both (get-fullname uid anonymous-name))))


(define *counter* 0)
(define (anonymous-name)
  (set! *counter* (add1 *counter*))
  (format "Anonymous Chugchanga Member #~a" *counter*))

(define (last-name name)
  (do ((i (sub1 (string-length name)) (sub1 i)))
      ((or (< i 0) (char-whitespace? (string-ref name i)))
       (substring name (add1 i) (string-length name)))))


(define *canon* #f)
(define *record-map* #f)

(define (build-canon)
  (set! *canon* (make-hash-table 'equal))
  (set! *record-map* (make-hash-table))
  (read-canon)
  (for-each canonicalize-voter *voters*)
  (read-mapping)
  (build-ranking))

(define (read-canon)
  (for-each
   (lambda (rec) (apply canonicalize-record rec))
   (with-input-from-file (string-append *input-dir* "/canonical") read)))

(define (canonicalize-voter voter)
  (let-values (((votes mentions)
		(safe-split-at (entry-records (voter-entry voter)) 20)))
    (for-each (lambda (r) (canonicalize-vote voter r #t)) votes)
    (for-each (lambda (r) (canonicalize-vote voter r #f)) mentions)))

(define (safe-split-at x i)
  (if (<= i (length x))
      (split-at x i)
      (values x '())))

(define (canonicalize-vote vr r vote?)
  (let ((v (make-vote vr r)))
    (match-let ((($ record artist name label comments) r))
      (unless (and (string-null? artist) (string-null? name))
	((if vote? add-canonical-record-vote! add-canonical-record-mention!)
	 (canonicalize-record artist name label) v)))))

(define (canonicalize-record artist name label)
  (let* ((artist (canonicalize-artist artist))
	 (name (canonicalize-name name artist))
	 (label (canonicalize-label label))
	 (key (make-record-key artist name))
	 (anchor (make-record-anchor artist name)))
    (define (new)
      (let ((cr (make-canonical-record key anchor artist name label '() '() 0)))
	(hash-table-put! *canon* key cr)
	cr))
    (hash-table-get *canon* key new)))

(define (canonicalize-artist artist)
  (let ((artist (string-trim-both artist)))
    (cond ((and (string-prefix-ci? "various" artist)
		(not (string-prefix-ci? "various artists" artist)))
	   (string-replace artist "Various Artists" 0 7))
	  ((string-prefix-ci? "v/a" artist)
	   (string-replace artist "Various Artists" 0 3))
	  (else
	   artist))))

(define (canonicalize-name name artist)
  (let ((name (string-trim-both name)))
    (cond ((or (string-prefix-ci? "self titled" name)
	       (string-prefix-ci? "self-titled" name))
	   (string-replace name artist 0 11))
	  ((string-prefix-ci? "s/t" name)
	   (string-replace name artist 0 3))
	  (else
	   name))))

(define (canonicalize-label label)
  (string-trim-both label))

(define (make-record-key artist name)
  (string-append (make-key artist) "\0" (make-key name)))

(define (make-record-anchor artist name)
  (string-append (make-anchor artist) "__" (make-anchor name)))

(define (make-anchor s)
  (remove-spaces! (make-key s)))

(define (make-key s)
  (let ((s (string-trim-both (remove-punctuation (string-downcase s)))))
    (cond ((or (string-prefix? "the " s)
	       (string-prefix? "les " s)
	       (string-prefix? "los " s))
	   (string-drop s 4))
	  ((or (string-prefix? "el " s)
	       (string-prefix? "la " s)
	       (string-prefix? "le " s))
	   (string-drop s 3))
	  (else s))))

(define (remove-punctuation s)
  (let loop ((i 0) (r ""))
    (if (= i (string-length s))
	r
	(case (string-ref s i)
	  ((#\' #\` #\" #\( #\) #\[ #\] #\< #\> #\! #\? #\. #\, #\205)
	   (loop (add1 i) r))
	  ((#\& #\+)
	   (loop (add1 i) (string-append r "and")))
	  (else
	   (loop (add1 i) (string-append r (string (string-ref s i)))))))))

(define (remove-spaces! s)
  (do ((i 0 (add1 i)))
      ((= i (string-length s)) s)
    (unless (char-alphanumeric? (string-ref s i))
      (string-set! s i #\_))))

(define (char-alphanumeric? c) (or (char-alphabetic? c) (char-numeric? c)))


(define (read-mapping)
  (let ((mapping (with-input-from-file
		     (string-append *input-dir* "/mapping")
		   read)))
    (for-each (lambda (keys) (add-aliases (car keys) (cdr keys))) mapping)
    (for-each (lambda (keys) (for-each
			      (lambda (k) (hash-table-remove! *canon* k))
			      (cdr keys)))
	      mapping)))

(define (add-aliases key keys)
  (let ((cr (hash-table-get *canon* key)))
    (for-each (lambda (k) (add-alias cr k)) keys)))

(define (add-alias cr key)
  (let ((alias-cr (hash-table-get *canon* key)))
    (for-each (lambda (v) (add-canonical-record-vote! cr v))
	      (canonical-record-votes alias-cr))
    (for-each (lambda (v) (add-canonical-record-mention! cr v))
	      (canonical-record-mentions alias-cr))))

(define (build-ranking)
  (let loop ((crs (canonical-records-by-votes)) (i 1)
	     (last-r 1) (last-v 0) (last-m 0))
    (unless (null? crs)
      (let* ((cr (car crs))
	     (v (length (canonical-record-votes cr)))
	     (m (length (canonical-record-mentions cr)))
	     (r (if (and (= v last-v) (= m last-m)) last-r i)))
	(set-canonical-record-rank! cr r)
	(loop (cdr crs) (add1 i) r v m)))))


(define (canonical-records)
  (hash-table-map *canon* (lambda (k v) v)))

(define (canonical-records-by-artist)
  (mergesort (canonical-records) compare-canonical-records))

(define (canonical-records-by-votes)
  (mergesort (canonical-records) compare-canonical-records-by-votes))


(define-struct vote
  (voter record))
(define-struct canonical-record
  (key anchor artist name label votes mentions rank))

(define (add-canonical-record-vote! cr v)
  (update-record-map (vote-record v) cr)
  (set-canonical-record-votes! cr (cons v (canonical-record-votes cr))))

(define (add-canonical-record-mention! cr m)
  (update-record-map (vote-record m) cr)
  (set-canonical-record-mentions! cr (cons m (canonical-record-mentions cr))))

(define (update-record-map r cr)
  (hash-table-put! *record-map* r cr))

(define (record->canonical-record r)
  (hash-table-get *record-map* r))


(define (compare-votes v1 v2)
  (compare-voters (vote-voter v1) (vote-voter v2)))

(define (compare-canonical-records cr1 cr2)
  (string<=? (canonical-record-key cr1) (canonical-record-key cr2)))

(define (compare-canonical-records-by-votes cr1 cr2)
  (let ((l1 (length (canonical-record-votes cr1)))
	(l2 (length (canonical-record-votes cr2))))
    (if (= l1 l2)
	(let ((l1 (length (canonical-record-mentions cr1)))
	      (l2 (length (canonical-record-mentions cr2))))
	  (if (= l1 l2)
	      (compare-canonical-records cr1 cr2)
	      (> l1 l2)))
	(> l1 l2))))

(define (canonical-record->xexprs cr page)
  (canonical-record->xexprs/comments* cr page #f))

(define (canonical-record->xexprs/comments cr page)
  (canonical-record->xexprs/comments* cr page #t))

(define (canonical-record->xexprs/comments* cr page comments?)
  (match-let ((($ canonical-record
		  key anchor artist name label votes mentions rank) cr))
    (let ((votes (mergesort votes compare-votes))
	  (mentions (mergesort mentions compare-votes))
	  (v+m (mergesort (append votes mentions) compare-votes)))
      (define (voter-links votes)
	(comma-list (lambda (vote) (vote->xexpr vote anchor)) votes))
      (define (voter-comments votes)
	(append-map (lambda (vote) (vote->xexprs/comments vote anchor)) votes))
      `((tr (td ,(number->string rank) ".")
	    (td (a ((name ,anchor)))
		(a ((href ,(string-append page "#" anchor)))
		   ,@(record-info->xexprs artist name label)))
	    (td ,@(if (and (null? votes) (not (null? mentions)))
		      '()
		      `(,(number->string (length votes))))
		,@(if (null? mentions)
		      '()
		      `("(" ,(number->string (length mentions)) ")"))
		":")
	    (td (font ((size "-2"))
		      ,@(voter-links votes)
		      ,@(if (null? mentions)
			    '()
			    `(" (" ,@(voter-links mentions) ")")))))
	,@(if comments?
	      `((tr (td)
		    (td ((colspan "3"))
			(font ((size "-1"))
			      ,@(voter-comments v+m)))))
	      '())))))

(define (comma-list f l)
  (cond ((null? l) l)
	((null? (cdr l)) (list (f (car l))))
	(else (cons* (f (car l)) ", " (comma-list f (cdr l))))))

(define (vote->xexpr v anchor)
  (match-let* ((($ vote vr r) v)
	       (($ voter uid last-name full-name entry) vr))
    `(a ((href ,(string-append (uid->filename uid) "#" anchor))) ,full-name)))

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


(define (write-voters)
  (xexpr-to-file
   "voters.html"
   (make-page-xexpr*
    (string-append *poll-name* " Voters")
    `(,(string-append "Chugchanga-L Members Who Voted in the " *year* " Poll"))
    (append-map
     (match-lambda
       (($ voter uid last-name name entry)
	`((a ((href ,(uid->filename uid))) ,name) (br))))
     *voters*)
    *foot*))

  (let loop ((voters *voters*) (prev #f))
    (unless (null? voters)
      (xexpr-to-file
       (uid->filename (voter-uid (car voters)))
       (voter->xexpr (car voters) prev (and (not (null? (cdr voters)))
					    (cadr voters))))
      (loop (cdr voters) (car voters)))))

(define (voter->xexpr v prev next)
  (let ((navbar `(,(nav-voter prev "prev") " | "
		  (a ((href "voters.html")) "list") " | "
		  ,(nav-voter next "next"))))
    (match-let* ((($ voter uid last-name name e) v)
		 (($ entry anonymous? preamble records postamble) e))
      `(html
	(head (title ,(string-append *poll-name* " - " name)))
	(body ,@navbar (hr)
	      (h1 ,(string-append name "'s Favorite Records Of " *year*))
	      (p ,@(string->xexprs preamble))
	      (ol ,@(append-map record->linked-xexprs records))
	      (p ,@(string->xexprs postamble))
	      (hr) ,@navbar)))))

(define (nav-voter voter nav)
  (if voter `(a ((href ,(uid->filename (voter-uid voter)))) ,nav) nav))

(define (record->linked-xexprs r)
  (match-let ((($ record artist name label comments) r))
    (let* ((rx (record-info->xexprs artist name label))
	   (xexprs
	    (append
	     (if (null? rx)
		 '()
		 (let ((anchor (canonical-record-anchor
				(record->canonical-record r))))
		   `((a ((name ,anchor)))
		     (a ((href ,(string-append "byartistwithcomments.html#"
					       anchor)))
			,@rx))))
	     (if (string-null? comments)
		 '()
		 `((br) ,@(string->xexprs comments))))))
      (if (null? xexprs)
	  '()
	  `((li ,@xexprs))))))


(define (xexpr-to-file file xexpr)
  (with-output-to-file (build-path *output-dir* file)
    (lambda ()
      (display (xexpr->string xexpr))
      (newline))
    'replace))

(define (uid->filename uid)
  (format "~a.html" uid))


(define (write-artists)
  (let* ((byartist (canonical-records-by-artist))
	 (byvotes (canonical-records-by-votes))
	 (table-header '(tr (th "Rank")
			    (th "Record")
			    (th ((colspan "2")) "Votes (Mentions)")))
	 (head `((a ((href "voters.html")) "Chugchanga-L Members")
		 "' Favorite Records of " ,*year*)))
    (xexpr-to-file
     "byartist.html"
     (make-page-xexpr*
      (string-append *poll-name* " By Artist") head
      `((a ((href "byvotes.html")) "Sort by votes") " | "
	(a ((href "byartistwithcomments.html")) "Show comments")
	(table ,table-header
	       ,@(append-map (lambda (cr) (canonical-record->xexprs
					   cr
					   "byartistwithcomments.html"))
			     byartist)))
      *foot*))
    (xexpr-to-file
     "byartistwithcomments.html"
     (make-page-xexpr*
      (string-append *poll-name* " By Artist With Comments") head
      `((a ((href "byvoteswithcomments.html")) "Sort by votes") " | "
	(a ((href "byartist.html")) "Hide comments")
	(table ,table-header
	       ,@(append-map (lambda (cr) (canonical-record->xexprs/comments
					   cr
					   "byvoteswithcomments.html"))
			     byartist)))
      *foot*))
    (xexpr-to-file
     "byvotes.html"
     (make-page-xexpr*
      (string-append *poll-name* " By Votes") head
      `((a ((href "byartist.html")) "Sort by artist") " | "
	(a ((href "byvoteswithcomments.html")) "Show comments")
	(table ,table-header
	       ,@(append-map (lambda (cr) (canonical-record->xexprs
					   cr
					   "byvoteswithcomments.html"))
			     byvotes)))
      *foot*))
    (xexpr-to-file
     "byvoteswithcomments.html"
     (make-page-xexpr*
      (string-append *poll-name* " By Votes With Comments") head
      `((a ((href "byartistwithcomments.html")) "Sort by artist") " | "
	(a ((href "byvotes.html")) "Hide comments")
	(table ,table-header
	       ,@(append-map (lambda (cr) (canonical-record->xexprs/comments
					   cr
					   "byartistwithcomments.html"))
			     byvotes)))
      *foot*))
    (with-output-to-file "keys"
      (lambda () (pretty-print (map canonical-record-key byartist)))
      'replace)
    (with-output-to-file "bad"
      (lambda ()
	(display "(") (newline)
	(for-each (lambda (cr)
		    (let ((artist (canonical-record-artist cr))
			  (name (canonical-record-name cr))
			  (label (canonical-record-label cr)))
		      (when (string-null? label)
			(write (list artist name label))
			(newline))))
		  byvotes)
	(display ")") (newline))
      'replace)))


(define (poll)
  (read-voters)
  (build-canon)
  (write-voters)
  (write-artists))
