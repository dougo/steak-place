;; Canonicalized record info.

(module canon mzscheme
  (require (lib "list.ss" "srfi" "1"))	;lset-adjoin
  (require (lib "string.ss" "srfi" "13")) ;string-*
  (require (rename (lib "list.ss") mergesort mergesort))
  (require "mb.ss")			;find-artist/album-by-*
  (require "entries.ss")		;struct record
  (require "memo.ss")			;define/memo
  (provide (all-defined))

  (define (canonicalize record)
    (let* ((name (record-artist record))
	   (title (canonicalize-title name (record-title record)))
	   (crecord (canonicalize-record name title)))
      (add-crecord-label! crecord (record-label record))
      crecord))

  (define ktrue (lambda args #t))
  (define kfalse (lambda args #f))

  ;; The canonical information for an artist.
  ;; name and sortname are strings.
  ;; mb is a MusicBrainz ID, or #f.
  ;; search is a map from artist search terms to MusicBrainz search results.
  (define-struct cartist (name sortname mb search))

  (define (cartist-compare c1 c2 k< k= k>)
    (string-compare-ci (cartist-sortname c1) (cartist-sortname c2) k< k= k>))
  (define (cartist<? c1 c2)
    (cartist-compare c1 c2 ktrue kfalse kfalse))

  ;; (list String String (opt String)) -> Cartist
  (define *cartists* (make-hash-table 'equal))

  ;; String String (opt String) -> Cartist
  (define/memo* (find-cartist name sortname mb) *cartists*
    (make-cartist name sortname mb (make-hash-table 'equal)))

  ;; String String (opt String) String Result -> Cartist
  (define (add-cartist name sortname mb search-term search-result)
    (let ((cartist (find-cartist name sortname mb)))
      (hash-table-put! (cartist-search cartist) search-term search-result)
      cartist))

  (define (remove-cartist cartist)
    (hash-table-remove! *cartists* cartist))

  (define (cartist-key cartist)
    (list (cartist-name cartist)
	  (cartist-sortname cartist)
	  (cartist-mb cartist)))

  ;; Artist String Result -> Cartist
  (define (add-artist artist search-term search-result)
    (if artist
	(add-cartist (artist-name artist)
		     (artist-sortname artist)
		     (artist-id artist)
		     search-term search-result)
	(let ((name (canonicalize-name search-term)))
	  (add-cartist name (canonicalize-sortname name) #f
		       search-term search-result))))


  ;; The canonical information for a record.
  ;; artist is a cartist structure.
  ;; title is a string.
  ;; mb is a MusicBrainz ID, or #f.
  ;; search is a map from (artist-name title) search terms to MusicBrainz
  ;; search results.
  ;; label is a string and labels is a list of strings.
  (define-struct crecord (artist title mb search label labels))

  ;; Does c1 precede c2 in a lexicographic order?  Sort by artist,
  ;; then title.
  (define (crecord-compare c1 c2 k< k= k>)
    (cartist-compare (crecord-artist c1) (crecord-artist c2)
		     k<
		     (lambda args (crecord-compare-titles c1 c2 k< k= k>))
		     k>))
  (define (crecord<? c1 c2)
    (crecord-compare c1 c2 ktrue kfalse kfalse))

  (define (crecord-compare-titles c1 c2 k< k= k>)
    (string-compare-ci (crecord-title c1) (crecord-title c2) k< k= k>))
  (define (crecord-title<? c1 c2)
    (crecord-compare-titles c1 c2 ktrue kfalse kfalse))

  ;; (list Cartist String (opt String)) -> Crecord
  (define *crecords* (make-hash-table 'equal))

  ;; The list of canonical records.
  (define (crecords) (hash-table-map *crecords* (lambda (k v) v)))

  ;; The list of canonical records, sorted lexicographically.
  (define (crecords-by-artist) (mergesort (crecords) crecord<?))

  ;; Cartist String (opt String) -> Crecord
  (define/memo* (find-crecord cartist title mb) *crecords*
    (printf "~v - ~v~%" (cartist-name cartist) title)
    (make-crecord cartist title mb (make-hash-table 'equal) "" null))

  ;; Cartist String (opt String) String String Result -> Crecord
  (define (add-crecord cartist title mb
		       artist-search-term title-search-term search-result)
    (let ((crecord (find-crecord cartist title mb)))
      (hash-table-put! (crecord-search crecord)
		       (list artist-search-term title-search-term)
		       search-result)
      crecord))

  (define (remove-crecord crecord)
    (hash-table-remove! *crecords* (crecord-key crecord)))

  (define (crecord-key crecord)
    (list (crecord-artist crecord)
	  (crecord-title crecord)
	  (crecord-mb crecord)))

  ;; Album Artist String Result String Result -> Crecord
  (define (add-album album artist
		     name artist-result
		     title album-result)
    (if album
	(add-crecord (add-artist (album-artist album) name artist-result)
		     (album-title album)
		     (album-id album)
		     name title album-result)
	(add-crecord (add-artist artist name artist-result)
		     title
		     #f
		     name title album-result)))

  ;; (list String String) -> Crecord
  (define *crecord-search* (make-hash-table 'equal))

  ;; Find or make a crecord struct given an artist name and album title.
  ;; String String -> Crecord
  (define/memo* (canonicalize-record name title) *crecord-search*
    (let* ((artist-result (find-artist-by-name name))
	   (artist (result-resource artist-result))
	   (album-result (if (exact-result? artist-result)
			     (find-album-by-artist+title artist title)
			     (find-album-by-title title)))
	   (album (result-resource album-result))
	   (artist (if album (album-artist album) artist)))
      (add-album album artist name artist-result title album-result)))

  (define (add-crecord-label! crecord label)
    (unless (string-null? label)
      (let ((labels (crecord-labels crecord)))
	(set-crecord-labels! crecord (lset-adjoin string=? labels label))
	;; Choose the longest label name.
	(when (> (string-length label)
		 (string-length (crecord-label crecord)))
	  (set-crecord-label! crecord label)))))

  ;; Canonicalize an artist's name.
  ;; String -> String
  (define (canonicalize-name artist)
    (cond ((and (string-prefix-ci? "various" artist)
		(not (string-prefix-ci? "various artists" artist)))
	   (string-replace artist "Various Artists" 0 7))
	  ((string-prefix-ci? "v/a" artist)
	   (string-replace artist "Various Artists" 0 3))
	  (else
	   artist)))

  ;; String -> String
  (define (canonicalize-sortname s)
    (cond ((or (string-prefix-ci? "the " s)
	       (string-prefix-ci? "les " s)
	       (string-prefix-ci? "los " s))
	   (string-drop s 4))
	  ((or (string-prefix-ci? "el " s)
	       (string-prefix-ci? "la " s)
	       (string-prefix-ci? "le " s))
	   (string-drop s 3))
	  (else s)))

  ;; String String -> String
  (define (canonicalize-title artist name)
    (cond ((string-null? name)
	   artist)
	  ((or (string-prefix-ci? "self titled" name)
	       (string-prefix-ci? "self-titled" name))
	   (string-replace name artist 0 11))
	  ((string-prefix-ci? "s/t" name)
	   (string-replace name artist 0 3))
	  (else
	   name)))
)
