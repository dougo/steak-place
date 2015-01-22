(module mb-lookup mzscheme
  (require (lib "mb.ss" "musicbrainz"))
  (require (only (lib "list.ss" "srfi" "1") filter find))
  (require (only (lib "13.ss" "srfi") string-null?))
  (provide (all-defined))

  ;; Lookup an artist and album in the Musicbrainz database.  Three
  ;; values are returned: albums whose titles and artists match,
  ;; albums whose titles match, and artists whose names match.  Each
  ;; value may be #f, a single resource, a list of resources, or a
  ;; list of relevance/resource pairs.

  ;; String String -> (listof (union #f (listof Album)
  ;;                                    (listof (pairof Number Album)))
  ;;                          (union #f Album (listof Album)
  ;;                                    (listof (pairof Number Album)))
  ;;                          (union #f Artist (listof (pairof Number Artist))))
  (define (mb-lookup artist album)
    (let* ((artist-result
	    (if (string-null? artist)
		#f
		(find-artist-by-name artist)))
	   (album-result
	    (if (string-null? album)
		#f
		(if (artist? artist-result)
		    (find-album-by-artist+title artist-result album)
		    (find-album-by-title album))))
	   (albums
	    (and (pair? album-result) (pair? artist-result)
		 (filter
		  (lambda (album-or-pair)
		    (find
		     (lambda (artist-or-pair)
		       (resource=? (if (pair? artist-or-pair)
				       (cdr artist-or-pair)
				       artist-or-pair)
				   (album-artist
				    (if (pair? album-or-pair)
					(cdr album-or-pair)
					album-or-pair))))
		     artist-result))
		  album-result))))
      (values albums album-result artist-result)))
)
