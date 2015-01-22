;; Cache MusicBrainz info in a local database.  All procedures in this
;; module assume that the current-connection parameter is set.

(module mb-persist mzscheme
  (require (lib "mb.ss" "musicbrainz"))
  (require (planet "spgsql.ss" ("schematics" "spgsql.plt" 1)))
  (require (lib "db.ss" "db"))		;current-connection
  (require (lib "sql.ss" "db"))		;select, insert
  (require (lib "class.ss"))		;send
  (provide (all-defined))

  ;; String -> Artist
  (define (find-artist-by-id/cached id)
    (let ((artists
	   (send (current-connection) map
		 (select (name sortname) (mb-artists) (where (= id ,id)))
		 (lambda (name sortname)
		   (make-artist (artist-id->uri id) name sortname)))))
      (if (null? artists)
	  (cache-artist (find-artist-by-id id))
	  (car artists))))

  (define (cache-artist artist)
    (send (current-connection) exec
	  (insert mb-artists
		  ((id ,(artist-id artist))
		   (name ,(artist-name artist))
		   (sortname ,(artist-sortname artist)))))
    artist)

  (define (cache-album album)
    (send (current-connection) exec
	  (insert mb-albums
		  ((id ,(album-id album))
		   (artist ,(album-artist album))
		   (title ,(album-title album)))))
    album)

)
