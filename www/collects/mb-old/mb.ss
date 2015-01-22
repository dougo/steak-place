;; Interface to the old RDF web service for the MusicBrainz database.
;; http://www.musicbrainz.org/MM/
;; http://www.ldodds.com/projects/musicbrainz/schema/

(module mb mzscheme
  (require (lib "rdf.ss" "rdf"))	;document->graph, *rdf-ns*
  (require "web.ss")			;get-query, post-query, xexpr->document
  (require "vocab.ss")			;*mm-ns*, *mq-ns*, *dc-ns*
  (require "resource.ss")		;Artist, Album, etc
  (provide (all-from "resource.ss"))
  (provide (all-defined))

  ;; Search the MusicBrainz database for an artist by name.
  ;; String -> (union Artist (listof (pairof Number Artist)))
  (define (find-artist-by-name name)
    (let-values (((graph result-node)
		  ;; Use FileInfoLookup rather than FindArtist because
		  ;; it provides relevance numbers.
		  (post-rdf-query `(mq:FileInfoLookup (mq:artistName ,name)))))
      (cond ((graph-object graph result-node *mq:artist*)
	     => (lambda (node) (node->artist graph node)))
	    (else (parse-lookup-result-list graph result-node)))))

  ;; Retrieve an artist info structure from the MusicBrainz database
  ;; given the artist's ID.
  ;; String -> Artist
  (define (find-artist-by-id id)
    (let-values (((graph result-node)
		  (get-rdf-query (string-append "artist/" id))))
      (let* ((artist-list (graph-object graph result-node *mm:artistList*))
	     (node (car (graph-container-contents graph artist-list))))
	(node->artist graph node))))

  ;; Search the MusicBrainz database for an album by its title.
  ;; String -> (listof Album)
  (define (find-album-by-title title)
    (let-values (((graph result-node)
		  (post-rdf-query
		   `(mq:FindAlbum
		     ;; Depth of 4 is needed to get the artist name.
		     (mq:depth "4")
		     (mq:albumName ,title)))))
      (cond ((graph-object graph result-node *mm:albumList*)
	     => (lambda (album-list)
		  (map (lambda (album-node) (node->album graph album-node))
		       (graph-container-contents graph album-list))))
	    (else null))))

  ;; Search the MusicBrainz database for an album given an artist
  ;; and title.
  ;; Artist String -> (union Album (listof (pairof Number Album)))
  (define (find-album-by-artist+title artist title)
    (let-values (((graph result-node)
		  (post-rdf-query
		   `(mq:FileInfoLookup (mm:artistid ,(artist-id artist))
				       (mq:albumName ,title)))))
      (cond ((graph-object graph result-node *mq:album*)
	     => (lambda (node) (node->album graph node artist)))
	    (else (parse-lookup-result-list graph result-node)))))


  ;; Graph Node -> (listof (pairof Number (union Artist Album)))
  (define (parse-lookup-result-list graph node)
    (let ((lookup-results (graph-object graph node *mq:lookupResultList*)))
      (map (lambda (lookup-result) (lookup-result->pair graph lookup-result))
	   (graph-container-contents graph lookup-results))))

  ;; Graph Node -> (pairof Number (union Artist Album))
  (define (lookup-result->pair graph node)
    (cons
     (string->number
      (literal-lexical-form
       (graph-object graph node *mq:relevance*)))
     (let ((type (graph-node-type graph node)))
       (cond ((node=? type *mq:ArtistResult*)
	      (node->artist graph (graph-object graph node *mq:artist*)))
	     ((node=? type *mq:AlbumResult*)
	      (node->album graph (graph-object graph node *mq:album*)))))))

  ;; Query the MusicBrainz server using HTTP GET.
  ;; String -> Graph Node
  (define (get-rdf-query path)
    (graph->graph+result
     (document->graph
      (get-query path))))

  ;; Query the MusicBrainz server using HTTP POST with a RDF
  ;; X-expression element.
  ;; Xexpr -> Graph Node
  (define (post-rdf-query query)
    (graph->graph+result
     (document->graph
      (post-query
       (xexpr->document
	(make-query-rdf query))))))

  ;; Wrap a query in a RDF element.
  ;; Xexpr -> Xexpr
  (define (make-query-rdf query)
    `(rdf:RDF
      ((xmlns:rdf ,*rdf-ns*)
       (xmlns:dc ,*dc-ns*)
       (xmlns:mq ,*mq-ns*)
       (xmlns:mm ,*mm-ns*))
      ,query))

  ;; Get the result node from a RDF graph returned by the MusicBrainz server.
  ;; Raise an error if the result has an error.
  ;; Graph -> Graph Node
  (define (graph->graph+result graph)
    (let ((result-node (graph-typed-node graph *mq:Result*)))
      (cond ((graph-object graph result-node *mq:error*)
	     => (lambda (message-node)
		  (error 'graph->result (literal-lexical-form message-node))))
	    ((graph-object graph result-node *mq:status*)
	     (values graph result-node))
	    (else
	     (error 'graph->result "can't parse result")))))
)
