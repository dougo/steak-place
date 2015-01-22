;; Structures for MusicBrainz database entries.

(module resource mzscheme
  (require (lib "list.ss" "srfi" "1"))	;last
  (require (lib "url.ss" "net"))	;url-path, path/param-path
  (require (lib "rdf.ss" "rdf"))	;Graph, Node, *dc:title*, *dc:creator*
  (require "vocab.ss")			;*mm:Artist*, *mm:sortName*, *mm:Album*
  (provide (all-defined))

  ;; A resource is either an artist, an album, or a track.
  ;; Resource = Artist | Album
  (define-struct resource (uri title))

  ;; Resource Resource -> Boolean
  (define (resource=? r1 r2)
    (string=? (resource-uri r1) (resource-uri r2)))

  ;; Resource -> String
  (define (resource-id resource) (uri-id (resource-uri resource)))

  ;; Resource -> String
  (define (resource-url resource)
    (let ((id (resource-id resource)))
      (cond ((artist? resource) (artist-id->url id))
	    ((album? resource) (album-id->url id))
	    (else (resource-uri resource)))))

  ;; Artist = String String String
  (define-struct (artist resource) (sortname))

  ;; Artist -> String
  (define (artist-id artist) (resource-id artist))

  ;; Artist -> String
  (define (artist-name artist) (resource-title artist))

  ;; Graph Node -> Artist
  (define (node->artist graph node)
    (unless (node=? (graph-node-type graph node) *mm:Artist*)
      (error 'node->artist "node is not an artist"))
    (make-artist
     node
     (literal-lexical-form (graph-object graph node *dc:title*))
     (literal-lexical-form (graph-object graph node *mm:sortName*))))

  ;; Album = String String Artist
  (define-struct (album resource) (artist))

  ;; Album -> String
  (define (album-id album) (resource-id album))

  ;; Album -> String
  (define (album-title album) (resource-title album))

  ;; Graph Node -> Album
  (define (node->album graph node . artist)
    (unless (node=? (graph-node-type graph node) *mm:Album*)
      (error 'node->album "node is not an album"))
    (let ((artist-node (graph-object graph node *dc:creator*)))
      (make-album
       node
       (literal-lexical-form (graph-object graph node *dc:title*))
       (if artist-node (node->artist graph artist-node) (car artist)))))

  ;; Extract an artist/album/track ID from a URI.
  ;; String -> String
  (define (uri-id uri)
    ;; The ID is the final path segment name.
    (path/param-path (last (url-path (string->url uri)))))

  ;; Convert an artist ID to its URI.
  ;; String -> String
  (define (artist-id->uri id)
    (string-append "http://musicbrainz.org/mm-2.1/artist/" id))

  ;; Convert an album ID to its URI.
  ;; String -> String
  (define (album-id->uri id)
    (string-append "http://musicbrainz.org/mm-2.1/album/" id))

  ;; Convert an artist ID to a URL locating the (human-readable) artist page.
  ;; String -> String
  (define (artist-id->url id)
    (string-append "http://musicbrainz.org/artist/" id))

  ;; Convert an album ID to a URL locating the (human-readable) album page.
  ;; String -> String
  (define (album-id->url id)
    (string-append "http://musicbrainz.org/album/" id))
)
