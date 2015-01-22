;; MusicBrainz RDF vocabulary.
;; http://www.musicbrainz.org/MM/
;; http://www.ldodds.com/projects/musicbrainz/schema/

(module vocab mzscheme
  (require (lib "vocab.ss" "rdf"))	;ns+name->uri
  (provide (all-defined))

  (define *mm-ns*
    "http://musicbrainz.org/mm/mm-2.1#")
  (define *mq-ns*
    "http://musicbrainz.org/mm/mq-1.1#")
  (define *dc-ns*
    "http://purl.org/dc/elements/1.1/")

  ;; Symbol -> String
  (define (mm-uri name)
    (ns+name->uri *mm-ns* name))

  ;; Symbol -> String
  (define (mq-uri name)
    (ns+name->uri *mq-ns* name))

  ;; Symbol -> String
  (define (dc-uri name)
    (ns+name->uri *dc-ns* name))

  (define *mm:artistList*
    (mm-uri 'artistList))
  (define *mm:albumList*
    (mm-uri 'albumList))
  (define *mm:trackList*
    (mm-uri 'trackList))

  (define *mm:Artist*
    (mm-uri 'Artist))
  (define *mm:sortName*
    (mm-uri 'sortName))

  (define *mm:Album*
    (mm-uri 'Album))

  (define *mm:Track*
    (mm-uri 'Track))
  
  (define *mq:Result*
    (mq-uri 'Result))
  (define *mq:error*
    (mq-uri 'error))
  (define *mq:status*
    (mq-uri 'status))
  (define *mq:artist*
    (mq-uri 'artist))
  (define *mq:album*
    (mq-uri 'album))
  (define *mq:lookupResultList*
    (mq-uri 'lookupResultList))

  (define *mq:ArtistResult*
    (mq-uri 'ArtistResult))
  (define *mq:AlbumResult*
    (mq-uri 'AlbumResult))
  (define *mq:relevance*
    (mq-uri 'relevance))

  (define *dc:title*
    (dc-uri 'title))
  (define *dc:creator*
    (dc-uri 'creator))
)
