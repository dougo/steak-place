;; Interface to the MusicBrainz database via its XML Web service.
;; http://wiki.musicbrainz.org/XMLWebService

(module mb mzscheme
  (require (lib "xml.ss" "xml"))
  (require (lib "url.ss" "net"))
  (require (lib "list.ss" "srfi" "1"))	;alist-cons, fold-right, third
  (require (lib "string.ss" "srfi" "13")) ;string-every, string-join,
					  ;string-tokenize

  (require "resource.ss")
  (provide (all-from "resource.ss"))

  (provide (all-defined))

  ;; Use the test server for now, it has better search results.
  (define *base-url* (string->url "http://test.musicbrainz.org/ws/1/"))

  (define (combine-url/query base query)
    ;; TO DO: use copy-struct
    (make-url
     (url-scheme base)
     (url-user base)
     (url-host base)
     (url-port base)
     (url-path-absolute? base)
     (url-path base)
     (append (url-query base) query)
     (url-fragment base)))

  ;; Lookup an attribute by name in an element X-expression, returning
  ;; its value (or #f if the attribute is missing).
  (define (xexpr-attribute-value element name)
    (cond ((assq name (second element)) => cadr)
	  (else #f)))

  ;; Concatenate the string content of an element X-expression.  This
  ;; is needed because, for example, the X-expression for
  ;; "<foo>blah&amp;blah</foo>" is (foo () "blah" "&" "blah").
  (define (xexpr-content-string element)
    (apply string-append (cddr element)))

  ;; Lookup a child element by name in an element X-expression,
  ;; returning its content string value (or #f if the child is
  ;; missing).
  (define (xexpr-content-value element name)
    (cond ((assq name (cddr element)) => xexpr-content-string)
	  (else #f)))

  ;; Recursively eliminate all strings consisting solely of
  ;; whitespace from the content of an element X-expression.
  (define (eliminate-whitespace-content element)
    (cons* (car element) (cadr element)
	   (filter-map
	    (lambda (xexpr)
	      (cond ((and (string? xexpr)
			  (string-every char-whitespace? xexpr))
		     #f)
		    ((pair? xexpr)
		     (eliminate-whitespace-content xexpr))
		    (else
		     xexpr)))
	    (cddr element))))
	       
  ;; Make a URL for accessing a resource given its type (a symbol, one
  ;; of "artist"/"release"/"track"), ID (a string), and query fields.
  (define (make-resource-url type id query)
    (combine-url/query
     (combine-url/relative *base-url* (format "~a/~a" type id))
     query))

  (define (make-collection-url type query)
    (combine-url/query
     (combine-url/relative *base-url* (symbol->string type))
     query))

  ;; Get a pure port containing an XML representation of a resource.
  ;; The ID may be followed by any number of symbols indicating
  ;; additional data to be included in the resource (these vary by
  ;; resource type, e.g. "aliases" for artist or "artist" for
  ;; release).
  (define (get-resource-xml-port type id . inc)
    ;; TO DO: error handling: get impure port, check status-code
    (get-pure-port
     (apply make-resource-xml-url type id inc)))

  (define (make-resource-xml-url type id . inc)
    (make-resource-url
     type id
     (alist-cons 'type "xml"
                 (if (null? inc)
                     null
                     (list (cons 'inc (string-join
                                       (map symbol->string inc))))))))

  (define (resource-xml-port->xexpr port)
    (eliminate-whitespace-content
     (xml->xexpr (document-element (read-xml port)))))

  (define (xexpr->resource resource type)
    (let ((id (xexpr-attribute-value resource 'id)))
      (case type
	((artist)
	 (make-artist id (xexpr-content-value resource 'name)
		      (xexpr-content-value resource 'sort-name)
		      (xexpr-content-value resource 'disambiguation)))
	((release)
	 (let ((types
		(map string->symbol
		     (string-tokenize
		      (xexpr-attribute-value resource 'type)))))
	   (make-release id (xexpr-content-value resource 'title)
			 (car types) (cadr types))))
	)))

  (define (get-resource type id)
    (xexpr->resource
     (third (resource-xml-port->xexpr
	     (get-resource-xml-port type id)))
     type))

)
