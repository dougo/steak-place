#lang scheme

;;; mb-mirror.ss -- MusicBrainz mirror servlet
;;;
;;; This servlet forwards HTTP GET and POST requests to the
;;; MusicBrainz server.  The URL is supplied as the "url" parameter of
;;; the request.

(provide interface-version timeout start)

(require web-server/servlet)
(require net/url)
(require net/head)

(define interface-version 'v1)
(define timeout +inf.0)

;; (require (lib "unitsig.ss")		;unit/sig, import
;; 	 (lib "response.ss" "web-server") ;make-response/full
;;          (lib "servlet-sig.ss" "web-server") ;servlet^
;; 	 (lib "url.ss" "net")		;HTTP get/post
;; 	 (lib "plt-match.ss")		;match-let
;; 	 (lib "head.ss" "net")		;extract/remove-field
;; 	 )


(define (start initial-request)
  (with-errors-to-browser
   send/back
   (lambda ()

     (define method
       (string->symbol
	(string-downcase
	 (bytes->string/latin-1 (request-method initial-request)))))

     (define url
       (string->url
	(extract-binding/single 'url (request-bindings initial-request))))

     (define url->port-proc
       (case method
	 ((get) get-impure-port)
	 ((post) (lambda (url) (post-impure-port url (request-bindings/raw
						      initial-request))))
	 (else (error "Unsupported method: ~a" method))))

     (call/input-url url url->port-proc port->X-proc)
)))

;; Put the contents of the impure port into a response.
(define (port->X-proc port)
  (match-let (((list portion version code message header)
	       (regexp-match "^(.*?) (.*?) (.*?)\n(.*)$" (purify-port port))))
    (let* ((mime (extract-field "Content-Type" header))
	   (extras
	    (map (lambda (field)
		   (make-header (string->bytes/latin-1 (car field))
				(string->bytes/latin-1 (cdr field))))
		 (extract-all-fields (remove-field "Content-Type" header)))))
      (make-response/full
       (string->number code) (string->bytes/latin-1 message)
       (current-seconds) (string->bytes/latin-1 mime) extras
       (let loop ()
	 (let ((line (read-bytes-line port)))
	   (if (eof-object? line)
	       null
	       (cons line (loop)))))))))

