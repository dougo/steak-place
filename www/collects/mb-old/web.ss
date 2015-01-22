;; Interface to the MusicBrainz web service.

(module web mzscheme
  (require (lib "xml.ss" "xml"))	;xexpr->xml, Document, etc
  (require (lib "url.ss" "net"))	;post-pure-port, string->url
  (require (lib "port.ss"))		;peeking-input-port, copy-port
  (require "try.ss")			;try
  (provide (all-defined))

  ;; Wrap an X-expression in a Document struct.
  ;; Xexpr -> Document
  (define (xexpr->document xexpr)
    (make-document
     (make-prolog (list (make-pi #f #f "xml" "version='1.0'")) #f)
     (xexpr->xml xexpr)
     null))

  ;; The request URLs.
  (define *get-url*
    (string->url "http://musicbrainz.org/mm-2.1/"))
  (define *post-url*
    (string->url "http://mm.musicbrainz.org/cgi-bin/mq_2_1.pl"))

  ;; Parse an XML document from a pure port.  If it's not valid XML,
  ;; raise exn:xml with the pure port's contents as the exception
  ;; message.
  ;; Input-Port -> Document
  (define (pure-port->xml in)
    (let ((in2 (peeking-input-port in)))
      (try (read-xml in2)
	((exn:xml? exn)
	 (let ((out (open-output-string)))
	   (copy-port in out)
	   (raise (make-exn:xml
		   (string->immutable-string (get-output-string out))
		   (current-continuation-marks)
		   (exn:fail:read-srclocs exn)))))
	(finally
	 (close-input-port in2)
	 (close-input-port in)))))


  ;; Query the MusicBrainz server using HTTP GET.
  ;; String -> Document
  (define (get-query path)
    (pure-port->xml (get-pure-port (combine-url/relative *get-url* path))))

  ;; Query the MusicBrainz server using HTTP POST.
  ;; Document -> Document
  (define (post-query query)
    (let ((out (open-output-bytes)))
      (write-xml query out)
      (pure-port->xml (post-pure-port *post-url* (get-output-bytes out)))))
)
