#lang scheme

(require web-server/servlet)
(require net/url)
(provide interface-version timeout start)
(define interface-version 'v1)
(define timeout +inf.0)
(define (start req)
  (with-errors-to-browser send/back (lambda () (handle-request req))))

(define (handle-request req)
  (bytes->string/utf-8 (or (request-post-data/raw req) #"")))
