#lang web-server

(provide interface-version start)
(define interface-version 'stateless)

(require myweb/html       ;*footer*
	 net/url
	 srfi/1
	 srfi/13)

;; TO DO: use request-bindings/raw
(require web-server/http/bindings)


;; FIXME: This depends on the cwd when the web server was started...
;; probably not a good idea.
(define *htdocs* "..")

(define (string-after-last s c)
  (let ((last (string-index-right s c)))
    (and last (string-drop s (add1 last)))))

(define (file-extension file) (string-after-last (path->string file) #\.))

(define (path-extension path)
  (let-values (((base name must-be-dir?) (split-path path)))
    (and (not must-be-dir?)
	 (path? name)
	 (file-extension name))))

(define (jpeg-files path)
  (filter (lambda (path) (equal? "jpg" (path-extension path)))
	  (directory-list path)))

(define (jpegs game)
  (let ((url-path (string-append "/games/zendo/" game "/")))
    (append-map (lambda (file)
		  ;; FIXME: convert file to url-path
		  `((img ((src ,(string-append url-path
					       (path->string file)))))))
		(jpeg-files (url->path (string->url
					(string-append *htdocs* url-path)))))))

(define (start initial-request)
  (let* ((env (request-bindings initial-request))
	 (game (extract-binding/single 'game env)))
    (response/xexpr
     `(html
       (head (title ,game))
       (body
        (h1 (hr) "Zendo Summary: " ,game)
        ,@(jpegs game)
        (hr)
        (address (a ((href "/dougo/")) "Doug Orleans") " "
                 (a ((href "mailto:dougorleans@gmail.com"))
                    "<dougorleans@gmail.com>"))
        ))
     )))
