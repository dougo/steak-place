;; Integrate cookies library with web-server structs.

(module cookie mzscheme
  (require (lib "servlet-sig.ss" "web-server")) ;request-headers,
					       ;extract-bindings
  (require (lib "cookie.ss" "net"))	;print-cookie
  (provide (all-from (lib "cookie.ss" "net")))
  (provide (all-defined))

  ;; A list of cookies in a request struct.
  ;; extract-cookies : request -> (listof str)
  (define (extract-cookies request)
    (map bytes->string/utf-8
	 (extract-bindings 'cookie (request-headers request))))

  ;; Wrap a cookie in an environment suitable for the extras field of
  ;; a response struct.
  ;; cookie-headers : cookie -> (listof (cons sym str))
  (define (cookie-headers cookie)
    `((Set-Cookie . ,(print-cookie cookie))))
)
