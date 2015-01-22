#lang web-server

(provide interface-version start)
(define interface-version 'stateless)

(define (start request)
  (send/suspend/dispatch
   (lambda (k-url)
     (response/xexpr
      `(html (body (a ((href ,(k-url start))) "Hello!")))))))

