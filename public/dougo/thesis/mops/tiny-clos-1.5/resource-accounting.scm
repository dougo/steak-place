(define-class <resource-accounting-class> (<class>) ())

(define-method (allocate-instance (class <resource-accounting-class>))
  (format #t "Allocating an instance of ~a.~%" (class-name class))
  (call-next-method))

(define-class <counted-object> (<object>)
  ()
  'metaclass <resource-accounting-class>)

(define-class <counted-vector> (<vector>)
  ()
  'metaclass <resource-accounting-class>)

