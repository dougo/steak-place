(load "tcp-connection.scm")
(load "tcp-actions.scm")

(define (main)
  (let ((passive (make-tcp-connection))
	(active (make-tcp-connection)))
    (passive-open passive)
    (active-open active)
    (transmit active (make-tcp-octet-stream))
    (close active)
    (transmit passive (make-tcp-octet-stream)) ; does nothing
    (send passive)
    (close passive)))
