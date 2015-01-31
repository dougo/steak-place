(define-values (struct:tcp-connection
		make-tcp-connection
		tcp-connection?
		tcp-connection-ref
		tcp-connection-set!)
  (make-struct-type 'tcp-connection #f 0 1 'closed))
(define tcp-connection-state
  (make-struct-field-accessor tcp-connection-ref 0))
(define set-tcp-connection-state!
  (make-struct-field-mutator tcp-connection-set! 0))

(define-method make ((eq? x struct:tcp-connection))
  (make-tcp-connection 'closed))
(define-method state ((tcp-connection? t))
  (tcp-connection-state t))
(define-method change-state! ((tcp-connection? t) (symbol? s))
  (printf "Changing state from ~a to ~a in ~a~n"
	  (state t) s t)
  (set-tcp-connection-state! t s))

(define-struct tcp-octet-stream ())
(define-method process-octet ((tcp-connection? t) (tcp-octet-stream? o))
  (printf "Processing ~a in ~a~n" o t))

(define-msg active-open)
(define-msg passive-open)
(define-msg close)
(define-msg transmit)
(define-msg send)
(define-msg acknowledge)
(define-msg synchronize)
(define state-dependent-msgs
  (list active-open passive-open close transmit send acknowledge synchronize))

;; Default methods for state-dependent actions
(define-branch
  (lambda (dp) (and (tcp-connection? (car (dp-args dp)))
		    (memq (dp-msg dp) state-dependent-msgs)))
  (lambda (dp) (void)))
