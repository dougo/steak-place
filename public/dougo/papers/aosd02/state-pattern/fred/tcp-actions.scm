(define-method active-open ((tcp-connection? t)) & (eq? (state t) 'closed)
  (printf "activeOpen() in ~a~n" t)
  (change-state! t 'established))

(define-method passive-open ((tcp-connection? t)) & (eq? (state t) 'closed)
  (printf "passiveOpen() in ~a~n" t)
  (change-state! t 'listen))

(define-method close ((tcp-connection? t)) & (eq? (state t) 'established)
  (printf "close() in ~a~n" t)
  (change-state! t 'listen))

(define-method transmit ((tcp-connection? t)
			 (tcp-octet-stream? o)) & (eq? (state t) 'established)
  (printf "transmit() in ~a~n" t)
  (process-octet t o))

(define-method send ((tcp-connection? t)) & (eq? (state t) 'listen)
  (printf "send() in ~a~n" t)
  (change-state! t 'established))

