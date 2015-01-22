(module session mzscheme
  (require (lib "list.ss")		;filter
	   "password.scm"		;random-string
	   "cookie.scm" "table.scm")
  (provide make-session make-session-cookie
	   session->uid extract-sessions extract-active-sessions
	   extract-uid extend-session expire-session)

  (define *table* (make-table "sessions"))

  (define *session-expiration-time* (* 60 24 60 60)) ;60 days

  (define (make-session uid)
    (let ((session (random-string 16))
	  (expire (+ (current-seconds) *session-expiration-time*)))
      (table-put! *table* session (cons uid expire))
      session))

  (define (make-session-cookie uid)
    (session->cookie (make-session uid) *session-expiration-time*))

  (define (session->uid session)
    (car (table-get *table* session (lambda () '(#f)))))

  (define (extract-active-sessions request)
    (filter session->uid (extract-sessions request)))

  (define (extract-sessions request)
    (let ((cookies (extract-cookies request)))
      (apply append (map (lambda (cookie) (get-cookie "Session" cookie))
			 cookies))))

  (define (extract-uid request)
    (let ((uids (map session->uid (extract-active-sessions request))))
      (and (not (null? uids)) (car uids))))

  (define (extend-session session time)
    (table-update! *table* session (lambda (uid-expire)
				     (cons (car uid-expire)
					   (+ (cdr uid-expire) time)))))

  (define (expire-session session)
    (table-remove! *table* session)
    (session->cookie session 0))

  (define (delete-expired-sessions)
    (let ((time (current-seconds)))
      (table-remove-if! *table* (lambda (session uid-expire)
				  (> time (cdr uid-expire))))))

  (define (session->cookie session age)
    (cookie:add-path
     (cookie:add-max-age (set-cookie "Session" session) age)
     "/"))
)
