;; PostgreSQL database access stuff.

(module db mzscheme
  (require (planet "spgsql.ss" ("schematics" "spgsql.plt" 1)))
  (require (lib "class.ss"))		;send
  (provide (all-defined))

  (define current-connection (make-parameter #f))

  ;; Open a connection to a database, invoke thunk, then disconnect.
  ;; The current-connection parameter will hold the connection during
  ;; thunk's dynamic extent.
  ;; FIXME: allow host, port, password
  ;; with-connection-to-db : string string (void -> any) -> void
  (define (with-connection-to-db db user thunk)
    (parameterize ((current-connection (connect "localhost" #f db user)))
      (send (current-connection) use-type-conversions #t)
      (begin0
	(thunk)
	(send (current-connection) disconnect))))

  ;; Evaluate a sequence of expressions as a single transaction with
  ;; the current database connection.
  (define-syntax begin-transaction
    (syntax-rules ()
      ((_ cmd ... expr)
       (begin
	 (send (current-connection) exec "begin;")
	 cmd ...
	 (begin0
	   expr
	   (send (current-connection) exec "commit;"))))))
)
