;; PostgreSQL database access stuff.

(module db mzscheme
  (require (lib "spgsql.ss" "spgsql"))	;connect, current-connection
  (require (lib "class.ss"))		;send
  (provide (all-defined))

  ;; Open a connection to a database, invoke thunk, then disconnect.
  ;; The current-connection parameter will hold the connection during
  ;; thunk's dynamic extent.
  ;; FIXME: allow host, port, password
  ;; FIXME: use strings instead of bytes
  ;; with-connection-to-db : bytes bytes (void -> any) -> void
  (define (with-connection-to-db db user thunk)
    (parameterize ((current-connection (connect "localhost" 5432 db user #f)))
      (send (current-connection) use-type-conversions #t)
      (begin0
	(thunk)
	(send (current-connection) disconnect))))


  ;; In all procedures below, the current-connection parameter is
  ;; assumed to be set.

  ;; A SQL name (identifier).
  (define (sql-name? v)
    (symbol? v))
  (define (sql-name->string v)
    (symbol->string v))

  ;; A SQL type is referred to by name.
  (define (sql-type? v)
    (sql-name? v))
  (define (bytes->sql-type b)
    (string->symbol (bytes->string/utf-8 b)))

  ;; Quote a SQL name using double-quotes.
  ;; sql-name -> string
  (define (sql-quote name)
    ;; Escape double-quotes by repeating them.
    ;; FIXME: do we have to worry about newlines, nulls, etc?
    (string-append
     "\"" (regexp-replace "\"" (sql-name->string name) "\"\"") "\""))

  ;; A table reference is either the name of a table or a derived
  ;; table expression (string).
  (define (table-ref->string table-ref)
    (if (sql-name? table-ref)
	(sql-quote table-ref)
	table-ref))

  ;; string+ -> string
  (define (comma-list l)
    (if (null? (cdr l))
	(car l)
	(format "~a, ~a" (car l) (comma-list (cdr l)))))

  ;; table-ref table-ref string -> table-ref
  (define (join-on table1 table2 expr)
    (format "~a join ~a on ~a"
	    (table-ref->string table1)
	    (table-ref->string table2)
	    expr))

  ;; The types of columns in a table.
  ;; sql-name sql-name+ -> sql-type+
  (define (table-column-types table cols)
    (send (current-connection) map
	  (format
	   "select typname from ~a where relname = ~a and attname in (~a)"
	   (join-on (join-on 'pg_class 'pg_attribute
			     "pg_class.oid = pg_attribute.attrelid")
		    'pg_type "pg_attribute.atttypid = pg_type.oid")
	   (sql-marshal 'text table)
	   (comma-list (map (lambda (name) (sql-marshal 'text name)) cols)))
	  bytes->sql-type))

  ;; sql-name sql-name+ any+ ->
  (define (add-row! table cols vals)
    (let ((types (table-column-types table cols)))
      (send (current-connection) exec
	    (format
	     "insert into ~a (~a) values (~a)"
	     (sql-quote table)
	     (comma-list (map sql-quote cols))
	     (comma-list (map sql-marshal types vals))))))
)
