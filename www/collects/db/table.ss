;; A local model for PostgreSQL tables.

(module table mzscheme
  (require (planet "spgsql.ss" ("schematics" "spgsql.plt" 1)))
  (require (lib "class.ss"))		;send
  (require "select.ss")			;select
  (provide (all-defined))

  ;; Make a local representation of the table with the given name
  ;; using the current database connection.
  (define (get-table-by-name name)
    (caar (select 'pg_class 'oid 'relname name)))

  (define (table-name table)
    ;; FIXME: the type of relname is 'name, which doesn't have a parser.
    ;; So we have to convert it from a bytes manually.
    (string->symbol
     (bytes->string/utf-8
      (caar (select 'pg_class 'relname 'oid (table-oid table))))))

  (define (get-table-by-oid oid)
    oid)

  (define (table-oid table)
    table)

  (define (table-column-names table)
    (map (lambda (bytes)
	   (string->symbol
	    (bytes->string/utf-8
	     bytes)))
	 (select 'pg_attribute 'attname 'attrelid (table-oid table))
	 ;;"and attnum > 0 order by attnum"
	 ))

;;; "select atttypid from pg_attribute where attrelid = ~a and attname in (~a)"

  (define (type-oid type)
    type)

  (define (type-name type)
    (string->symbol
     (bytes->string/utf-8
      (select 'pg_type 'typname 'oid (type-oid type)))))

  (define (add-row! table cols vals)
    (let ((types (map type-name (table-column-types table cols))))
      (send (current-connection) exec
	    (format
	     "insert into ~a (~a) values (~a)"
	     (sql-quote (table-name table))
	     (comma-list (map sql-quote cols))
	     (comma-list (map sql-marshal types vals))))))
)
