;; Miscellaneous utility procedures.

(module util mzscheme
  (provide (all-defined))

  ;; Quote a SQL name using double-quotes.
  ;; sym -> str
  (define (sql-quote name)
    ;; Escape double-quotes by repeating them.
    ;; FIXME: do we have to worry about newlines, nulls, etc?
    (string-append
     "\"" (regexp-replace "\"" (symbol->string name) "\"\"") "\""))

  ;; any+ -> str
  (define (comma-list l)
    (if (null? (cdr l))
	(car l)
	(format "~a, ~a" (car l) (comma-list (cdr l)))))

  ;; Guess the SQL type of a value.
  ;; sql-val -> sym
  (define (sql-type v)
    (cond ((number? v) 'integer)
	  ((boolean? v) 'boolean)
	  ((date? v) 'timestamptz)
	  (else 'text)))
)
