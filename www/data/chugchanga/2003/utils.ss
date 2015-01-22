(module utils mzscheme
  (require (lib "pretty.ss"))
  (provide (all-defined))

  ;; Read a datum from the text file at the given path.
  (define (read-from-file path)
    (read (open-input-file path 'text)))

  ;; Pretty-print x as a datum to the text file at the given path,
  ;; truncating it or replacing it as needed.
  (define (write-to-file path x)
    (pretty-print x (open-output-file path 'text 'truncate/replace)))

  ;; Apply proc to each member of l, with first argument the (0-based) index.
  (define (index-for-each proc l)
    (do ((i 0 (add1 i))
	 (l l (cdr l)))
	((null? l))
      (proc i (car l))))

  ;; Apply proc to each member of l, with first argument the (0-based)
  ;; index.  Return the list of results.
  (define (index-map proc l)
    (let loop ((i 0) (l l))
      (if (null? l)
	  null
	  (cons (proc i (car l))
		(loop (add1 i) (cdr l))))))

  (define (singleton-list? l)
    (and (not (null? l)) (null? (cdr l))))
)
