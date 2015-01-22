(module names mzscheme
  (require (lib "etc.ss")		;opt-lambda
	   "table.scm")
  (provide get-fullname set-fullname!)

  (define *table* (make-table "names"))

  (define get-fullname
    (opt-lambda (uid (failure-thunk
		      (lambda () "Anonymous Steak Place Customer")))
      (table-get *table* uid failure-thunk)))

  (define (set-fullname! uid name)
    (if (string=? name "")
	(table-remove! *table* uid)
	(table-put! *table* uid name)))
)

