(module email mzscheme
  (require "table.scm" "notify.scm")
  (provide get-email set-email! email->uid)

  (define *table* (make-table "email"))

  (define (get-email uid)
    (let ((entry (assoc uid (table-map *table* xcons))))
      (and entry (cdr entry))))

  (define (set-email! uid email)
    (let ((old (get-email uid)))
      (unless (equal? old email)
	(table-remove! *table* old)
	(table-put! *table* email uid)
	(notify-email-change uid email))))      

  (define (email->uid email)
    (table-get *table* email (lambda () #f)))

  (define (xcons x y) (cons y x))
)
