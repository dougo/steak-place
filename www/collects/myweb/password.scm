(module password mzscheme
  (require ;; (lib "md5.ss" "handin-server")) ;md5
   "../../../md5.scm")
  (require "counter.scm" "table.scm")
  (provide make-user set-password! correct-password? delete-user random-string)

  (define *table* (make-table "passwords"))

  (define make-uid (make-counter "uid"))

  (define (make-salt)
    (random-string 16))

  (define (make-hash uid pass salt)
    (md5password (number->string uid) pass salt))

  (define (make-salt-hash uid pass)
    (let ((salt (make-salt)))
      (cons salt (make-hash uid pass salt))))

  (define (password-matches? uid pass salt-hash)
    (string=? (make-hash uid pass (car salt-hash)) (cdr salt-hash)))

  (define (make-user pass)
    (let ((uid (make-uid)))
      (set-password! uid pass)
      uid))

  (define (get-salt-hash uid)
    (table-get *table* uid (lambda () #f)))

  (define (set-password! uid pass)
    (table-put! *table* uid (make-salt-hash uid pass))
    (void))

  (define (correct-password? uid pass)
    (let ((salt-hash (get-salt-hash uid)))
      (and salt-hash (password-matches? uid pass salt-hash))))

  (define (delete-user uid)
    (table-remove! *table* uid))

  (define *alphanumerics*
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
  (define (random-alphanumeric)
    (string-ref *alphanumerics* (random 62)))

  (define (random-string n)
    (let ((str (make-string n)))
      (do ((i 0 (+ i 1))) ((= i n) str)
	(string-set! str i (random-alphanumeric)))))

  ;; I don't remember where this came from...
  (define (md5password user password salt)
    (md5 (string-append (md5 (string-append password user)) salt)))
)
