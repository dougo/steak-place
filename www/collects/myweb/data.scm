;; Persistent data, which can be shared by multiple threads (but not
;; multiple processes).  Note that this only works for `read'-able data.
;; You should treat the contents of the datum as immutable (because
;; it's not synchronized).

(module data mzscheme
  (require (lib "etc.ss")		;this-expression-source-directory
	   (lib "file.ss")		;make-directory*, path-only
	   (lib "pretty.ss"))		;pretty-print
  (provide (rename make make-datum)
	   (rename get-value datum-value)
	   (rename set-value! set-datum-value!)
	   (rename update-value! update-datum-value!)
	   *data-path*)

  (define *data-path*
    (build-path (this-expression-source-directory) 'up 'up "data"))

  (define-struct datum (file semaphore value))

  (define (make file default)
    (let ((file (build-path *data-path* file)))
      (make-directory* (path-only file))
      (make-datum
       file
       (make-semaphore 1)
       (if (file-exists? file)
	   (read-from-file file)
	   (begin
	     (write-to-file file default)
	     default)))))

  (define (persist datum)
    (write-to-file (datum-file datum) (datum-value datum)))

  (define (get-value datum)
    (call-with-semaphore (datum-semaphore datum)
      (lambda () (datum-value datum))))

  (define (set-value! datum value)
    (update-value! datum (lambda (old) value)))

  (define (update-value! datum proc)
    (call-with-semaphore (datum-semaphore datum)
      (lambda ()
	(let* ((old (datum-value datum))
	       (new (proc old)))
	  (unless (equal? old new)
	    (set-datum-value! datum new)
	    (persist datum))
	  (values old new)))))

  (define (read-from-file file)
    (with-input-from-file file read))

  (define (write-to-file file x)
    (with-output-to-file file
      (lambda () (pretty-print x))
      'truncate))
)
