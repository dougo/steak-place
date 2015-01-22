;; Persistent thread-synchronized counters.

(module counter mzscheme
  (require "data.scm")
  (provide make-counter)

  (define (make-counter file)
    (let ((counter (make-datum file 0)))
      (lambda ()
	(let-values (((old new) (update-datum-value! counter add1)))
	  old))))
)

