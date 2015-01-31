; MUSEME thing

(define (make-thing key myname location mobile?)
  (let ((w (make-widget key myname)))
    (set-description! w key "a thing")
    (set-describer! w key (lambda (observer) (description w)))
    (set-pub-handler! w key 'thing? #t)
    (make-frob w (limited w key) location mobile?)
    w))

(define (thing? x)
  (is-a 'thing? x))
