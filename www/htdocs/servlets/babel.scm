;(current-namespace (make-namespace))

(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

;(load "/home/dougo/fred/babel.scm")

;(let ((game (make game%)))
  (unit/sig ()
    (import servlet^)
    `(html (head (title "Babel"))
	   (body (p ,(->string game)))))
;)
