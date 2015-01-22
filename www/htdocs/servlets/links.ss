(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server") ;servlet^
	 )

(unit/sig () (import servlet^)

  (report-errors-to-browser send/back)

  `(html
    (head (title "Links"))
    (body
     (hr) (h1 "Links")
     ,@(with-input-from-file "links"
	 (lambda ()
	   (let loop ((line (read-line)))
	     (if (eof-object? line)
		 null
		 (list* `(a ((href ,line)) ,line)
			`(br)
			(loop (read-line)))))))))
  )
