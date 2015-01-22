(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
	 (lib "logout.scm" "myweb"))

(unit/sig ()
  (import servlet^)

  (report-errors-to-browser send/back)

  (send/finish (logout initial-request))
)

