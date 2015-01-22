(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
	 (lib "account.scm" "myweb"))

(unit/sig ()
  (import servlet^)

  (report-errors-to-browser send/back)

  (send/finish (edit-account initial-request))
)


