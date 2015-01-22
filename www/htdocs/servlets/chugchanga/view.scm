(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
	 (lib "poll.scm" "chugchanga"))

(unit/sig ()
  (import servlet^)

  (report-errors-to-browser send/back)

  (send/finish (view-poll-entry initial-request))
)
