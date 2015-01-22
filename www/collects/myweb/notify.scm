(module notify mzscheme
  (require (lib "smtp.ss" "net")
	   (lib "head.ss" "net")
	   "names.scm")
  (provide notify-email-change)

  (define *smtp-host* "smtp.comcast.net")
  (define *from-address* "The Steak Place <dougo@place.org>")

  (define (notify-email-change uid email)
    (let* ((name (get-fullname uid (lambda () "Someone")))
	   (to (format "~a <~a>" name email)))
      (smtp-send-message
       *smtp-host*
       *from-address*
       (list to)
       (standard-message-header
	*from-address* (list to) '() '()
	"Address registration")
       (list
	(format "~a registered <~a> as the email address" name email)
	"for a Steak Place account (http://steak.place.org/)."
	"If this was NOT you, please reply to this message"
	"and the account will be suspended."
	))))
)
