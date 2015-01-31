;;;----------------------------------------------------------------------------
;;; MUD-COMMANDS.SCM
;;; 
;;; Change log:
;;; * 11/23/94 (lyn) 
;;;   + Updated TAKE and DROP to deal handle scheme objects in a reasonable
;;;     way. Convenient, but not sure this is the right thing.
;;;     (because it is outside of the usual mud model).
;;; * 11/23/94 (BJR)
;;;   + Fixed but in DROP: if dropping a scheme object, must convert to
;;;     mud widget first, take it, and then drop.  But don't take objects
;;;     that are not automatically generated or get messages about self
;;;     is trying to take object from self.
;;; * 1/6/94 (danw)
;;;   + changed MUSEME to MUD in the title above to match the actual filename
;;;   + removed BECOME and associated stuff and coordinated with changes
;;;     to nameserver so that you automagically get one character who IS you
;;;   + added CONNECT to parallel DISCONNECT
;;; * 1/9/94 (lyn)
;;;   + Updated stale references of CHAR to ME.
;;;----------------------------------------------------------------------------

; MUSEME commands, etc.

(define key (make-key username))

(define me (make-person key username neuter four-ai))

(define (resolve ref) (resolve-for-person ref me key))

(define connect
  (let ((first-time? #t))
    (lambda (port)
      (set-prv-handler! me key 'output-port port)
      (apply-handler me key 'echo 
		     (if first-time? 
			 (begin (set! first-time? #f)
				"suddenly appears.")
			 "wakes up.")
		     #t)
      (look))))

(define (disconnect)
  (set-prv-handler! me key 'output-port #f)
  (apply-handler me key 'echo "goes to sleep." #t))

(define (quit)
  (disconnect)
  (error 'quit))

(define (say text)
  (ensure-is-a string? text "string")
  (apply-handler me key 'echo (string-append "says, \"" text "\"") #t)
  (apply-handler me key 'write-me (string-append "You say, \"" text "\""))
  )

(define (emote text)
  (ensure-is-a string? text "string")
  (apply-handler me key 'echo text))

(define (whisper person text)
  (ensure-is-a person? person "person")
  (ensure-is-a string? text "string")
  (apply-handler me key 'echo-to person (string-append "whispers \""
							 text "\"")))

(define (page person . text)
  (ensure-is-a person? person "person")
  (if (pair? text)
      (begin (ensure-is-a string? (car text) "string")
	     (apply-handler me key 'echo-to person
			    (string-append "pages you from "
					   (name (location me)) ": " text)))
      (apply-handler me key 'echo-to person
		     (string-append "pages you from "
				    (name (location me))))))

(define (go direction)
  (ensure-is-a symbol? direction "symbol" "direction")
  (let ((dest (can-go? (location me) direction
		       me)))
    (if (room? dest)
	(begin (apply-handler me key 'echo (string-append "goes "
							  (symbol->string
							   direction)
							  ".") #t)
	       (apply-handler me key 'move-self dest)
	       (apply-handler me key 'echo (string-append "wanders in.") #t)
	       (clear-context me key)
	       (look)))))

(define (take thing)
  (define (really-take thing)
    (apply-handler me key 'get thing) 
    (apply-handler me key 'echo (string-append "picks up" (thename thing)) #t)
    (apply-handler me key 'write-me "Taken")
    )
  (cond ((not (widget? thing)) (really-take (scheme->mud key thing)))
	((not (thing? thing)) (print "You can't take that."))
	((inside? thing me) (print "You already have that."))
	(else (really-take thing))))

(define (drop thing)
  (define (really-drop thing)
    (apply-handler me key 'put thing (location me))
    (apply-handler me key 'echo (string-append "drops" (thename thing)) #t)
    (apply-handler me key 'write-me "Dropped")
    )
  (cond ((not (widget? thing)) 
	 (let ((wid (scheme->mud key thing)))
	   (apply-handler me key 'get wid) ; Must TAKE before dropping.
	   (really-drop wid)))
	((not (eq? (location thing) me)) (print "You aren't carrying that."))
	(else (really-drop thing))))

(define (give thing dest)
  (ensure-is-a thing? thing "thing")
  (cond ((not (inside? thing me)) (print "You aren't carrying that."))
	((not (person? dest)) (print "You can't give something to "
				     (aname dest)))
	((apply-handler me key 'put thing dest) (print "Done."))))
	
(define (look . what)
  (let ((what (if (pair? what)
		  (car what)
		  (location me))))
  (describe what me)))

(define (inventory)
  (list-stuff "You are carrying " aname (contents me)
	      "." "You are empty-handed."))

(define (examine what)
  (describe what me))

(define (north) (go 'north))
(define (south) (go 'south))
(define (east) (go 'east))
(define (west) (go 'west))
(define (northeast) (go 'ne))
(define (ne) (go 'ne))
(define (northwest) (go 'nw))
(define (nw) (go 'nw))
(define (southeast) (go 'se))
(define (se) (go 'se))
(define (southwest) (go 'sw))
(define (sw) (go 'sw))
(define (up) (go 'up))
(define (down) (go 'down))
(define (in) (go 'in))
(define (out) (go 'out))

