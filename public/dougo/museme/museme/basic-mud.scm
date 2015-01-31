; a simple MUD to test it out...

(define key (make-key "MUD"))

(define four-ai (make-room key "4 AI"))
(set-description! four-ai key "This is the fourth floor of the MIT AI Lab.")

(define california (make-room key "California"))
(set-description! california key "A sunny beach in California.")

(define north-pole (make-room key "North Pole"))
(set-description! north-pole key "This is the North Pole.")

(define ocean (make-room key "Middle of the Atlantic Ocean (in a boat)"))
(set-description! ocean key "You're in a boat in the middle of the Atlantic Ocean. Watch out for sharks!")
(set-pub-handler! ocean key 'accept? (lambda (what src)
				       (if (thing? what)
					   (print "(Into the boat)"))
				       #t))

(define atlantis (make-room key "Lost City of Atlantis (swimming)"))
(set-description! atlantis key "Cool! There are lots of crumbling pillars and collapsed buildings around.")

(make-exit four-ai key 'north north-pole)
(make-exit four-ai key 'west california)
(make-exit four-ai key 'east ocean)
(make-exit california key 'east four-ai)
(make-exit california key 'north north-pole)
(make-exit ocean key 'west four-ai)
(make-exit ocean key 'down (lambda (person)
			     (if (null? (contents person))
				 atlantis
				 (begin (print "You don't want to get the items you're carrying wet.")
					#f))))
(make-exit atlantis key 'up ocean)
(make-exit north-pole key 'south (let ((places (list four-ai california ocean))
				       (state 0))
				   (lambda (person)
				     (set! state (if (= state 2)
						     1
						     (+ state 1)))
				     (list-ref places state))))
(make-exit north-pole key 'north "How could you possibly go north from here?" #f)

(define computer (make-thing key "computer" four-ai #f))
(set-description! computer key "It's an HP Snake.")
(set-aliases! computer key '("snake" "unix box" "box"))

(define statue (make-thing key "gold statue" atlantis #t))
(set-description! statue key "A statue of Gnu, the Atlantean god of free software.")
(set-aliases! statue key '("statue" "gnu"))

(define bbf (make-thing key "black-box frobozzinator" four-ai #t))
(set-description! bbf key "You have no clue what this does.")
(set-aliases! bbf key '("black box frobozzinator" "frobozzinator" "box" "black-box" "black box"))

(define cd (make-thing key "Pearl Jam CD" four-ai #t))
(set-description! cd key "It's `Ten'")
(set-aliases! cd key '("cd" "pearl jam"))

