;; Quick approximation of Java-style Vectors as used by the telecom simulation
(define-class <java-vector> () (l))
(define-method (initialize (this <java-vector>) initargs)
  (slot-set! this 'l (if (null? initargs) '() (car initargs))))

(define-generic add-element!)
(define-method (add-element! (this <java-vector>) x)
  (slot-set! this 'l (cons x (slot-ref this 'l))))

(define-generic remove-element!)
(define-method (remove-element! (this <java-vector>) x)
  (slot-set! this 'l (remove x (slot-ref this 'l))))

(define-generic last-element)
(define-method (last-element (this <java-vector>))
  ;; the last-ADDED element...
  (car (slot-ref this 'l)))

(define-generic enumerate)
(define-method (enumerate (this <java-vector>) (f <procedure>))
  (for-each f (slot-ref this 'l)))

(define-class <customer> () (name areacode calls)
  'metaclass <aspectizable-class>)
(define-class <call> () (caller receiver connections)
  'metaclass <aspectizable-class>)
(define-class <connection> () (caller receiver state)
  'metaclass <aspectizable-class>)
(define-class <local> (<connection>) ()
  'metaclass <aspectizable-class>)
(define-class <long-distance> (<connection>) ()
  'metaclass <aspectizable-class>)

;; Customer.java

(define-method (initialize (this <customer>) initargs)
  (slot-set! this 'calls (make <java-vector>))
  (apply initialize this initargs))
(define-method (initialize (this <customer>)
			   (name <string>) (areacode <number>))
  (slot-set! this 'name name)
  (slot-set! this 'areacode areacode))

(define-generic add-call 'generic-class <aspectizable-generic>)
(define-method (add-call (this <customer>) (call <call>))
  (add-element! (slot-ref this 'calls) call))

(define-generic remove-call 'generic-class <aspectizable-generic>)
(define-method (remove-call (this <customer>) (call <call>))
  (remove-element! (slot-ref this 'calls) call))

(define-method (print-object (this <customer>) (port <port>))
  (format port "~a(~a)"
	  (slot-ref this 'name)
	  (slot-ref this 'areacode)))

(define-generic get-areacode 'generic-class <aspectizable-generic>)
(define-method (get-areacode (this <customer>))
  (slot-ref this 'areacode))

(define-generic local-to? 'generic-class <aspectizable-generic>)
(define-method (local-to? (this <customer>) (other <customer>))
  (= (slot-ref this 'areacode) (slot-ref other 'areacode)))

(define-generic call 'generic-class <aspectizable-generic>)
(define-method (call (this <customer>) (receiver <customer>))
  (let ((call (make <call> this receiver)))
    (add-call this call)
    call))

(define-generic pickup 'generic-class <aspectizable-generic>)
(define-method (pickup (this <customer>) (call <call>))
  (pickup call)
  (add-call this call))

(define-generic hangup 'generic-class <aspectizable-generic>)
(define-method (hangup (this <customer>) (call <call>))
  (hangup call this)
  (remove-call this call))

(define-generic merge 'generic-class <aspectizable-generic>)
(define-method (merge (this <customer>) (call1 <call>) (call2 <call>))
  (merge call1 call2)
  (remove-call this call2))

;; Call.java

(define-method (initialize (this <call>) initargs)
  (slot-set! this 'connections (make <java-vector>))
  (apply initialize this initargs))
(define-method (initialize (this <call>)
			   (caller <customer>) (receiver <customer>))
  (slot-set! this 'caller caller)
  (slot-set! this 'receiver receiver)
  (add-element! (slot-ref this 'connections)
		(if (local-to? caller receiver)
		    (make <local> caller receiver)
		    (make <long-distance> caller receiver))))

(define-method (pickup (this <call>))
  (complete (last-element (slot-ref this 'connections))))

(define-generic is-connected? 'generic-class <aspectizable-generic>)
(define-method (is-connected? (this <call>))
  (eq? (get-state (last-element (slot-ref this 'connections)))
       'complete))

(define-method (hangup (this <call>) (c <customer>))
  (enumerate (slot-ref this 'connections)
	     (lambda (conn) (drop conn))))
	    
(define-generic includes? 'generic-class <aspectizable-generic>)
(define-method (includes? (this <call>) (c <customer>))
  (let ((result #f))
    (enumerate (slot-ref this 'connections)
	       (lambda (conn)
		 (set! result (or result (connects? conn c)))))
    result))

(define-method (merge (this <call>) (other <call>))
  (enumerate (slot-ref this 'connections)
	     (lambda (conn)
	       (remove-element! (slot-ref other 'connections) conn)
	       (add-element! (slot-ref this 'connections) conn))))

;; Connection.java

(define-method (initialize (this <connection>) initargs)
  (slot-set! this 'state 'pending)
  (apply initialize this initargs))
(define-method (initialize (this <connection>) (a <customer>) (b <customer>))
  (slot-set! this 'caller a)
  (slot-set! this 'receiver b))

(define-generic get-state 'generic-class <aspectizable-generic>)
(define-method (get-state (this <connection>))
  (slot-ref this 'state))

(define-generic get-caller 'generic-class <aspectizable-generic>)
(define-method (get-caller (this <connection>))
  (slot-ref this 'caller))

(define-generic get-receiver 'generic-class <aspectizable-generic>)
(define-method (get-receiver (this <connection>))
  (slot-ref this 'receiver))

(define-generic complete 'generic-class <aspectizable-generic>)
(define-method (complete (this <connection>))
  (slot-set! this 'state 'complete)
  (format #t "connection completed~%"))

(define-generic drop 'generic-class <aspectizable-generic>)
(define-method (drop (this <connection>))
  (slot-set! this 'state 'dropped)
  (format #t "connection dropped~%"))

(define-generic connects? 'generic-class <aspectizable-generic>)
(define-method (connects? (this <connection>) (c <customer>))
  (or (eq? (slot-ref this 'caller) c) (eq? (slot-ref this 'receiver) c)))

;; Local.java

(define-method (initialize (this <local>) (a <customer>) (b <customer>))
  (call-next-method)
  (format #t "[new local connection from ~a to ~a]~%" a b))

;; LongDistance.java

(define-method (initialize (this <long-distance>) (a <customer>) (b <customer>))
  (call-next-method)
  (format #t "[new long distance connection from ~a to ~a]~%" a b))

;; AbstractSimulation.java

(define-class <abstract-simulation> () ())
(define *the-simulation* #f)

(define-generic run)
(define-method (run (this <abstract-simulation>))
  (let ((jim (make <customer> "Jim" 650))
	(mik (make <customer> "Mik" 650))
	(crista (make <customer> "Crista" 415)))

    (say "jim calls mik...")
    (let ((c1 (call jim mik)))
      (wait 1)
      (say "mik accepts...")
      (pickup mik c1)
      (wait 2)
      (say "jim hangs up...")
      (hangup jim c1)
      (report this jim)
      (report this mik)
      (report this crista))

    (say "mik calls crista...")
    (let ((c2 (call mik crista)))
      (say "crista accepts...")
      (pickup crista c2)
      (wait 1.5)
      (say "crista hangs up...")
      (hangup crista c2)
      (report this jim)
      (report this mik)
      (report this crista))
))

(define-generic report)

(define-generic wait)
(define-method (wait (seconds <number>))
  // FIXME: sleep
  seconds)

(define-generic say)
(define-method (say (s <string>))
  (display s)
  (newline))

;; BasicSimulation.java

(define-class <basic-simulation> (<abstract-simulation>) ())

(define (basic-main)
  (set! *the-simulation* (make <basic-simulation>))
  (run *the-simulation*))

(define-method (report (this <basic-simulation>) (c <customer>))
  this)

;; Timing.java (version1)

(define-class <timing> () (total-connect-time)
  'metaclass <aspect-of-eachobject>)

(define-method (initialize (this <timing>) initargs)
  (slot-set! this 'total-connect-time 0))

(define-generic get-total-connect-time)
(define-method (get-total-connect-time (this <timing>))
  (slot-ref this 'total-connect-time))

(define-generic start-timing 'generic-class <pointcut>)
(define-method (start-timing (this <timing>) (jp <join-point>)) #f)
(define-method (start-timing (this <timing>) (jp <reception-join-point>))
  (eq? (generic-name (slot-ref jp 'generic)) 'complete))

(define-generic end-timing 'generic-class <pointcut>)
(define-method (end-timing (this <timing>) (jp <join-point>)) #f)
(define-method (end-timing (this <timing>) (jp <reception-join-point>))
  (eq? (generic-name (slot-ref jp 'generic)) 'drop))

(define-class <connect-timing> () (timer)
  'metaclass <aspect-of-eachobject>)

(define-method (initialize (this <connect-timing>) initargs)
  (call-next-method)
  (slot-set! this 'timer (make <timer>)))

(define-generic get-timer)
(define-method (get-timer (this <connect-timing>))
  (slot-ref this timer))

(define-method (start-timing (this <connect-timing>) (jp <join-point>))
  ;; Timing.startTiming()
  ;; FIXME: static method on <timing>
  (start-timing (make <timing>) jp))
(define-method (end-timing (this <connect-timing>) (jp <join-point>))
  ;; instanceof(Connection) & Timing.endTiming()
  (and (eq? (class-name (class-of (this jp))) '<connection>)
       ;; FIXME: static method on <timing>
       (end-timing (make <timing>) jp)))

(define-generic after-start-timing 'generic-class <after-advice>)
(define-method (after-start-timing (this <connect-timing>) (jp <join-point>))
  (start (slot-ref this 'timer)))
(slot-set! after-start-timing 'pointcut start-timing)
(add-advice! <connect-timing> after-start-timing)

(define-generic after-end-timing 'generic-class <after-advice>)
(define-method (after-end-timing (this <connect-timing>) (jp <join-point>))
  (let ((c (this jp))
	(timer (slot-ref this 'timer)))
    (stop timer)
    (let ((caller-timing-instance (aspect-of <timing> (get-caller c)))
	  (receiver-timing-instance (aspect-of <timing> (get-receiver c))))
      (slot-set! caller-timing-instance 'total-connect-time
		 (+ (slot-ref caller-timing-instance 'total-connect-time)
		    (get-time timer)))
      (slot-set! receiver-timing-instance 'total-connect-time
		 (+ (slot-ref receiver-timing-instance 'total-connect-time)
		    (get-time timer))))))
(slot-set! after-end-timing 'pointcut end-timing)
(add-advice! <connect-timing> after-end-timing)

;; Timer.java

(require 'current-time)

(define-class <timer> () (start-time stop-time) 'metaclass <aspectizable-class>)

(define-generic start 'generic-class <aspectizable-generic>)
(define-method (start (this <timer>))
  (slot-set! this 'start-time (current-time))
  (slot-set! this 'stop-time (slot-ref this 'start-time)))

(define-generic stop 'generic-class <aspectizable-generic>)
(define-method (stop (this <timer>))
  (slot-set! this 'stop-time (current-time)))

(define-generic get-time 'generic-class <aspectizable-generic>)
(define-method (get-time (this <timer>))
  (- (slot-ref this 'stop-time) (slot-ref this 'start-time)))

;; TimerLog.java

(define-class <timer-log> () () 'metaclass <aspect>)

(define-generic timer-start 'generic-class <pointcut>)
(define-method (timer-start (this <timer-log>) (jp <join-point>)) #f)
(define-method (timer-start (this <timer-log>) (jp <reception-join-point>))
  ;; instanceof(Timer) & receptions(* start())
  (and (eq? (class-name (this jp)) '<timer>)
       (eq? (generic-name (slot-ref jp 'generic)) 'start)
       (= (length (slot-ref jp 'args)) 1)))

(define-generic timer-stop 'generic-class <pointcut>)
(define-method (timer-stop (this <timer-log>) (jp <join-point>)) #f)
(define-method (timer-stop (this <timer-log>) (jp <reception-join-point>))
  ;; instanceof(Timer) & receptions(* stop())
  (and (eq? (class-name (this jp)) '<timer>)
       (eq? (generic-name (slot-ref jp 'generic)) 'stop)
       (= (length (slot-ref jp 'args)) 1)))

(define-generic after-timer-start 'generic-class <after-advice>)
(define-method (after-timer-start (this <timer-log>) (jp <join-point>))
  (let ((t (this jp)))
    (format #t "Timer started: ~a~%" (slot-ref t 'start-time))))
(slot-set! after-timer-start 'pointcut timer-start)
(add-advice! <timer-log> after-timer-start)

(define-generic after-timer-stop 'generic-class <after-advice>)
(define-method (after-timer-stop (this <timer-log>) (jp <join-point>))
  (let ((t (this jp)))
    (format #t "Timer stopped: ~a~%" (slot-ref t 'stop-time))))
(slot-set! after-timer-stop 'pointcut timer-stop)
(add-advice! <timer-log> after-timer-stop)

;; Billing.java (version1)

(define *local-rate* 3)
(define *long-distance-rate* 10)

(define-class <billing> () () 'metaclass <aspect>)

(define-class <connection-cost> () (payer my-connection)
  ;; dominates <connect-timing>
  'metaclass <aspect-of-eachobject>)

(define-generic new-connection-caller-receiver 'generic-class <pointcut>)
(define-method (new-connection-caller-receiver (this <connection-cost>)
					       (jp <join-point>))
  #f)
(define-method (new-connection-caller-receiver (this <connection-cost>)
					       (jp <reception-join-point>))
  ;; receptions(new(Customer, Customer))
  (and (eq? (generic-name (slot-ref jp 'generic)) 'initialize)
       (= (length (slot-ref jp 'args)) 3)
       (eq? (class-name (cadr (slot-ref jp 'args))) <customer>)
       (eq? (class-name (caddr (slot-ref jp 'args))) <customer>)))

(define-generic after-new-connection-caller-receiver
  'generic-class <after-advice>)
(define-method (after-new-connection-caller-receiver (this <connection-cost>)
						     (jp <join-point>))
  (let ((c (cadr (slot-ref jp 'args))))
    (slot-set! this 'payer c)))
(slot-set! after-new-connection-caller-receiver 'pointcut
	   new-connection-caller-receiver)
(add-advice! <connection-cost> after-new-connection-caller-receiver)

(define-generic calculate-rate)
(define-method (calculate-rate (this <connection-cost>))
  (calculate-rate this (slot-ref this 'my-connection)))
(define-method (calculate-rate (this <connection-cost>)
			       (my-connection <local>))
  *local-rate*)
(define-method (calculate-rate (this <connection-cost>)
			       (my-connection <long-distance>))
  *long-distance-rate*)

(define-generic new-connection 'generic-class <pointcut>)
(define-method (new-connection (this <connection-cost>)
			       (jp <join-point>))
  #f)
(define-method (new-connection (this <connection-cost>)
			       (jp <reception-join-point>))
  ;; returning (Connection c): receptions(new(..))
  (and (eq? (generic-name (slot-ref jp 'generic)) 'allocate-instance)
       (eq? (class-name (slot-ref jp 'returning)) '<connection>)))

(define-generic after-new-connection 'generic-class <after-advice>)
(define-method (after-new-connection (this <connection-cost>) (jp <join-point>))
  (let ((c (slot-ref jp 'returning)))
    (slot-set! this 'my-connection c)))
(slot-set! after-new-connection 'pointcut new-connection)
(add-advice! <connection-cost> after-new-connection)

(define-generic timing-aspect)
(define-method (timing-aspect (this <connection-cost>))
  (aspect-of <connect-timing> (slot-ref this 'my-connection)))

(define-method (end-timing (this <connection-cost>) (jp <join-point>))
  ;; Timing.endTiming()
  ;; FIXME: static method on <timing>
  (end-timing (make <timing>) jp))

(define-method (after-end-timing (this <connection-cost>) (jp <join-point>))
  (let* ((time (get-time (get-timer (timing-aspect this))))
	 (rate (calculate-rate this))
	 (cost (* rate time)))
    (add-charge (aspect-of <bill> (slot-ref this 'payer)) cost)))
(add-advice! <connection-cost> after-end-timing)

(define-class <bill> () (total-charge) 'metaclass <aspect-of-eachobject>)

(define-generic add-charge)
(define-method (add-charge (this <bill>) (charge <number>))
  (slot-set! this 'total-charge
	     (+ (slot-ref this 'total-charge) charge)))

(define-generic get-total)
(define-method (get-total (this <bill>))
  (slot-ref this 'total-charge))

;; TimingSimulation.java (version1)

(define-class <timing-simulation> (<abstract-simulation>) ())

(define (timing-main)
  (set! *the-simulation* (make <timing-simulation>))
  (run *the-simulation*))

(define-method (report (this <timing-simulation>) (c <customer>))
  (format #t "~a spent ~a~%" c (get-total-connect-time (aspect-of <timing> c))))

;; BillingSimulation.java (version1)

(define-class <billing-simulation> (<abstract-simulation>) ())

(define (billing-main)
  (set! *the-simulation* (make <billing-simulation>))
  (run *the-simulation*))

(define-method (report (this <billing-simulation>) (c <customer>))
  (format #t "~a has been connected for ~a seconds and has a bill of ~a~%"
	  c (get-total-connect-time (aspect-of <timing> c))
	  (get-total (aspect-of <bill> c))))

;; aspect attachment

(define aspectizables
  (list <customer> <call> <connection> <local> <long-distance> <timer>
	add-call remove-call get-areacode local-to?
	call pickup hangup merge is-connected? includes?
	get-state get-caller get-receiver complete drop connects?
	start stop get-time))

(define aspects
  (list <timing> <connect-timing> <timer-log>
	<billing> <connection-cost> <bill>))

(define (attach)
  (for-each (lambda (aspect)
	      (for-each (lambda (aspectizable)
			  (add-aspect! aspectizable aspect))
			aspectizables))
	    aspects))

(define (detach)
  (for-each (lambda (aspect)
	      (for-each (lambda (aspectizable)
			  (remove-aspect! aspectizable aspect))
			aspectizables))
	    aspects))

	      
