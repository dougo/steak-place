;;;;;;;;;;;;;;;; Trace.java

(define-class <trace> () (tracelevel traceport calldepth) 'metaclass <aspect>)

(define-method (initialize (trace <trace>) initargs)
  (slot-set! trace 'tracelevel 2)
  (slot-set! trace 'traceport (current-output-port))
  (slot-set! trace 'calldepth 0)
  (call-next-method))

(define-generic init-traceport)
(define-generic indent)
(define-generic entering)
(define-generic exiting)
(define-generic trace-entry)
(define-generic trace-exit)

(define-method (init-traceport (trace <trace>) (port <port>))
  (slot-set! trace port)
  'ok)

(define-method (indent (trace <trace>))
  (let loop ((depth (slot-ref trace 'calldepth)))
    (when (> depth 0)
	  (display "  " (slot-ref trace 'traceport))
	  (loop (1- depth)))))

(define-method (entering (trace <trace>) (signature <signature>))
  (indent trace)
  (format (slot-ref trace 'traceport) "Entering ~a~%" signature))

(define-method (exiting (trace <trace>) (signature <signature>))
  (indent trace)
  (format (slot-ref trace 'traceport) "Exiting ~a~%" signature)) 

(define-method (trace-entry (trace <trace>) (signature <signature>))
  (unless (= (slot-ref trace 'tracelevel) 0)
	  (when (= (slot-ref trace 'tracelevel) 2)
		(slot-set! trace 'calldepth (1+ (slot-ref trace 'calldepth))))
	  (entering trace signature)))

(define-method (trace-exit (trace <trace>) (signature <signature>))
  (unless (= (slot-ref trace 'tracelevel) 0)
	  (exiting trace signature)
	  (when (= (slot-ref trace 'tracelevel) 2)
		(slot-set! trace 'calldepth (1- (slot-ref trace 'calldepth))))))

;; (define-pointcut (classes (trace <trace>) (jp <join-point>)) #f)
(define-generic classes 'generic-class <pointcut>)
;; This method probably just shouldn't be defined, since it's abstract.
(define-method (classes (trace <trace>) (jp <join-point>)) #f)

(define-generic methods 'generic-class <pointcut>)
(define-method (methods (trace <trace>) (jp <join-point>)) #f)
(define-method (methods (trace <trace>) (jp <execution-join-point>))
  (classes trace jp))

;; (define-before-advice ((trace <trace>) (jp <join-point>)) ...)
(define-generic before-methods 'generic-class <before-advice>)
(define-method (before-methods (trace <trace>) (jp <join-point>))
  (trace-entry trace (make <signature>
		       'generic (slot-ref jp 'generic) 
		       'method (slot-ref jp 'method))))
(slot-set! before-methods 'pointcut methods)
(add-advice! <trace> before-methods)

(define-generic after-methods 'generic-class <after-advice>)
(define-method (after-methods (trace <trace>) (jp <join-point>))
  (trace-exit trace (make <signature>
		      'generic (slot-ref jp 'generic) 
		      'method (slot-ref jp 'method))))
(slot-set! after-methods 'pointcut methods)
(add-advice! <trace> after-methods)


;; TraceMyClasses.java
(define-class <trace-my-classes> (<trace>) ()
  'metaclass <aspect-of-eachVM>)

(define-method (classes (trace <trace-my-classes>) (jp <join-point>))
  ;; within(TwoDShape) || within(Circle) || within(Square)
  (memq (class-name (car (method-specializers (slot-ref jp 'method))))
	'(<2d-shape> <circle> <square>)))

;; aspect attachment

(define (aspectizables)
  (list <2d-shape> <circle> <square>
	get-x get-y distance perimeter area))

(define (attach)
  (for-each (lambda (aspectizable)
	      (add-aspect! aspectizable <trace-my-classes>))
	    (aspectizables)))

(define (detach)
  (for-each (lambda (aspectizable)
	      (remove-aspect! aspectizable <trace-my-classes>))
	    (aspectizables)))
