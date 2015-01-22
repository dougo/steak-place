(define (initialize-slots object initargs)
  (let ((not-there (list 'shes-not-there)))
    (for-each (lambda (slot)
		(let ((name (car slot)))
		  (let ((value  (getl initargs name not-there)))
		    (if (eq? value not-there)
			'do-nothing
			(slot-set! object name value)))))
	      (class-slots (class-of object)))))

;;;;;;;;;;;;;;;; TwoDShape.java

(define-class <2d-shape> () (x y) 'metaclass <aspectizable-class>)

(define-method (initialize (shape <2d-shape>) initargs)
  (call-next-method)
  (slot-set! shape 'x 0)
  (slot-set! shape 'y 0)
  (initialize-slots shape initargs))

(define-generic get-x 'generic-class <aspectizable-generic>)
(define-method (get-x (shape <2d-shape>))
  (slot-ref shape 'x))
(define-generic get-y 'generic-class <aspectizable-generic>)
(define-method (get-y (shape <2d-shape>))
  (slot-ref shape 'y))

(define-generic distance 'generic-class <aspectizable-generic>)
(define-method (distance (shape1 <2d-shape>) (shape2 <2d-shape>))
  (let ((dx (abs (- (get-x shape1) (get-x shape2))))
	(dy (abs (- (get-y shape1) (get-y shape2)))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(define-generic perimeter 'generic-class <aspectizable-generic>)
(define-generic area 'generic-class <aspectizable-generic>)

(define-method (print-object (shape <2d-shape>) (port <port>))
  (display " @ (" port)
  (display (get-x shape) port)
  (display ", " port)
  (display (get-y shape) port)
  (display ") " port))

;;;;;;;;;;;;;;;; Circle.java

(define-class <circle> (<2d-shape>) (r) 'metaclass <aspectizable-class>)

(define-method (initialize (circle <circle>) initargs)
  (slot-set! circle 'r 1)
  (call-next-method))

(define *pi* 3.14159)

(define-method (perimeter (circle <circle>))
  (* 2 *pi* (slot-ref circle 'r)))

(define-method (area (circle <circle>))
  (let ((r (slot-ref circle 'r)))
    (* *pi* r r)))

(define-method (print-object (circle <circle>) (port <port>))
  (display "Circle radius = " port)
  (display (slot-ref circle 'r) port)
  (call-next-method))

;;;;;;;;;;;;;;;; Square.java

(define-class <square> (<2d-shape>) (s) 'metaclass <aspectizable-class>)

(define-method (initialize (square <square>) initargs)
  (slot-set! square 's 1)
  (call-next-method))

(define-method (perimeter (square <square>))
  (* 4 (slot-ref square 's)))

(define-method (area (square <square>))
  (let ((s (slot-ref square 's)))
    (* s s)))

(define-method (print-object (square <square>) (port <port>))
  (display "Square side = " port)
  (display (slot-ref square 's) port)
  (call-next-method))

;;;;;;;;;;;;;;;; ExampleMain.java

(define (main)
  (let ((c1 (make <circle> 'x 3 'y 3 'r 2))
	(c2 (make <circle> 'r 4))
	(s1 (make <square> 'x 1 'y 2)))
    (format #t "(perimeter c1) => ~a~%" (perimeter c1))
    (format #t "(area c1) => ~a~%" (area c1))
    (format #t "(perimeter s1) => ~a~%" (perimeter s1))
    (format #t "(area s1) => ~a~%" (area s1))
    (format #t "(distance c2 c1) => ~a~%" (distance c2 c1))
    (format #t "(distance s1 c1) => ~a~%" (distance s1 c1))
    (format #t "s1 => ~a~%" s1)))
