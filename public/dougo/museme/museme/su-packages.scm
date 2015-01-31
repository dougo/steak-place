;;;----------------------------------------------------------------------------
;;; SU-PACKAGES.SCM
;;; Packages for single-user
;;;
;;; Change log:
;;; * 11/23/94 (BJR) 
;;;   + Adapted from packages.scm
;;;----------------------------------------------------------------------------

;; output that does force-output
(define-structure forced-output
  (export write write-char display newline print)
  (open scheme i/o)
  (files output))

;; the special reader
(define-structure museme-reader
  (export museme-read)
  (open scheme ascii signals i/o features silly)
  (files read))

;; utils
(define-structure utils
  (export list-stuff delq delete filter bifilter lookup adelq
	  ensure-is-a muserror)
  (open scheme signals forced-output)
  (files utils))

;; the database for names/passwords/user-procs/user environments
(define-structure nameserver
  (export user-env lookup-user add-user! delete-user! all-users
	  new-scheme-env scheme->string)
  (open scheme packages package-commands-internal syntactic evaluation
	signals utils 
	extended-ports	;; Just for call-with-string-output-port (BJR)
	)
  (files nameserver))

; ;; the always-running server that actually runs things
; (define-structure server (export script-entry who)
;   (open scheme sockets extended-ports signals ascii museme-reader
; 	handle conditions nameserver display-conditions
; 	ports build primitives threads i/o forced-output utils build)
;   (files server engine))

;; the single-user server that actually runs things
(define-structure single-user (export entry-point who)
  (open scheme sockets extended-ports signals ascii museme-reader
	handle conditions nameserver display-conditions
	ports build primitives threads i/o forced-output utils build)
  (files single-user engine))

;; test MUD
(define-interface museme-mud-interface
  (export list-stuff delq delete filter bifilter
	  lookup adelq ensure-is-a muserror
	  make-key limited
	  make-widget fetch-widget widget? my-widget? uid name
	  set-name! aliases set-aliases! cname aname thename
	  description set-description! describer set-describer! describe
	  set-pub-handler! set-prv-handler! apply-handler
	  is-a with-no-handler
	  make-frob frob? make-frob-holder frob-holder? make-frob-mover
	  location contents transitive-contents inside? mobile?
	  make-listener make-talker audience
	  make-thing thing? 
	  make-object object? value
	  make-person person? male female neuter context add-context
	  clear-context
	  make-room room? can-go? make-exit nowhere
	  resolve-for-person resolve-in-room
	  mud->scheme scheme->mud
	  four-ai nowhere))

(define-interface museme-wizard-interface
  (export master-key Move Echo all-people-names
	  widget-handlers set-widget-handlers! widget-key))

(define-structures
  ((museme-mud-environment museme-mud-interface)
   (museme-wizard-environment (compound-interface museme-mud-interface
						  museme-wizard-interface)))
  (open scheme fluids define-record-types signals forced-output
	extended-ports utils primitives
	debug-data closures)
  (files widget move echo thing person room resolve translate
         object
	 basic-mud))

;; this is what user environments start with
(define-structure museme-scheme
  (export * + - / < <= = > >=

	  abs acos and angle append apply asin assoc assq assv atan
	  
	  begin boolean?
	  
	  caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar
	  caddar cadddr caddr cadr call-with-values car case cdaaar cdaadr
	  cdaar cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr
	  cdddr cddr cdr ceiling char->integer char->integer char-alphabetic?
	  char-ci<=?  char-ci<? char-ci=? char-ci>=?  char-ci>? char-downcase
	  char-lower-case? char-numeric? char-upcase char-upper-case?
	  char-whitespace? char<=? char<?  char=? char>=? char>? char?
	  complex? cond cons cos

	  define define-syntax delay denominator display do dynamic-wind
	  
	  eq? equal? eqv? eval even?  exact->inexact exact? exp expt
	  
	  floor for-each force
	  
	  gcd

	  if imag-part inexact->exact inexact? integer->char integer?
	  
	  lambda lcm length let let* let-syntax letrec letrec-syntax list
	  list->string list->vector list-ref list-tail list? log
	  
	  magnitude make-polar make-rectangular make-string make-vector map max
	  member memq memv min modulo
	  
	  negative? newline not null? number->string number? numerator
	  
	  odd? or
	  
	  pair? positive? procedure?
	  
	  quasiquote quote quotient
	  
	  rational? rationalize real-part real?  remainder reverse round
	  
	  set! set-car! set-cdr! set-fluid! sin sqrt string string->list
	  string->number string->symbol string->symbol string-append 
	  string-ci<=? string-ci<? string-ci=? string-ci>=?  string-ci>?
	  string-copy string-copy string-fill! string-length string-ref
	  string-set! string<=? string<? string=? string=?  string>=?
	  string>? string? substring symbol->string symbol?  syntax-rules

	  tan truncate
	  
	  values vector vector->list vector-fill! vector-length vector-ref
	  vector-set! vector?
	  
	  write write-char
	  
	  zero?

	  error current-output-port

          ;; NAME-SERVER 
	  user-env all-users new-scheme-env scheme->string

          ;; SERVER
          who

	  print)
  (open scheme signals extended-ports nameserver single-user fluids forced-output 
	handle))
