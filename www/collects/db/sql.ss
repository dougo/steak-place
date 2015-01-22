;;; Project:           Scheme-PG
;;; Author:            David J. Neu, djneu@acm.org
;;; Maintainer:        David J. Neu, djneu@acm.org
;;; Project Home Page: http://scheme-pg.sourceforge.net
;;; Copyright:         Copyright (c) 2004 Universal Technical Resource Services, Inc.
;;; License:           MIT License, see license.txt
;;; CVS Id:            $Id: sql.ss,v 1.1 2004/09/16 17:43:00 djneu Exp $

;;; Modifications by Doug Orleans.

(module sql
  mzscheme

  (require (lib "string.ss" "srfi" "13"))
;;;  (require (lib "s-pg.ss" "scheme-pg")) ; only needed for the esacpe-string call in format-value
  (define (escape-string s)
    ;; Escape backslashes and single-quotes.
    (regexp-replace* "'" (regexp-replace* "\\\\" s "\\\\\\\\") "\\\\'"))

  (define-syntax where-clause-and
    (syntax-rules ()
      ((_) '())
      ((_  e) (list e))
      ((_ e1 e2 e3 ...) (append (list e1 "AND") (where-clause-and e2 e3 ...)))))

  (define-syntax where-clause-or
    (syntax-rules ()
      ((_) '())
      ((_  e) (list e))
      ((_ e1 e2 e3 ...) (append (list e1 "OR") (where-clause-or e2 e3 ...)))))

  (define operator?
    (lambda (aoperator)
      (symbol? aoperator)))

  (define format-operator
    (lambda (aoperator)
      (if (operator? aoperator)
          (string-upcase (format "~a" aoperator))
          (let ((e (make-exn
                    (format "format-operator: expects a symbol, given: ~a" aoperator)
                    (current-continuation-marks))))
            (raise e)))))

;;; column-name/table-name?: column -> boolean
;;; Returns #t if acolumn is a symbol and #f otherwise
  (define column-name?
    (lambda (acolumn)
      (symbol? acolumn)))

;;; format-column-name: symbol -> string
;;;Accepts a symbol representing a column name and returns a string
;;;containing the symbol surrounded by double quotes.  This allows
;;;programmers to use column names that for example contain spaces or
;;;dashes.
  (define format-column-name
    (lambda (acolumn)
      (format "\"~a\"" acolumn)))

;;; column-name/table-name?: column -> boolean
;;; Returns #t if acolumn is a length two proper list of symbols and
;;; #f otherwise.
  (define column-name/table-name?
    (lambda (acolumn)
      (and (list? acolumn)
           (= (length acolumn) 2)
           (symbol? (car acolumn))
           (symbol? (cadr acolumn)))))

;;; format-column-name: symbol -> string
;;; Accepts a length two proper list of symbols symbol, representing a
;;; table name and a column name and returns a string of the form
;;; "table"."column" where table is the table name and column is the
;;; column name. This allows programmers to use table and column names
;;; that for example contain spaces or dashes.
  (define format-column-name/table-name
    (lambda (acolumn)
      (format "\"~a\".\"~a\"" (car acolumn) (cadr acolumn))))

;;; column?: column -> boolean
;;; Returns #t when acolumn is either a Scheme symbol representing a
;;; column name or a proper list of length 2 of symbols representing a
;;; table name and a column name and returns #f otherwise.
  (define column?
    (lambda (acolumn)
      (cond ((column-name? acolumn) #t)
            ((column-name/table-name? acolumn) #t)
            (else #f))))

;;; format-column: acolumn -> string
;;; Returns a string representing column when acolumn is in either of
;;; the two valid column formats.  Otherwise it raises an exception.
  (define format-column
    (lambda (acolumn)
      (cond ((column-name? acolumn)
             (format-column-name acolumn))
            ((column-name/table-name? acolumn)
             (format-column-name/table-name acolumn))
            (else
             (let ((e (make-exn
                       (string->immutable-string
			(format "format-column: expects a symbol or a length two proper list of symbols, given: ~a" acolumn))
                       (current-continuation-marks))))
               (raise e))))))

  (define format-value-string
    "format-value: expects a symbol or a length two proper list of symbols to be used as a column or number or string to be used as a value, given: ~a")

;;; In procedure format-value a value can either be a valid Scheme-PG
;;; column (see column? for the definition), or it can be a valid
;;; Scheme-PG value.  The first case occurs in a condition in which
;;; the values of two database columns are compared.  For example,
;;;     SELECT * FROM pers WHERE first-name = last-name
;;; and
;;;     SELECT * FROM pers,addr WHERE pers.id = addr.id.
;;; The second case occurs in a condition in which the value of a
;;; database column is compared to a constant.  For example,
;;;     SELECT * FROM pers WHERE last-name LIKE 'Do%'
;;; and
;;;     SELECT * FROM pers WHERE age < 22.
;;; If avalue is a valid column it is formatted as one by calling
;;; format-column.  If avalue is not a valid column then there are
;;; three valid situtations: it is a string, it is a number, or it is
;;; a boolean.  If avalue is a string, escape-value is applied and the
;;; result is surrounded by single quotes.  If avalue is a number it
;;; returned as a string without any additional formatting.  If avalue
;;; is a boolean it is returned as the string 't' or 'f' cast to
;;; boolean with the :: operator.  In all other cases an exception is
;;; raised. An example of an additional Scheme data type that could be
;;; supported is a list, which could be used in an SQL IN, e.g. SELECT
;;; * FROM addr WHERE state IN ('NH', 'NJ', 'NY').
  (define format-value
    (lambda (avalue)
      (cond ((column? avalue) (format-column avalue))
            ((string? avalue) (format "'~a'" (escape-string avalue)))
            ((number? avalue) (format "~a" avalue))
	    ((boolean? avalue) (format "'~a'::boolean" (if avalue "t" "f")))
            (else
             (let ((e (make-exn
                       (string->immutable-string
			(format format-value-string avalue))
                       (current-continuation-marks))))
               (raise e))))))

  (define-syntax where-clause
    (syntax-rules (and or not)
      ((where-clause (not x)) (format "NOT ~a" (where-clause x)))
      ;((where-clause (not x)) (list "NOT" (where-clause x))) this would add parentheses      
      ((where-clause (and x)) (where-clause x))
      ((where-clause (and x y ...)) (where-clause-and  (where-clause x) (where-clause y) ...))
      ((where-clause (or x)) (where-clause x))
      ((where-clause (or x y ...)) (where-clause-or  (where-clause x) (where-clause y) ...))
      ((where-clause (operator column value))
      ;(list (format "~a ~a ~a" (format-column `column) (format-operator `operator) (format-value `value)))))) this would add parentheses
       (format "~a ~a ~a" (format-column `column) (format-operator `operator) (format-value `value)))))

  (define-syntax where
    (syntax-rules ()
      ((_ . expr)
       (let ((lwhere-clause (where-clause . expr)))
         (if (null? lwhere-clause)
             ""
             (let ((lwhere-string (format "~a" lwhere-clause)))
               ; remove redundant outer parentheses
               (if (and (eq? (string-ref lwhere-string 0) #\() (eq? (string-ref lwhere-string 0) #\())
                   (format "WHERE ~a " (string-drop-right (string-drop lwhere-string 1) 1))
                   (format "WHERE ~a " lwhere-string))))))))

  (define-syntax comma-separate
    (syntax-rules ()
      ((_) "")
      ((_  e) e)
      ((_ e1 e2 e3 ...) (format "~a,~a" e1 (comma-separate e2 e3 ...)))))

;;; table?: table -> boolean
;;; Returns #t if atable is a symbol and #f otherwise
  (define table?
    (lambda (atable)
      (symbol? atable)))

;;; format-table: symbol -> string
;;; Accepts a symbol representing a table name and returns a string
;;; containing the symbol surrounded by double quotes.  This allows
;;; programmers to use table names that for example contain spaces or
;;; dashes.
  (define format-table
    (lambda (atable)
      (if (table? atable)
          (format "\"~a\"" atable)
          (let ((e (make-exn
		    (string->immutable-string
		     (format "format-table: expects a symbol, given: ~a" atable))
                    (current-continuation-marks))))
            (raise e)))))

;;; This macro contains four rules that are matched depending on
;;; whether the SELECT has a WHERE clause or not and depending on
;;; whether column is a list or the single literal all.
  (define-syntax select
    (syntax-rules (all)
      ((_ (column ...) (table ...))
       (format "SELECT ~a FROM ~a" (comma-separate (format-column `column) ...) (comma-separate (format-table `table) ...)))

      ((_ all (table ...))
       (format "SELECT * FROM ~a" (comma-separate (format-table `table) ...)))

      ((_ (column ...) (table ...) (where clause ...))
       (format "SELECT ~a FROM ~a ~a" (comma-separate (format-column `column) ...)
               (comma-separate (format-table `table) ...) (where clause ...)))

      ((_ all (table ...) (where clause ...))
       (format "SELECT * FROM ~a ~a"
               (comma-separate (format-table `table) ...) (where clause ...)))))

;;; limit: string (non-negative-integer non-negative-integer) -> string
;;; The limit macro appends a LIMIT clause to the end of a SELECT.
;;; The offset argument is the number of rows to be skipped, so an
;;; offset of 0 means that the first row returned will be the first
;;; row of the result.  The number argument is an upper bound on the
;;; number of rows to return.  Less than number rows can be returned
;;; depending on the number of rows in the result and the value of
;;; offset.
  (define-syntax limit
    (syntax-rules ()
      ((_ select (offset number)) (format "~a OFFSET ~a LIMIT ~a" select `offset `number))))
  
  (define format-asc/desc
    (lambda (x)
      (cond ((eq? x 'asc) "ASC")
            ((eq? x 'desc) "DESC")
            (else
             (let ((e (make-exn
		       (string->immutable-string
			(format "format-asc/desc: expects the symbol 'ASC or the symbol 'DESC, given: ~a" x))
                       (current-continuation-marks))))
               (raise e))))))
  
;;; order-by: string (listof (symbol 'asc or 'desc)) -> string
;;; The order-by macro appends an ORDER BY clause to the end of a
;;; SELECT.  It accepts a SELECT statment and a proper list of length
;;; two lists that consists of a symbol representing a column name and
;;; either the symbol asc (to indicate the result should be put in
;;; ascending order) or the symbol desc (to indicate the result should
;;; be put in descending order).
  (define-syntax order-by
    (syntax-rules ()
      ((_ select ((column asc/desc) ...))
       (format "~a ORDER BY ~a" select (comma-separate (format "~a ~a" (format-column `column) (format-asc/desc `asc/desc)) ...)))))
  
;;; distinct: string -> string
;;; The distinct procedure checks to the aselect is a string that begins
;;; with "SELECT".  If so, it replaces "SELECT" with "SELECT DISTINCT",
;;; otherwise it raises an exception.
  (define distinct
    (lambda (aselect)
      (let* ((lselect-str "SELECT")
             (lselect-str-length (string-length lselect-str)))
        (if (and (string? aselect)
                 (equal? (substring aselect 0 lselect-str-length) lselect-str))
            (format "SELECT DISTINCT ~a" (substring aselect (add1 lselect-str-length) (string-length aselect)))
            (let ((e (make-exn
		      (string->immutable-string
		       (format "distinct: expects a SELECT statement as a string, given: ~a" aselect))
                      (current-continuation-marks))))
              (raise e))))))

;;; The insert macro has four rules that support the creation of
;;; INSERT statements.
;;;
;;; The following form constructs the INSERT statement shown:
;;; (insert pers (1 "John" "Doe" 20)) ; rule 3
;;; => INSERT INTO "pers" VALUES (1,'John','Doe',20)
;;;
;;; The following three forms all construct the same INSERT statement:
;;; (insert pers (id first-name last-name age) (1 "John" "Doe" 20)) ; rule 4
;;; (insert pers ((id . 1) (first-name . "John") (last-name . "Doe") (age . 20))) ; rule 2
;;; (insert pers ((id 1) (first-name "John") (last-name "Doe") (age 20))) ; rule 1
;;; => INSERT INTO "pers" ("id","first-name","last-name","age") VALUES (1,'John','Doe',20)
  (define-syntax insert
    (syntax-rules ()
      ((_ table ((column value) ...)) ; rule 1
       (format "INSERT INTO ~a (~a) VALUES (~a)" (format-table `table)
               (comma-separate (format-column `column) ...) (comma-separate (format-value `value) ...)))

      ((_ table ((column . value) ...)) ; rule 2
       (format "INSERT INTO ~a (~a) VALUES (~a)" (format-table `table)
               (comma-separate (format-column `column) ...) (comma-separate (format-value `value) ...)))

      ((_ table (value ...)) ; rule 3
       (format "INSERT INTO ~a VALUES (~a)" (format-table `table) (comma-separate (format-value `value) ...)))

      ((_ table (column ...) (value ...)) ; rule 4
       (format "INSERT INTO ~a (~a) VALUES (~a)"
               (format-table `table) (comma-separate (format-column `column) ...) (comma-separate (format-value `value) ...)))))

;;; The delete macro has two rules that support the creation of
;;; DELETE statements as shown in the examples below:
;;; (delete pers)
;;; => DELETE FROM "pers"
;;;
;;; (delete pers (where (and (< age 45) (= state "NJ"))))
;;; =>  DELETE FROM "pers" WHERE "age" < 45 AND "state" = 'NJ'
  (define-syntax delete
                                        ;(syntax-rules (where)
    (syntax-rules (all)
      ((_  table)
       (format "DELETE FROM ~a" (format-table `table)))

      ((_ table (where clause ...))
       (format "DELETE FROM ~a ~a" (format-table `table) (where clause ...)))))

;;; The update macro has six rules that support the creation of
;;; UPDATE statements.
;;;
;;; The following three forms all construct the same UPDATE statement:
;;; (update pers ((first-name "John") (age 20))) ; rule 1
;;; (update pers ((first-name . "John") (age . 20))) ; rule 2
;;; (update pers ((first-name age) ("John" 20))) ; rule 3
;;; => UPDATE pers SET first-name="John", age=20
;;;
;;; The following three forms all construct the same UPDATE statement:
;;; (update pers ((first-name "John") (age 20)) (where (= last-name "Doe"))) ; rule 1a
;;; (update pers ((first-name . "John") (age . 20)) (where (= last-name "Doe"))) ; rule 2a
;;; (update pers ((first-name age) ("John" 20)) (where (= last-name "Doe"))) ; rule 3a
;;; => UPDATE pers SET first-name="John", age=20 WHERE age < 20
  (define-syntax update
    (syntax-rules (where)
      ((_ table ((column value) ...)) ; rule 1
       (format "UPDATE ~a SET ~a" (format-table `table)
               (comma-separate (format "~a=~a" (format-column `column) (format-value `value)) ...)))

      ((_ table ((column value) ...) (where clause ...)) ; rule 1a
       (format "UPDATE ~a SET ~a ~a" (format-table `table)
               (comma-separate (format "~a=~a" (format-column `column) (format-value `value)) ...)
               (where clause ...)))

      ((_ table ((column . value) ...)) ; rule 2
       (format "UPDATE ~a SET ~a" (format-table `table)
               (comma-separate (format "~a=~a" (format-column `column) (format-value `value)) ...)))

      ((_ table ((column . value) ...) (where clause ...)) ; rule 2a
       (format "UPDATE ~a SET ~a ~a" (format-table `table)
               (comma-separate (format "~a=~a" (format-column `column) (format-value `value)) ...)
               (where clause ...)))

      ((_ table (column ...) (value ...)) ; rule 3
       (format "UPDATE ~a SET ~a" (format-table `table)
               (comma-separate (format "~a=~a" (format-column `column) (format-value `value)) ...)))

      ((_ table (column ...) (value ...) (where clause ...)) ; rule 3a
       (format "UPDATE ~a SET ~a ~a" (format-table `table)
               (comma-separate (format "~a=~a" (format-column `column) (format-value `value)) ...)
               (where clause ...)))))
  (provide
   where
   select
   limit
   order-by
   distinct
   insert
   update
   delete
   ))
