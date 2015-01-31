#lang web-server

(provide interface-version start)

(define interface-version 'stateless)

(require (lib "servlet.ss" "web-server"))
(require (lib "response-structs.ss" "web-server" "http"))
(require (lib "html.ss" "myweb"))
(require (only-in (lib "etc.ss") this-expression-source-directory))
(require (only-in (lib "struct.ss") copy-struct))
(require (only-in (lib "1.ss" "srfi") filter filter-map))

(define *web-root* (build-path (this-expression-source-directory) 'up 'up "public"))
(define *htdocs* (build-path *web-root* "htdocs"))
(define *digicam-root*
  (build-path *web-root* "digicam")
  ;(build-path *htdocs* "digicam")
  )
(define *digicam-url-root* "/digicam")

(define-struct pic (dir number))
(define-struct dir (year month date))

;; Extract a numeric field from an environment of HTTP request
;; bindings, or raise an exception if it's missing or duplicated or
;; not a number.
(define (extract-num name env)
  (string->number (extract-binding/single name env)))

;; Create a pic structure from an environment of HTTP request bindings.
(define (extract-pic env)
  (make-pic (make-dir (extract-num 'y env)
                      (extract-num 'm env)
                      (extract-num 'd env))
            (extract-num 'n env)))

;; Convert a pic to a URL query string.
(define (pic->query pic)
  (let ((dir (pic-dir pic)))
    (format "?y=~a;m=~a;d=~a;n=~a"
            (dir-year dir)
            (dir-month dir)
            (dir-date dir)
            (pic-number pic))))

;; Convert a pic number to a relative filename string.
(define (number->filename num rotated?)
  (format "1~a_~a~a"
          (floor (/ (sub1 num) 100))
          num
          (if rotated? "_r1.jpg" ".JPG")))

;; Convert a relative file path to a pic number.
(define (filepath->number path)
  (let ((matches (regexp-match #rx"_([0-9]+)(.JPG|_r1.jpg)"
                               (path->string path))))
    (and matches (string->number (cadr matches)))))

(define (dir< dir1 dir2)
  (let ((y1 (dir-year dir1)) (m1 (dir-month dir1)) (d1 (dir-date dir1))
        (y2 (dir-year dir2)) (m2 (dir-month dir2)) (d2 (dir-date dir2)))
    (or (< y1 y2)
        (and (= y1 y2)
             (or (< m1 m2)
                 (and (= m1 m2)
                      (< d1 d2)))))))

;; This is gross... refactor!
(define (max-dir . dirs)
  (if (null? dirs)
      #f
      (let loop ((dirs (cdr dirs)) (max (car dirs)))
        (if (null? dirs)
            max
            (let ((dir (car dirs)))
              (loop (cdr dirs) (if (dir< max dir) dir max)))))))
(define (min-dir . dirs)
  (if (null? dirs)
      #f
      (let loop ((dirs (cdr dirs)) (min (car dirs)))
        (if (null? dirs)
            min
            (let ((dir (car dirs)))
              (loop (cdr dirs) (if (dir< dir min) dir min)))))))

;; Convert a dir to a relative directory name string.
(define (dir->dirname dir)
  (format "~a_~a~a_~a~a"
          (dir-year dir)
          (if (< (dir-month dir) 10) "0" "")
          (dir-month dir)
          (if (< (dir-date dir) 10) "0" "")
          (dir-date dir)))

;; Convert a dir to an absolute directory path.
(define (dir->dirpath dir)
  (build-path *digicam-root* (dir->dirname dir)))

;; Convert a relative directory path to a dir.
(define (dirpath->dir dir)
  (let ((matches (regexp-match #rx"^([0-9]+)_([0-9]+)_([0-9]+)$"
                               (path->string dir))))
    (and matches (apply make-dir (map string->number (cdr matches))))))

;; A list of all dirs that have pictures.
(define (pic-dirs)
  (filter-map dirpath->dir (directory-list *digicam-root*)))

(define (prev-dir dir)
  (apply max-dir (filter (lambda (d) (dir< d dir)) (pic-dirs))))

(define (next-dir dir)
  (apply min-dir (filter (lambda (d) (dir< dir d)) (pic-dirs))))


;; Convert a pic to an absolute file path.
(define (pic->path pic rotated?)
  (build-path *digicam-root*
              (dir->dirname (pic-dir pic))
              (number->filename (pic-number pic) rotated?)))

(define (pic-exists? pic rotated?)
  (file-exists? (pic->path pic rotated?)))

(define (pic->url-path pic)
  (and (pic-exists? pic #f)
       (format "~a/~a/~a"
               *digicam-url-root*
               (dir->dirname (pic-dir pic))
               (number->filename (pic-number pic) (pic-exists? pic #t)))))

;; Convert a pic to an xexpr linking to it (or blank if the pic does
;; not exist).
(define (pic->link pic)
  (if pic
      `(a ((href ,(pic->query pic)))
          (tt ,(number->string (pic-number pic))))
      '(tt (nbsp nbsp nbsp nbsp nbsp))))

;; A list of picture numbers for a given dir.
(define (pic-nums-for-dir dir)
  (if dir
      (filter-map filepath->number (directory-list (dir->dirpath dir)))
      null))

;; This is gross too!  Refactor!
(define (prev-pic p nums)
  (let ((prev-nums (filter (lambda (num) (< num (pic-number p))) nums)))
    (if (null? prev-nums)
        (let* ((prev-dir (prev-dir (pic-dir p)))
               (prev-nums (pic-nums-for-dir prev-dir)))
          (if (null? prev-nums)
              #f
              (make-pic prev-dir (apply max prev-nums))))
        (make-pic (pic-dir p) (apply max prev-nums)))))

(define (next-pic p nums)
  (let ((next-nums (filter (lambda (num) (> num (pic-number p))) nums)))
    (if (null? next-nums)
        (let* ((next-dir (next-dir (pic-dir p)))
               (next-nums (pic-nums-for-dir next-dir)))
          (if (null? next-nums)
              #f
              (make-pic next-dir (apply min next-nums))))
        (make-pic (pic-dir p) (apply min next-nums)))))

;; The previous and next pics.
(define (neighbor-pics p)
  (let ((nums (pic-nums-for-dir (pic-dir p))))
    (values
     (prev-pic p nums)
     (next-pic p nums))))

(define (pic->page pic)
  (let ((pic-link (pic->url-path pic)))
    (if (not pic-link)
        (make-not-found-response)
        (let-values (((prev next) (neighbor-pics pic)))
          (make-page
           (number->string (pic-number pic))
           `((p ,(pic->link prev) "   |   " ,(pic->link next))
             (p (iframe ((width "100%") (height "80%")
                         (src ,pic-link))
                        "(this uses an iframe)"))))))))

(define (start initial-request)
  (with-errors-to-browser
   send/back
   (lambda ()
     (pic->page (extract-pic (request-bindings initial-request))))))

