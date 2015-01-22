(module bug mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start request)
    (report-errors-to-browser send/back)
    (/ 1 0)))
