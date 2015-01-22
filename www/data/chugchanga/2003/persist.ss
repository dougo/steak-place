(module persist mzscheme
  (require (lib "pretty.ss"))		;pretty-print
  (provide (all-defined))

  ;; Pretty-print a single datum to a text file, closing the port
  ;; afterward.  If the file already exists, its contents will be
  ;; replaced, if possible, otherwise the file will be replaced.
  (define (write-to-file datum path)
    (with-output-to-file path
      (lambda () (pretty-print datum))
      'text 'truncate/replace))

  ;; Read a datum from a text file containing a single datum, closing
  ;; the port afterward.
  (define (read-from-file path)
    (with-input-from-file path
      (lambda () (read))
      'text))
)
