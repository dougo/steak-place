(module web-collects mzscheme
  (require (lib "etc.ss") (lib "list.ss" "srfi" "1"))
  (define web-collects
    (build-path (this-expression-source-directory) "www" "collects"))

  (current-library-collection-paths
   (lset-adjoin (lambda (p1 p2) (string=? (path->string p1) (path->string p2)))
		(current-library-collection-paths) web-collects))
)
