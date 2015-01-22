(require (lib "web-server.ss" "web-server")
	 (lib "configuration.ss" "web-server")
	 (lib "etc.ss"))

(define web-collects
  (build-path (this-expression-source-directory) "www" "collects"))

(current-library-collection-paths
 (cons web-collects (current-library-collection-paths)))

(define config
  (load-configuration (build-path (this-expression-source-directory)
				  "www" "test-configuration-table")))
(define shutdown (serve config))
