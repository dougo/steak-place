(require (lib "web-server.ss" "web-server")
	 (lib "configuration.ss" "web-server")
	 (lib "etc.ss"))

(define web-collects
  (build-path (this-expression-source-directory) "www" "collects"))

(current-library-collection-paths
 (cons web-collects (current-library-collection-paths)))

(define shutdown
  (serve (load-configuration (build-path (this-expression-source-directory)
					 "www" "configuration-table"))
	 80 "0.0.0.0")) ;"192.168.1.21"))

