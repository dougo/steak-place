;; Resource Description Framework (RDF) --  http://www.w3.org/RDF/
;; RDF vocabulary.

(module vocab mzscheme
  (require (lib "string.ss" "srfi" "13")) ;string-prefix?
  (provide (all-defined))

  ;; Convert a namespace and a local name to a URI.
  ;; String Symbol -> String
  (define (ns+name->uri ns name)
    (string-append ns (symbol->string name)))

  ;; The RDF namespace name.
  (define *rdf-ns*
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

  ;; Convert a local name to a URI using the RDF namespace.
  ;; Symbol -> String
  (define (rdf-uri name)
    (ns+name->uri *rdf-ns* name))

  (define *rdf:RDF*
    (rdf-uri 'RDF))
  (define *rdf:ID*
    (rdf-uri 'ID))
  (define *rdf:about*
    (rdf-uri 'about))
  (define *rdf:parseType*
    (rdf-uri 'parseType))
  (define *rdf:resource*
    (rdf-uri 'resource))
  (define *rdf:nodeID*
    (rdf-uri 'nodeID))
  (define *rdf:datatype*
    (rdf-uri 'datatype))

  (define *rdf:Description*
    (rdf-uri 'Description))
  (define *rdf:li*
    (rdf-uri 'li))

  (define *rdf:type*
    (rdf-uri 'type))

  ;; Form the URI for a list element with the given index.
  ;; Number -> String
  (define (li-uri index)
    (rdf-uri (string->symbol (string-append "_" (number->string index)))))

  ;; Is a URI a list element property name?
  ;; String -> Boolean
  (define (li? uri)
    (string-prefix? (string-append *rdf-ns* "_") uri))
)
