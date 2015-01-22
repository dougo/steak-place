;; Resource Description Framework (RDF) --  http://www.w3.org/RDF/
;; RDF/XML syntax -- http://www.w3.org/TR/rdf-syntax-grammar/

(module xml mzscheme
  (require "structs.ss")
  (require "vocab.ss")
  (require "namespace.ss")
  (require (lib "xml.ss" "xml"))
  (require (lib "list.ss" "srfi" "1"))
  (provide (all-defined))

  ;; Convert a RDF/XML document to a graph.
  (define (document->graph document)
    ;; FIXME: xml:base
    (let ((root-element (document-element document)))
      (make-graph
       (delete-duplicates
	(if (string=? (element-uri root-element null) *rdf:RDF*)
	    (node-elements->triples
	     (element-content root-element)
	     (element-namespaces root-element)
	     (element-language-tag root-element ""))
	    ;; If the root element is not rdf:RDF, it's regarded as a
	    ;; node element itself.
	    (node-element->triples root-element null ""))
	triple=?))))

  ;; Convert a node element list to a list of triples.  Non-elements
  ;; in the node element list are ignored.
  (define (node-elements->triples nes namespaces lang)
    (append-map (lambda (ne) (node-element->triples ne namespaces lang))
		(filter element? nes)))

  ;; Convert a node element to a list of triples.
  (define (node-element->triples ne namespaces lang)
    (let-values (((subject triples)
		  (node-element->subject+triples ne namespaces lang)))
      triples))

  ;; Convert a node element to a subject and a list of triples.
  (define (node-element->subject+triples ne namespaces lang)
    (let* ((namespaces (append (element-namespaces ne) namespaces))
	   (lang (element-language-tag ne lang))
	   (subject (node-element-subject ne namespaces))
	   (type (node-element-type ne namespaces))
	   (triples (append (element-attributes->triples
			     ne subject namespaces lang)
			    (property-elements->triples
			     (element-content ne) subject namespaces lang))))
      (values subject (if type
			  (cons (make-triple subject *rdf:type* type) triples)
			  triples))))

  ;; Determine the subject of a node element.
  ;; FIXME: rdf:ID, rdf:nodeID
  (define (node-element-subject ne namespaces)
    (let* ((namespaces (append (element-namespaces ne) namespaces))
	   (attr (node-element-subject-attribute ne namespaces)))
      (if attr
	  (attribute-value attr)
	  (make-blank-node))))

  ;; Find the subject attribute of a node element, or #f if none is
  ;; present.
  (define (node-element-subject-attribute ne namespaces)
    (element-attribute ne *rdf:about* namespaces))

  ;; The type of a node element, or #f.
  (define (node-element-type ne namespaces)
    (let ((uri (element-uri ne namespaces)))
      (and (not (string=? uri *rdf:Description*))
	   uri)))


  ;; Convert a property element list to a list of triples.
  ;; Non-elements in the property element list are ignored.
  (define (property-elements->triples pes subject namespaces lang)
    (let loop ((pes (filter element? pes))
	       (index 1))
      (if (null? pes)
	  null
	  (let* ((pe (car pes))
		 (namespaces (append (element-namespaces pe) namespaces))
		 (lang (element-language-tag pe lang))
		 (predicate (element-uri pe namespaces)))
	    (let-values (((predicate index)
			  (if (string=? predicate *rdf:li*)
			      (values (li-uri index) (add1 index))
			      (values predicate index))))
	      (append
	       (property-element->triples pe subject predicate namespaces lang)
	       (loop (cdr pes) index)))))))

  ;; Convert a property element to a list of triples.
  (define (property-element->triples pe subject predicate namespaces lang)
    (let-values (((object triples)
		  (property-element-object+triples pe namespaces lang)))
      (cons (make-triple subject predicate object) triples)))

  ;; Determine the object of a property element, plus the list of
  ;; triples below it.
  (define (property-element-object+triples pe namespaces lang)
    ;; FIXME: rdf:parseType, rdf:ID
    (let ((content (element-content pe)))
      (if (null? content)
	  (empty-property-element-object+triples pe namespaces lang)
	  (let ((children (filter element? content)))
	    (cond ((null? children)
		   (values (literal-property-element-object pe namespaces lang)
			   null))
		  ((null? (cdr children))
		   (node-element->subject+triples
		    (car children) namespaces lang))
		  (else
		   (error 'property-element-object+triples
			  "multiple children: ~v" children)))))))

  ;; Determine the object of a literal property element.
  (define (literal-property-element-object pe namespaces lang)
    (let ((datatype-attr (element-attribute pe *rdf:datatype* namespaces))
	  (text (element-content->string pe)))
      (if datatype-attr
	  (make-typed-literal text (attribute-value datatype-attr))
	  (make-plain-literal text lang))))

  ;; Determine the object of an empty property element, plus the list
  ;; of triples below it.
  (define (empty-property-element-object+triples pe namespaces lang)
    (let ((object (empty-property-element-object pe namespaces lang)))
      (values
       object
       (element-attributes->triples pe object namespaces lang))))

  ;; Determine the object of an empty property element.
  (define (empty-property-element-object pe namespaces lang)
    (cond ((null? (element-rdf-attributes pe))
	   (make-plain-literal "" lang))
	  ((element-attribute pe *rdf:resource* namespaces)
	   => attribute-value)
	  ;; FIXME: rdf:nodeID
	  (else
	   (make-blank-node))))


  ;; Extract the URI of an element from its name.
  (define (element-uri element namespaces)
    (let-values (((ns name) (element-ns+name element namespaces)))
      (string-append ns (symbol->string name))))

  ;; Convert an element's attribute list to a list of triples.
  (define (element-attributes->triples element subject namespaces lang)
    (filter-map (lambda (attr)
		  (attribute->triple attr subject namespaces lang))
		(element-rdf-attributes element)))

  ;; The attributes of an element whose names are not reserved by the
  ;; XML standard.
  (define (element-rdf-attributes element)
    (remove (lambda (attr) (reserved-xml-name? (attribute-name attr)))
	    (element-attributes element)))

  ;; Find the attribute of an element with the given URI, or #f if
  ;; none is present.
  (define (element-attribute element uri namespaces)
    (find (lambda (attr) (string=? uri (attribute-uri attr namespaces)))
	  (element-attributes element)))

  ;; Extract the URI of an attribute from its name.
  (define (attribute-uri attribute namespaces)
    (let-values (((ns name) (attribute-ns+name attribute namespaces)))
      (string-append ns (symbol->string name))))

  ;; Convert an attribute to a triple, or #f.
  (define (attribute->triple attr subject namespaces lang)
    (let ((uri (attribute-uri attr namespaces)))
      (if (string=? uri *rdf:type*)
	  (make-triple subject uri (attribute-value attr))
	  (and (property-attribute-uri? uri)
	       (make-triple subject uri (make-plain-literal
					 (attribute-value attr) lang))))))


  ;; Is a URI a valid property attribute name?
  (define (property-attribute-uri? uri)
    (not (member uri *disallowed-property-attribute-uris*)))

  (define *core-syntax-terms*
    (list *rdf:RDF* *rdf:ID* *rdf:about* *rdf:parseType* *rdf:resource*
	  *rdf:nodeID* *rdf:datatype*))

  (define *disallowed-property-attribute-uris*
    (cons* *rdf:Description* *rdf:li* *core-syntax-terms*))
)
