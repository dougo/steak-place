;; XML Namespaces -- http://www.w3.org/TR/REC-xml-names/

(module namespace mzscheme
  (require (lib "xml.ss" "xml"))	;struct attribute
  (require (lib "list.ss" "srfi" "1"))	;filter-map
  (require (lib "string.ss" "srfi" "13")) ;string-*
  (provide (all-defined))

  ;; Extract the namespace name (string) and local name (symbol) of an
  ;; element in the scope of a list of namespaces.  The namespace name
  ;; is the empty string if no namespaces are applicable.
  (define (element-ns+name element namespaces)
    (qname->ns+name (symbol->qname (element-name element))
		    (append (element-namespaces element) namespaces)
		    #t))

  ;; A list of element's attributes that declare namespaces.
  (define (element-namespaces element)
    (filter-map attribute->xmlns (element-attributes element)))

  ;; Extract the namespace name (string) and local name (symbol) of an
  ;; attribute in the scope of a list of namespaces.  The namespace
  ;; name is the empty string if no namespaces are applicable.  (The
  ;; default namespace does not apply to attributes.)
  (define (attribute-ns+name attribute namespaces)
    (qname->ns+name (symbol->qname (attribute-name attribute))
		    namespaces
		    #f))

  ;; A namespace has a name (a URI reference).
  (define-struct xmlns (name))

  ;; A prefixed namespace has a prefix (a symbol).
  (define-struct (prefixed-xmlns xmlns) (prefix))

  ;; The prefix of namespace ns, or #f if it's the default namespace.
  (define (xmlns-prefix ns)
    (and (prefixed-xmlns? ns)
	 (prefixed-xmlns-prefix ns)))

  ;; Convert a namespace declaration attribute to a namespace struct,
  ;; or #f if the attribute is not a namespace delcaration.
  (define (attribute->xmlns attribute)
    (let ((name (attribute-name attribute))
	  (value (attribute-value attribute)))
      (if (eq? name 'xmlns)
	  (make-xmlns value)
	  (let ((s (symbol->string name)))
	    (if (string-prefix? "xmlns:" s)
		(make-prefixed-xmlns
		 value
		 (string->symbol (string-drop s 6)))
		#f)))))
	  
  ;; Does the prefix of a namespace match the given prefix?
  (define (xmlns-prefix-eq? ns prefix)
    (eq? (xmlns-prefix ns) prefix))

  ;; The namespace for the 'xml prefix.
  (define *xml-ns* "http://www.w3.org/XML/1998/namespace")

  ;; Find the namespace name for a namespace prefix (symbol or #f)
  ;; given a list of namespaces, or the empty string if no namespaces
  ;; match or if default? is true and prefix is #f.
  (define (xmlns-prefix->name prefix namespaces default?)
    (case prefix
      ((xml) *xml-ns*)
      ((xmlns) "")
      (else
       (if (or prefix default?)
	   (let ((ns (find (lambda (ns) (xmlns-prefix-eq? ns prefix))
			   namespaces)))
	     (if ns (xmlns-name ns) ""))
	   ""))))

  ;; A qualified name has an optional prefix and a local part.  Both
  ;; are symbols.
  (define-struct qname (prefix local-part))

  ;; Parse a symbol into a qualified name.
  (define (symbol->qname symbol)
    (let* ((s (symbol->string symbol))
	   (i (string-index s #\:)))
      (if i
	  (make-qname (string->symbol (string-take s i))
		      (string->symbol (string-drop s (add1 i))))
	  (make-qname #f symbol))))	

  ;; Expand a qualified name using a list of namespaces.  Return two
  ;; values, the namespace name (string) and the local part (symbol).
  ;; The namespace name is the empty string if no namespaces match or
  ;; if default? is true and qname has no prefix.
  (define (qname->ns+name qname namespaces default?)
    (values (xmlns-prefix->name (qname-prefix qname) namespaces default?)
	    (qname-local-part qname)))


  ;; FIXME: the stuff below isn't related to namespaces

  ;; Determine the language tag for an element, where lang is the
  ;; language tag of the enclosing element.
  (define (element-language-tag element lang)
    (cond ((find (lambda (attr) (eq? (attribute-name attr) 'xml:lang))
		 (element-attributes element))
	   => attribute-value)
	  (else lang)))

  ;; Is an XML name (symbol) reserved by the XML standard?
  (define (reserved-xml-name? name)
    (string-prefix-ci? "xml" (symbol->string name)))

  ;; The content of an element converted to a string.
  (define (element-content->string element)
    (apply string-append (map content->string (element-content element))))

  ;; Convert XML content to a string without escaping.
  (define (content->string content)
    ;; xexpr->string converts "&" to "&amp;", so use pcdata-string
    ;; directly for pcdata content.
    (if (pcdata? content)
	(pcdata-string content)
	(xexpr->string (xml->xexpr content))))
)
