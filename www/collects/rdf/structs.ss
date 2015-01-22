;; Resource Description Framework (RDF) --  http://www.w3.org/RDF/
;; Abstract syntax -- http://www.w3.org/TR/rdf-concepts/#section-Graph-syntax

(module structs mzscheme
  (require (lib "list.ss" "srfi" "1"))	;filter
  (require (lib "string.ss" "srfi" "13")) ;string-null?
  (require "vocab.ss")
  (provide (all-defined))

  ;; A graph is a set (list) of triples.
  ;; Graph = (listof Triple)
  (define-struct graph (triples))

  ;; A triple.  Yes, that's really what it's called.  Not very imaginative!
  ;; The subject is either a URI reference or a blank node.
  ;; The predicate is a URI reference.
  ;; The object is either a URI reference, a literal, or a blank node.
  ;; All URI references should be absolute.
  ;; Triple = Node Predicate Node
  (define-struct triple (subject predicate object))

  ;; Triple Triple -> Boolean
  (define (triple=? t1 t2)
    (and (node=? (triple-subject t1) (triple-subject t2))
	 (predicate=? (triple-predicate t1) (triple-predicate t2))
	 (node=? (triple-object t1) (triple-object t2))))

  ;; Node = String | Literal | Blank-Node

  ;; Node Node -> Boolean
  (define (node=? n1 n2)
    (or (and (blank-node? n1) (blank-node? n2)
	     (blank-node=? n1 n2))
	(and (string? n1) (string? n2)
	     (string=? n1 n2))
	(and (literal? n1) (literal? n2)
	     (literal=? n1 n2))))

  ;; Predicate = String

  ;; Predicate Predicate -> Boolean
  (define (predicate=? p1 p2)
    (string=? p1 p2))

  ;; A literal has a lexical form (string).
  ;; Literal = Plain-Literal | Typed-Literal
  (define-struct literal (lexical-form))

  ;; Literal Literal -> Boolean
  (define (literal=? l1 l2)
    (or (and (plain-literal? l1) (plain-literal? l2)
	     (plain-literal=? l1 l2))
	(and (typed-literal? l1) (typed-literal? l2)
	     (typed-literal=? l1 l2))))

  ;; A plain literal is a literal with a language tag.  The language
  ;; tag is either a lowercase string in the form defined by RFC-3066,
  ;; or the empty string.
  ;; Plain-Literal = String String
  (define-struct (plain-literal literal) (language-tag))

  ;; Plain-Literal Plain-Literal -> Boolean
  (define (plain-literal=? pl1 pl2)
    (and (string=? (literal-lexical-form pl1)
		   (literal-lexical-form pl2))
	 (string=? (plain-literal-language-tag pl1)
		   (plain-literal-language-tag pl2))))

  ;; A typed literal is a literal with a datatype URI reference.
  ;; Typed-Literal = String String
  (define-struct (typed-literal literal) (datatype))

  ;; Typed-Literal Typed-Literal -> Boolean
  (define (typed-literal=? tl1 tl2)
    (and (string=? (literal-lexical-form tl1)
		   (literal-lexical-form tl2))
	 (string=? (typed-literal-datatype tl1)
		   (typed-literal-datatype tl2))))

  ;; A blank node is simply a unique value.
  ;; Blank-Node =
  (define-struct blank-node ())

  ;; Blank-Node Blank-Node -> Boolean
  (define (blank-node=? bn1 bn2)
    (eq? bn1 bn2))


  ;; The list of triples in a graph with the given subject, predicate,
  ;; and/or object.  Any of these may be #f to match any triple.
  ;; Graph (union Node #f) (union Predicate #f) (union Node #f)
  ;;   -> (listof Triple)
  (define (graph-match graph subject predicate object)
    (filter
     (lambda (triple)
       (and (or (not subject)
		(node=? (triple-subject triple) subject))
	    (or (not predicate)
		(predicate=? (triple-predicate triple) predicate))
	    (or (not object)
		(node=? (triple-object triple) object))))
     (graph-triples graph)))

  ;; The object of the triple with the given subject and predicate in
  ;; a graph, or #f if no such triple exists.  Raises an error if more
  ;; than one triple matches.
  ;; Graph Node Predicate -> (union Node #f)
  (define (graph-object graph subject predicate)
    (check-singleton
     (map triple-object (graph-match graph subject predicate #f))))

  ;; The subject of the triple with the given object and predicate in
  ;; a graph, or #f if no such triple exists.  Raises an error if more
  ;; than one triple matches.
  ;; Graph Node Predicate -> (union Node #f)
  (define (graph-subject graph object predicate)
    (check-singleton
     (map triple-subject (graph-match graph #f predicate object))))

  ;; The type of a node in a graph, or #f if untyped.  Raises an error
  ;; if the node has more than one type.
  ;; Graph Node -> (union String #f)
  (define (graph-node-type graph node)
    (graph-object graph node *rdf:type*))

  ;; The node with a given type in a graph, or #f if no such node.
  ;; Raises an error if there are more than one nodes of that type.
  ;; Graph String -> (union Node #f)
  (define (graph-typed-node graph type)
    (graph-subject graph type *rdf:type*))

  ;; The nodes in a container in a graph.
  ;; Graph Node -> (list Node)
  (define (graph-container-contents graph container)
    (map triple-object
	 (filter (lambda (triple) (li? (triple-predicate triple)))
		 (graph-match graph container #f #f))))

  ;; The single value in a singleton list, or #f if null.  Raises an
  ;; error if list has more than one element.
  ;; (union (list) (list A)) -> (union #f A)
  (define (check-singleton list)
    (cond ((null? list)
	   #f)
	  ((null? (cdr list))
	   (car list))
	  (else
	   (error 'check-singleton "not a singleton: ~v" list))))


  ;; Convert a graph to a string using the N-Triples format.
  ;; http://www.w3.org/TR/2004/REC-rdf-testcases-20040210/#ntriples
  ;; Graph -> String
  (define (graph->string graph)
    (apply string-append (map triple->string (graph-triples graph))))

  ;; Triple -> String
  (define (triple->string triple)
    (format "~a ~a ~a.~%"
	    (subject->string (triple-subject triple))
	    (predicate->string (triple-predicate triple))
	    (object->string (triple-object triple))))

  ;; Node -> String
  (define (subject->string subject)
    (if (blank-node? subject)
	(blank-node->string subject)
	(uriref->string subject)))

  ;; Predicate -> String
  (define (predicate->string predicate)
    (uriref->string predicate))

  ;; Node -> String
  (define (object->string object)
    (cond ((blank-node? object)
	   (blank-node->string object))
	  ((literal? object)
	   (literal->string object))
	  (else
	   (uriref->string object))))

  ;; String -> String
  (define (uriref->string uri)
    (string-append "<" uri ">"))


  ;; Create a unique label for a blank node.
  ;; Blank-Node -> String
  (define (blank-node->string blank-node)
    (symbol->string
     (or (hash-table-get *blank-node-codes* blank-node (lambda () #f))
	 (let ((code (gensym "_:bn")))
	   (hash-table-put! *blank-node-codes* blank-node code)
	   code))))

  (define *blank-node-codes* (make-hash-table 'weak))


  ;; Literal -> String
  (define (literal->string literal)
    (string-append
     "\"" (literal-lexical-form literal) "\""
     (cond ((plain-literal? literal)
	    (let ((lang (plain-literal-language-tag literal)))
	      (if (string-null? lang)
		  ""
		  (string-append "@" lang))))
	   ((typed-literal? literal)
	    (string-append "^^" (uriref->string
				 (typed-literal-datatype literal))))
	   (else
	    (error 'literal->string "unknown literal: ~a" literal)))))
)
