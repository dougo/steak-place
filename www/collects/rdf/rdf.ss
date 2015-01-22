;; Resource Description Framework (RDF) --  http://www.w3.org/RDF/

(module rdf mzscheme
  (require "vocab.ss")
  (provide (all-from "vocab.ss"))

  (require "structs.ss")
  (provide (all-from "structs.ss"))

  (require "xml.ss")
  (provide (all-from "xml.ss"))
)
