(module votes mzscheme
  (require (rename (lib "list.ss") mergesort mergesort))
  (require "canon.ss")
  (require "entries.ss")		;compare-entries
  (provide (all-defined))

  (define-struct vote (record entry))

  (define (compare-votes v1 v2)
    (compare-entries (vote-entry v1) (vote-entry v2)))

  ;; Tables mapping canonicalized records to lists of votes/mentions.
  (define *crecord->votes* (make-hash-table))
  (define *crecord->mentions* (make-hash-table))

  (define (crecord-votes crecord)
    (hash-table-get *crecord->votes* crecord (lambda () null)))
  (define (crecord-mentions crecord)
    (hash-table-get *crecord->mentions* crecord (lambda () null)))

  (define (add-vote record entry vote?)
    (add-vote!
     (if vote? *crecord->votes* *crecord->mentions*)
     (canonicalize record)
     (make-vote record entry)))

  (define (add-vote! table crecord vote)
    (hash-table-put!
     table crecord
     (cons vote (hash-table-get table crecord (lambda () null)))))

  ;; The list of canonical record structs, sorted by votes.
  (define (crecords-by-votes)
    (mergesort (crecords) crecord-more-votes?))

  ;; Does c1 have more votes than c2?  Ties are broken by number of
  ;; mentions, and then lexicographically.
  (define (crecord-more-votes? c1 c2)
    (crecord-compare-votes
     c1 c2
     kfalse
     (lambda args (crecord-compare c1 c2 ktrue kfalse kfalse))
     ktrue))

  (define (crecord-compare-votes c1 c2 k< k= k>)
    (let ((v1 (length (crecord-votes c1)))
	  (v2 (length (crecord-votes c2))))
      (cond ((< v1 v2)
	     (k<))
	    ((> v1 v2)
	     (k>))
	    (else
	     (crecord-compare-mentions c1 c2 k< k= k>)))))

  (define (crecord-compare-mentions c1 c2 k< k= k>)
    (let ((m1 (length (crecord-mentions c1)))
	  (m2 (length (crecord-mentions c2))))
      (cond ((< m1 m2)
	     (k<))
	    ((> m1 m2)
	     (k>))
	    (else
	     (k=)))))

  (define *ranking* #f)
  (define (crecord-rank crecord)
    (hash-table-get *ranking* crecord))
  (define (set-crecord-rank! crecord rank)
    (hash-table-put! *ranking* crecord rank))

  (define (build-ranking)
    (set! *ranking* (make-hash-table))
    (let loop ((crs (crecords-by-votes)) (i 1)
	       (last-r 1) (last-v 0) (last-m 0))
      (unless (null? crs)
	(let* ((cr (car crs))
	       (v (length (crecord-votes cr)))
	       (m (length (crecord-mentions cr)))
	       (r (if (and (= v last-v) (= m last-m)) last-r i)))
	  (when (zero? (+ v m))
	    (remove-crecord cr))
	  (set-crecord-rank! cr r)
	  (loop (cdr crs) (add1 i) r v m)))))
)
