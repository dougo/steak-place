(module poll mzscheme
  (require (lib "list.ss" "srfi" "1"))	;split-at
  (require "entries.ss")		;*entries*, entry-records
  (require "votes.ss")			;add-vote, build-ranking
  (require "canon-persist.ss")		;save-canon
  (require "output.ss")			;write-results
  (require "utils.ss")			;index-for-each
  (provide (all-defined))

  (define (main)
    (tabulate-entries)
    (save-canon)
    (write-results))

  (define (tabulate-entries)
    (for-each tabulate-entry *entries*)
    (build-ranking))

  (define (tabulate-entry entry)
    (let-values (((votes mentions)
		  (safe-split-at (entry-records entry) 20)))
      (for-each (lambda (record) (add-vote record entry #t)) votes)
      (for-each (lambda (record) (add-vote record entry #f)) mentions)))

  ;; Like split-at, but allow i >= len.
  (define (safe-split-at l i)
    (if (<= i (length l))
	(split-at l i)
	(values l null)))
)
