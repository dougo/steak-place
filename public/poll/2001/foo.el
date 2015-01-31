(defun mark-record-region ()
  (interactive)
  (prefix-region "$"))

(defun mark-comment-region ()
  (interactive)
  (prefix-region "#"))

(global-set-key "\C-x-$" 'mark-record-region)
(global-set-key "\C-#" 'mark-comment-region)
