;;----------------------------------------------------------------------------
;; enhance hippie-expand to support tag files
;;----------------------------------------------------------------------------

(require 'etags)

(global-set-key (kbd "M-/") 'hippie-expand)

;; let hippie-expand support tag files
;; shamelessly copied from http://emacswiki.org/emacs/HippieExpand
(when (> emacs-major-version 21)
  (progn
    (defun tags-complete-tag (string predicate what)
      (save-excursion
	;; If we need to ask for the tag table, allow that.
	(if (eq what t)
	    (all-completions string (tags-completion-table) predicate)
	  (try-completion string (tags-completion-table) predicate))))))

;; This is a simple function to return the point at the beginning of the symbol to be completed
(defun he-tag-beg ()
  (let ((p
         (save-excursion
           (backward-word 1)
           (point))))
    p))

;; The actual expansion function
(defun try-expand-tag (old)
  "use tag files to expand"
  ;; old is true if we have already attempted an expansion
  (unless old
    ;; he-init-string is used to capture the string we are trying to complete
    (he-init-string (he-tag-beg) (point))
    ;; he-expand list is the list of possible expansions
    (setq he-expand-list (sort
                          (all-completions he-search-string 'tags-complete-tag) 'string-lessp)))
  ;; now we go through the list, looking for an expansion that isn't in the table of previously
  ;; tried expansions
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  ;; if we didn't have any expansions left, reset the expansion list
  (if (null he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
    ;; otherwise offer the expansion at the head of the list
    (he-substitute-string (car he-expand-list))
    ;; and put that expansion into the tried expansions list
    (setq he-expand-list (cdr he-expand-list))
    t))

;; Done, now we just use it as a clause in our make-hippie-expand-function (as above)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-tag
	ggtags-try-complete-tag
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))

(provide 'init-hippie-expand)
