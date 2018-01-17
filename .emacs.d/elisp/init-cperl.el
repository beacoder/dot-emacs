;; cperl mode setting

;; @see http://www.emacswiki.org/emacs/CPerlMode
;; "C-c C-h p" => show perl doc at point
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))

(provide 'init-cperl)
