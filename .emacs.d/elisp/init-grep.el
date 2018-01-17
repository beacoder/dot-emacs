;;----------------------------------------------------------------------------
;; grep tool setting
;;----------------------------------------------------------------------------

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

;; install the_silver_searcher(ag) first
(when (and (executable-find "ag")
           (maybe-require-package 'ag))
  (require-package 'ag)
  (require-package 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "M-s") 'ag-project)
  (global-set-key (kbd "M-r") 'ag-project-regexp))

(after-load "ag"
  (progn
    (defalias #'ag/read-from-minibuffer #'smart/read-from-minibuffer)))


;; install rg first
(when (and (executable-find "rg")
           (maybe-require-package 'rg))
  (global-set-key (kbd "M-?") 'rg-project))


(provide 'init-grep)
