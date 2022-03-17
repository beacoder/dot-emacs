;;; init-grep.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default grep-highlight-matches t
              grep-scroll-output t)


(when *is-a-mac*
  (setq-default locate-command "mdfind"))


;; install the_silver_searcher(ag) first
(when (maybe-require-package 'ag)
  (require 'ag)
  (with-eval-after-load "ag"
    (progn
      (defalias #'ag/read-from-minibuffer #'smart/read-from-minibuffer)))

  ;; run ag
  ;; wgrep-change-to-wgrep-mode
  ;; make changes in buffer
  ;; wgrep-finish-edit
  (when  (maybe-require-package 'wgrep-ag)
    (setq wgrep-auto-save-buffer t
          wgrep-change-readonly-file t))

  (when (executable-find "ag")
    (setq-default ag-highlight-search t)
    (global-set-key (kbd "M-s") #'ag-project)
    (global-set-key (kbd "M-r") #'ag-project-regexp)))


;; install rg first
(when (and (executable-find "rg")
           (maybe-require-package 'rg))
  (global-set-key (kbd "M-?") #'rg-project))


(provide 'init-grep)
;;; init-grep.el ends here
