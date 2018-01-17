;;----------------------------------------------------------------------------
;; python programming mode setting
;;----------------------------------------------------------------------------

;; "C-c C-p" =>  run-python
;; "C-c C-s" =>  python-shell-send-string
;; "C-c C-r" =>  python-shell-send-region
;; "C-c C-z" =>  python-shell-switch-to-shell
;; "C-c C-j" =>  imenu

;; "C-c <"   =>  python-indent-shift-left
;; "C-c >"   =>  python-indent-shift-right

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist)
      python-shell-interpreter "python2.7")

(require-package 'pip-requirements)

(when (maybe-require-package 'anaconda-mode)
  (after-load 'python
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
    (bind-keys
     :map python-mode-map
     ("C-c c" . python-skeleton-class)
     ("C-c d" . python-skeleton-def)
     ("C-c f" . python-skeleton-for)
     ("C-c i" . python-skeleton-if)
     ("C-c m" . python-skeleton-import)
     ("C-c t" . python-skeleton-try)
     ("C-c w" . python-skeleton-while)))

  (after-load 'anaconda-mode
    (bind-keys
     :map anaconda-mode-map
     ("M-?" . anaconda-mode-go-back)
     ("M-]" . anaconda-mode-find-references)
     ("M-=" . anaconda-mode-find-assignments)
     ("M-," . anaconda-mode-show-doc)))

  (when (maybe-require-package 'company-anaconda)
    (after-load 'company
      (add-hook 'python-mode-hook
                (lambda () (sanityinc/local-push-company-backend 'company-anaconda))))))


(provide 'init-python)
