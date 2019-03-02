;;----------------------------------------------------------------------------
;; company-mode setting
;;----------------------------------------------------------------------------

(add-to-list 'completion-styles 'initials t)
(setq tab-always-indent 'complete
      completion-cycle-threshold 5
      company-show-numbers t)


(when (maybe-require-package 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  (after-load 'company
      (dolist (backend '(company-eclim company-semantic company-clang)) ;; Tags over Clang for efficiency
        (delq backend company-backends))
      (setq-default company-dabbrev-other-buffers 'all
                    company-tooltip-align-annotations t))
  (when (maybe-require-package 'company-quickhelp)
    (add-hook 'after-init-hook 'company-quickhelp-mode))
  (global-set-key (kbd "\C-c TAB") #'company-complete))


;; Suspend page-break-lines-mode while company menu is active
;; (see https://github.com/company-mode/company-mode/issues/416)
(after-load 'company
  (after-load 'page-break-lines
    (defvar-local sanityinc/page-break-lines-on-p nil)

    (defun sanityinc/page-break-lines-disable (&rest ignore)
      (when (setq sanityinc/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
        (page-break-lines-mode -1)))

    (defun sanityinc/page-break-lines-maybe-reenable (&rest ignore)
      (when sanityinc/page-break-lines-on-p
        (page-break-lines-mode 1)))

    (add-hook 'company-completion-started-hook 'sanityinc/page-break-lines-disable)
    (add-hook 'company-after-completion-hook 'sanityinc/page-break-lines-maybe-reenable)))


(provide 'init-company)
;;; init-company.el ends here
