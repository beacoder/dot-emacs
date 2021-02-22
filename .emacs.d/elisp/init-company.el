;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'completion-styles 'initials t)
(setq tab-always-indent 'complete
      completion-cycle-threshold 5
      company-show-numbers t)


(when (maybe-require-package 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  (with-eval-after-load 'company
      (dolist (backend '(company-eclim company-semantic company-clang)) ;; Tags over Clang for efficiency
        (delq backend company-backends))
      (setq-default company-dabbrev-other-buffers 'all
                    company-tooltip-align-annotations t))
  (when (maybe-require-package 'company-quickhelp)
    (add-hook 'after-init-hook 'company-quickhelp-mode))
  (global-set-key (kbd "\C-c TAB") #'company-complete))


(provide 'init-company)
;;; init-company.el ends here
