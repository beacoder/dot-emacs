;;----------------------------------------------------------------------------
;; flycheck configuration
;;----------------------------------------------------------------------------

;;; useful key-bindings
;;----------------------------------------------------------------------------
;; "C-c ! v" => flycheck-verify-setup
;; "C-c ! l" => flycheck-list-errors

(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  ;; Enable C++11 support for gcc
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

  ;; Disable clang check, gcc check works better
  (setq-default flycheck-disabled-checkers '(c/c++-clang))

  (when (maybe-require-package 'flycheck-color-mode-line)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))


(provide 'init-flycheck)
