;;----------------------------------------------------------------------------
;; whitespace setting
;;----------------------------------------------------------------------------

;;; "M-SPC" => cycle-spacing
(global-set-key [remap just-one-space] #'cycle-spacing)


;;; Whitespace
(setq-default show-trailing-whitespace t)
(defun sanityinc/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook
                ttcn-3-mode-hook))
  (add-hook hook #'sanityinc/no-trailing-whitespace))


(require-package 'whitespace-cleanup-mode)
(add-hook 'after-init-hook #'global-whitespace-cleanup-mode)
(after-load 'whitespace-cleanup-mode
  (diminish 'whitespace-cleanup-mode))


(provide 'init-whitespace)
;;; init-whitespace.el ends here
