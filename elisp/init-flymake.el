;;; init-flymake.el --- Configure Flymake global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'flymake "1.2.1")

;; Use flycheck checkers with flymake to extend its coverage
(when (maybe-require-package 'flymake-flycheck)
  ;; Disable flycheck checkers for which we have flymake equivalents
  (with-eval-after-load 'flycheck
    (setq-default
     flycheck-disabled-checkers
     (append (default-value 'flycheck-disabled-checkers)
             '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package sh-shellcheck))))

  (add-hook 'flymake-mode-hook 'flymake-flycheck-auto)
  (add-hook 'prog-mode-hook 'flymake-mode)
  (add-hook 'text-mode-hook 'flymake-mode))

(with-eval-after-load 'flymake
  ;; Provide some flycheck-like bindings in flymake mode to ease transition
  (define-key flymake-mode-map (kbd "C-c f") 'flymake-show-buffer-diagnostics)
  (define-key flymake-mode-map (kbd "C-c s") 'flymake-start)

  ;; Disable error prompts
  (defun flymake-error-no-popping (orig &rest args) (list))
  (advice-add 'flymake-error :around 'flymake-error-no-popping))


(use-package sideline-flymake
  :ensure t
  :diminish sideline-mode
  :hook (flymake-mode . sideline-mode)
  :init (setq sideline-flymake-display-mode 'point
              sideline-backends-right '(sideline-flymake)))


(provide 'init-flymake)
;;; init-flymake.el ends here
