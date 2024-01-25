;;; init-flymake.el --- Configure Flymake global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flymake
  :ensure t
  :diminish
  :functions my-elisp-flymake-byte-compile
  :bind ("C-c f" . flymake-show-buffer-diagnostics)
  :hook (prog-mode . flymake-mode)
  :init (setq flymake-no-changes-timeout nil
              flymake-fringe-indicator-position 'right-fringe)
  :config
  ;; Check elisp with ``load-path''
  (defun my-elisp-flymake-byte-compile (fn &rest args)
    ;; checkdoc-params: (fn args)
    "Wrapper for `elisp-flymake-byte-compile'."
    (let ((elisp-flymake-byte-compile-load-path
           (append elisp-flymake-byte-compile-load-path load-path)))
      (apply fn args)))
  (advice-add 'elisp-flymake-byte-compile :around #'my-elisp-flymake-byte-compile)
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
