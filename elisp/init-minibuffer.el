;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package consult-flycheck :ensure t)

;; vertical completion
(when (maybe-require-package 'vertico)
  (add-hook 'after-init-hook 'vertico-mode)

  (when (maybe-require-package 'embark)
    (with-eval-after-load 'vertico
      (define-key vertico-map (kbd "C-c C-o") 'embark-export)
      (define-key vertico-map (kbd "C-c C-c") 'embark-act)
      (define-key vertico-map (kbd "C-c C-l") 'embark-collect)))

  ;; https://github.com/purcell/whole-line-or-region/issues/30#issuecomment-3388095018
  (with-eval-after-load 'embark
    (push 'embark--mark-target
          (alist-get 'whole-line-or-region-delete-region
                     embark-around-action-hooks)))

  (when (maybe-require-package 'embark-consult)
    (require 'embark-consult)
    (defmacro sanityinc/no-consult-preview (&rest cmds)
      `(with-eval-after-load 'consult
         (consult-customize ,@cmds :preview-key "M-P")))

    (setq xref-show-xrefs-function 'consult-xref)

    (sanityinc/no-consult-preview
     consult-ripgrep
     consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult-source-recent-file consult-source-project-recent-file consult-source-bookmark)

    (defun sanityinc/consult-ripgrep-at-point (&optional dir initial)
      (interactive (list current-prefix-arg
                         (if (use-region-p)
                             (buffer-substring-no-properties
                              (region-beginning) (region-end))
                           (if-let* ((s (symbol-at-point)))
                               (symbol-name s)))))
      (consult-ripgrep dir initial))
    (sanityinc/no-consult-preview sanityinc/consult-ripgrep-at-point)
    (when (executable-find "rg")
      (global-set-key (kbd "M-?") 'sanityinc/consult-ripgrep-at-point))

    (global-set-key [remap switch-to-buffer] 'consult-buffer)
    (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
    (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
    (global-set-key [remap goto-line] 'consult-goto-line)))

;; show minibuffer command docs
(when (maybe-require-package 'marginalia)
  (add-hook 'after-init-hook 'marginalia-mode))


(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
