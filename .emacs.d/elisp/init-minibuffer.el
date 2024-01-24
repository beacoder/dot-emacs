;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'vertico)
  (add-hook 'after-init-hook 'vertico-mode)

  ;; Optionally use the `orderless' completion style.
  (use-package orderless
    :ensure t
    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles basic partial-completion))))
    (orderless-component-separator #'orderless-escapable-split-on-space))

  (when (maybe-require-package 'embark)
    (with-eval-after-load 'vertico
      (define-key vertico-map (kbd "C-c C-o") 'embark-export)
      (define-key vertico-map (kbd "C-c C-c") 'embark-act)))

  (when (maybe-require-package 'consult)
    (defmacro sanityinc/no-consult-preview (&rest cmds)
      `(with-eval-after-load 'consult
         (consult-customize ,@cmds :preview-key "M-P")))

    (sanityinc/no-consult-preview
     consult-ripgrep
     consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-recent-file consult--source-project-recent-file consult--source-bookmark)

    (global-set-key [remap switch-to-buffer] 'consult-buffer)
    (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
    (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
    (global-set-key [remap goto-line] 'consult-goto-line)

    (when (maybe-require-package 'embark-consult)
      (with-eval-after-load 'embark
        (require 'embark-consult)
        (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)))

    (maybe-require-package 'consult-flycheck)))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))


(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
