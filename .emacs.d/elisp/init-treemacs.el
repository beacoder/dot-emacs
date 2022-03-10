;; init-treemacs.el --- Initialize treemacs.    -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Treemacs: A tree layout file explorer.
;;
;;;
;;; Code:

(when (and (>= emacs-major-version 26)
           (maybe-require-package 'treemacs))
  ;; A tree layout file explorer
  (use-package treemacs
    :commands (treemacs-follow-mode
               treemacs-filewatch-mode
               treemacs-fringe-indicator-mode
               treemacs-git-mode)
    :custom-face
    (cfrs-border-color ((t (:background ,(face-foreground 'font-lock-comment-face nil t)))))
    :bind (("C-x t t" . treemacs)
           ("C-x t s" . treemacs-select-window)
           ("C-x t d" . treemacs-delete-other-windows)
           ("C-x t o" . treemacs-delete-other-windows)
           ("C-x t b" . treemacs-bookmark)
           ("C-x t f" . treemacs-find-file)
           ("C-x t g" . treemacs-find-tag)
           :map treemacs-mode-map
           ([mouse-1]   . treemacs-single-click-expand-action))
    :config
    (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
          treemacs-missing-project-action  'remove
          treemacs-sorting                 'alphabetic-asc
          treemacs-follow-after-init       t
          treemacs-width                   30
          ;; treemacs-no-png-images           (not centaur-icon)
          )
    :config
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (when (maybe-require-package 'treemacs-projectile)
      (use-package treemacs-projectile
        :after projectile
        :bind (:map projectile-command-map
                    ("h" . treemacs-projectile))))

    (when (maybe-require-package 'treemacs-magit)
      (use-package treemacs-magit
        :after magit
        :commands treemacs-magit--schedule-update
        :hook ((magit-post-commit
                git-commit-post-finish
                magit-post-stage
                magit-post-unstage)
               . treemacs-magit--schedule-update)))

    (when (maybe-require-package 'treemacs-persp)
      (use-package treemacs-persp
        :after persp-mode
        :demand
        :functions treemacs-set-scope-type
        :config (treemacs-set-scope-type 'Perspectives)))))


(provide 'init-treemacs)
;;; init-treemacs.el ends here
