;;----------------------------------------------------------------------------
;; Magit setting
;;----------------------------------------------------------------------------

;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
(require-package 'git-blamed)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'git-timemachine)


(when (maybe-require-package 'magit)
  (setq-default magit-diff-refine-hunk t)

  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up)
  (add-hook 'magit-popup-mode-hook 'sanityinc/no-trailing-whitespace)

  ;; change magit diff colors
  ;; (set-face-foreground 'magit-diff-add "green3")
  ;; (set-face-foreground 'magit-diff-del "red3")
  ;; (when (not window-system)
  ;;   (set-face-background 'magit-item-highlight "black"))

  (custom-set-faces
   '(magit-diff-added ((t (:background "black" :foreground "green3"))))
   '(magit-diff-removed ((t (:background "black" :foreground "red3"))))
   '(magit-diff-added-highlight ((t (:background "black" :foreground "green3"))))
   '(magit-diff-removed-highlight ((t (:background "black" :foreground "red3"))))))


(require-package 'fullframe)
(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(when (maybe-require-package 'git-commit)
  (add-hook 'git-commit-mode-hook 'goto-address-mode))


(when *is-a-mac*
  (after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))


;; Though see also vc-annotate's "n" & "p" bindings
(when (maybe-require-package 'git-messenger)
  (setq git-messenger:show-detail t)
  (global-set-key (kbd "C-x v p") #'git-messenger:popup-message))

(provide 'init-git)
