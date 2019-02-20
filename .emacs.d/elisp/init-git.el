;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
(require-package 'git-blamed)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)


;; "git-timemachine" => git-timemachine
;; "p"               => previous historic version
;; "n"               => next historic version
;; "g"               => nth revision
;; "q"               => quit time machine
(when (maybe-require-package 'git-timemachine)
  (global-set-key (kbd "C-x v t") 'git-timemachine-toggle))


;; "magit-blame-addition" => for each line show the revision in which it was added
;; "C-x v p"              => last commit for current thing-at-point
;; "C-x v l"              => git history of current file
;; "C-x v t"              => git timemachine
(when (maybe-require-package 'magit)
  (setq-default magit-diff-refine-hunk t)

  (global-set-key (kbd "C-x g") #'magit-status)
  (global-set-key (kbd "C-x M-g") #'magit-dispatch)

  (defun sanityinc/magit-or-vc-log-file (&optional prompt)
    (interactive "P")
    (if (and (buffer-file-name)
             (eq 'Git (vc-backend (buffer-file-name))))
        (if prompt
            (magit-log-buffer-file-popup)
          (magit-log-buffer-file t))
      (vc-print-log)))

  (after-load 'vc
    (define-key vc-prefix-map (kbd "l") 'sanityinc/magit-or-vc-log-file)))


(after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") #'magit-section-up)
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


(maybe-require-package 'magit-todos)


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
;;; init-git.el ends here
