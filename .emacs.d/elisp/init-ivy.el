;;----------------------------------------------------------------------------
;; ivy configuration
;;----------------------------------------------------------------------------

(when (maybe-require-package 'ivy)
  (add-hook 'after-init-hook 'ivy-mode)
  (after-load 'ivy
    (setq-default ivy-use-virtual-buffers t
                  ivy-virtual-abbreviate 'fullpath
                  ivy-count-format ""
                  projectile-completion-system 'ivy
                  ;; cause trouble when doing counsel-git-grep
                  ;; ivy-dynamic-exhibit-delay-ms 150
                  ivy-magic-tilde nil
                  ivy-initial-inputs-alist
                  '((man . "^")
                    (woman . "^")))

    ;; IDO-style directory navigation
    (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
    (dolist (k '("C-j" "C-RET"))
      (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))

    (define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)

    (when (maybe-require-package 'diminish)
      (diminish 'ivy-mode)))

  (defun sanityinc/enable-ivy-flx-matching ()
    "Make `ivy' matching work more like IDO."
    (interactive)
    (require-package 'flx)
    (setq-default ivy-re-builders-alist
                  '((t . ivy--regex-fuzzy)))))


(when (maybe-require-package 'ivy-historian)
  (add-hook 'after-init-hook (lambda () (ivy-historian-mode t))))

(when (maybe-require-package 'counsel)
  (after-load 'counsel
    ;; don't override pop-to-mark-command
    (define-key counsel-mode-map [remap pop-to-mark-command] nil))
  (setq-default counsel-mode-override-describe-bindings t)
  (when (maybe-require-package 'diminish)
    (after-load 'counsel
      (diminish 'counsel-mode)))
  (add-hook 'after-init-hook 'counsel-mode)
  (when (and (executable-find "ag") (maybe-require-package 'projectile))
    (defun smart/counsel-ag-project (initial-input)
      "Search using `counsel-ag' from the project root for INITIAL-INPUT."
      (interactive (list (smart/read-from-minibuffer "Search string")))
      (counsel-ag initial-input (condition-case err
                                    (projectile-project-root)
                                  (error default-directory))))))


(when (maybe-require-package 'swiper)
  (after-load 'ivy
    (defun smart/swiper-at-point (sym)
      "Use `swiper' to search for the symbol at point."
      (interactive (list (smart/read-from-minibuffer "Search string")))
      (swiper sym))))


(when (maybe-require-package 'ivy-xref)
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))


(provide 'init-ivy)
