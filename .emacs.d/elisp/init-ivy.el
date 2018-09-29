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

  (when (maybe-require-package 'ag)
    (defun smart/counsel-ag (initial-input)
      "Search using `counsel-rg' from the project root for INITIAL-INPUT.
If there is no project root, search from the current directory instead.
With prefix args, read directory from minibuffer."
      (interactive (list (smart/dwim-at-point)))
      (let* ((dir
              (if current-prefix-arg
                  (read-directory-name "Execute `ag' in directory: ")
                (ag/project-root default-directory)))
             ;; eat current-prefix-arg before calling counsel-ag
             (current-prefix-arg))
        (counsel-ag initial-input dir)))))


(when (maybe-require-package 'swiper)
  (after-load 'ivy
    (defun smart/swiper-at-point (sym)
      "Use `swiper' to search for the symbol at point."
      (interactive (list (smart/read-from-minibuffer "Search string")))
      (swiper sym))))


(when (maybe-require-package 'ivy-xref)
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))


(provide 'init-ivy)
;;; init-ivy.el ends here
