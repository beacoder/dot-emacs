;;; init-ivy.el --- Use ivy for minibuffer completion and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; ivy - a generic completion frontend for Emacs
;; "C-m"   => calls the current action
;; "M-o"   => show actions, calls action after selection, exit
;; "C-M-o" => show actions, calls action after selection, don't exit
(when (maybe-require-package 'ivy)
  (add-hook 'after-init-hook 'ivy-mode)
  (after-load 'ivy
    (setq-default ivy-height 20
                  ivy-use-virtual-buffers t
                  ivy-virtual-abbreviate 'fullpath
                  ivy-count-format ""
                  projectile-completion-system 'ivy
                  ;; cause trouble when doing counsel-git-grep
                  ;; ivy-dynamic-exhibit-delay-ms 150
                  ivy-use-selectable-prompt t
                  ivy-magic-tilde nil
                  ivy-initial-inputs-alist
                  '((man . "^")
                    (woman . "^")))

    ;; IDO-style directory navigation
    (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
    (dolist (k '("C-j" "C-RET"))
      (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))

    (define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)

    ;; show more results in counsel-ag
    ;; (add-to-list 'ivy-height-alist (cons 'counsel-ag 20))
    )

  (defun sanityinc/enable-ivy-flx-matching ()
    "Make `ivy' matching work more like IDO."
    (interactive)
    (require-package 'flx)
    (setq-default ivy-re-builders-alist
                  '((t . ivy--regex-fuzzy)))))


(when (maybe-require-package 'counsel)
  (after-load 'counsel
    ;; don't override pop-to-mark-command
    (define-key counsel-mode-map [remap pop-to-mark-command] nil))
  (setq-default counsel-mode-override-describe-bindings t)
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


;; @see https://github.com/abo-abo/swiper/blob/master/ivy-hydra.el
(defvar ivy-dispatching-done-columns 2
  "Number of columns to use if the hint does not fit on one line.")

(defun ivy-dispatching-done-hydra ()
  "Select one of the available actions and call `ivy-done'."
  (interactive)
  (let* ((actions (ivy-state-action ivy-last))
         (estimated-len (+ 25 (length
                               (mapconcat
                                (lambda (x) (format "[%s] %s" (nth 0 x) (nth 2 x)))
                                (cdr actions) ", "))))
         (n-columns (if (> estimated-len (window-width))
                        ivy-dispatching-done-columns
                      nil)))
    (if (null (ivy--actionp actions))
        (ivy-done)
      (funcall
       (eval
        `(defhydra ivy-read-action (:color teal :columns ,n-columns)
           "action"
           ,@(mapcar (lambda (x)
                       (list (nth 0 x)
                             `(progn
                                (ivy-set-action ',(nth 1 x))
                                (ivy-done))
                             (nth 2 x)))
                     (cdr actions))
           ("M-o" nil "back")
           ("C-g" nil)))))))
(define-key ivy-minibuffer-map (kbd "M-o") 'ivy-dispatching-done-hydra)


(provide 'init-ivy)
;;; init-ivy.el ends here
