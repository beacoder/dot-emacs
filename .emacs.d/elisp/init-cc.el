;;; init-cc.el --- C/C++ mode config -*- lexical-binding: t -*-
;;; Commentary:
;;
;; C/C++ programming mode setting
;;

;;; Code:

;;; useful key-bindings
;;----------------------------------------------------------------------------
;;; Find tag
;;  ggtags-find-tag-dwim  (M-.)
;;  ggtags-find-reference (M-])
;;
;;; Grep tag
;;  ag-project            (M-s|M-r)
;;  counsel-ag            (C-x q a)
;;  counsel-git-grep      (C-x q g)
;;  ggtags-grep           (C-c M-g)

;;; Find file
;;  counsel-git           (C-x q f)
;;  ggtags-find-file      (C-c M-f)
;;  counsel-locate        (C-x q l)

;;; useful commands
;;----------------------------------------------------------------------------
;;  sort-lines             (C-x q s)
;;  delete-duplicate-lines (C-x q d)

;; avoid default "gnu" style, use more popular one
(setq c-default-style "linux")

(defun fix-c-indent-offset-according-to-syntax-context (key val)
  "Fix indent offset according to KEY and VAL."
  ;; remove the old element
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist '(key . val)))

(defun my-refresh-imenu ()
  "Refresh imenu items."
  (when (derived-mode-p 'prog-mode)
    (setq imenu--index-alist nil)))

;; personal settings
(defun my-c-mode-common-hook ()
  "My c mode common hook."
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)
  ;; other customizations
  (setq c-basic-offset 4
        c-ts-mode-indent-offset 4
        tab-width 8)
  ;; show function name in mode-line
  (which-function-mode t)
  ;; enable flyspell for comments in source code
  ;; (flyspell-prog-mode)
  ;; improve performance
  (setq flyspell-issue-message-flag nil)

  ;; navigation between header and cpp/cc files
  (local-set-key (kbd "C-c o") #'ff-find-other-file)

  ;; we like auto-newline and hungry-delete
  (c-toggle-auto-hungry-state 1)
  (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
  (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0)

  ;; auto-refresh imenu items after auto-revert
  (remove-hook 'after-revert-hook 'my-refresh-imenu t)
  (add-hook 'after-revert-hook 'my-refresh-imenu nil t)

  ;; allow global binding to work when c/c++-mode is active
  (define-key c++-mode-map (kbd "C-c C-e") nil)
  (define-key c++-mode-map (kbd "C-c C-s") nil)
  (define-key c-mode-map (kbd "C-c C-e") nil)
  (define-key c-mode-map (kbd "C-c C-s") nil))

(dolist (c-mode-hook '(c-mode-common-hook c-ts-mode-hook c++-ts-mode-hook))
  (add-hook c-mode-hook #'my-c-mode-common-hook)
  (add-hook c-mode-hook #'hs-minor-mode))

(when (is-modern-emacs)
  ;; google-c-style
  (require-package 'google-c-style)
  (add-hook 'c-mode-common-hook #'google-set-c-style)
  (add-hook 'c-mode-common-hook #'google-make-newline-indent))


;; @see https://stackoverflow.com/questions/7299893/getting-rid-of-buffer-has-running-process-confirmation-when-the-process-is-a-f
;; stop asking me all the time.
(defadvice flymake-start-syntax-check-process (after
                                               cheeso-advice-flymake-start-syntax-check-1
                                               (cmd args dir)
                                               activate compile)
  "Set flag to allow exit without query on any active flymake processes."
  (set-process-query-on-exit-flag ad-return-value nil))


;; remove useless whitespaces before saving a file
;; (add-hook 'before-save-hook
;;           (lambda()
;;             (when (member major-mode '(c-mode c++-mode))
;;               (whitespace-cleanup)
;;               (delete-trailing-whitespace))))


;;; custom refactoring
;;----------------------------------------------------------------------------
(defun sort-include-files ()
  "Sort include files in alphabetically order."
  (interactive)
  (save-match-data
    (save-excursion
      (let* ((beg-region nil) (end-region nil))
        (goto-char (point-min))
        (while (re-search-forward "^\\s-*#include\\s-+.*$" nil t)
          (beginning-of-line)
          (setq beg-region (point))
          (while (and (< (point) (point-max)) ;; Not end of buffer
                      (or (string-match-p "^\\s-*#include\\s-+.*$" (thing-at-point 'line)) ;; Match includes
                          (= 0 (string-match-p "^\\s-*$" (thing-at-point 'line))))) ;; Match blank line
            (end-of-line)
            (setq end-region (point))
            (forward-line))
          (sort-lines nil beg-region end-region))))))


(provide 'init-cc)
;;; init-cc.el ends here
