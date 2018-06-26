;;----------------------------------------------------------------------------
;; c/c++ programming mode setting
;;----------------------------------------------------------------------------

;;; useful key-bindings
;;----------------------------------------------------------------------------
;;; Find tag
;;  ggtags-find-tag-dwim
;;  ggtags-find-reference
;;
;;; Grep tag
;;  ag-project
;;  counsel-git-grep
;;  ggtags-grep

;;; Find file
;;  counsel-git
;;  ggtags-find-file
;;  counsel-locate

;;; useful commands
;;----------------------------------------------------------------------------
;;; Keep #includes in alphabetical order
;;  sort-lines

;; avoid default "gnu" style, use more popular one
(setq c-default-style "linux")

(defun fix-c-indent-offset-according-to-syntax-context (key val)
  ;; remove the old element
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist '(key . val)))

;; personal settings
(defun my-c-mode-common-hook ()
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)
  ;; @see http://xugx2007.blogspot.com.au/2007/06/benjamin-rutts-emacs-c-development-tips.html
  (setq compilation-window-height 8)
  (setq compilation-finish-function
        (lambda (buf str)
          (if (string-match "exited abnormally" str)
              ;;there were errors
              (message "compilation errors, press C-x ` to visit")
            ;;no errors, make the compilation window go away in 0.5 seconds
            (when (string-match "*compilation*" (buffer-name buf))
              ;; @see http://emacswiki.org/emacs/ModeCompile#toc2
              (bury-buffer "*compilation*")
              (winner-undo)
              (message "NO COMPILATION ERRORS!")))))

  ;; other customizations
  (setq c-basic-offset 4)
  (setq tab-width 8)
  ;; show function name in mode-line
  (which-function-mode t)
  ;; enable flyspell for comments in source code
  ;; (flyspell-prog-mode)
  ;; improve performance
  (setq flyspell-issue-message-flag nil)

  ;; navigation between header and cpp/cc files
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (local-set-key (kbd "C-M-a") 'c-beginning-of-defun)
  (local-set-key (kbd "C-M-e") 'c-end-of-defun)

  ;; we like auto-newline and hungry-delete
  (c-toggle-auto-hungry-state 1)
  (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
  (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0)

  ;; allow global binding to work when c/c++-mode is active
  (define-key c++-mode-map (kbd "C-c C-e") nil)
  (define-key c++-mode-map (kbd "C-c C-s") nil)
  (define-key c-mode-map (kbd "C-c C-e") nil)
  (define-key c-mode-map (kbd "C-c C-s") nil))

;; use <tab> to indent region if anything is selected
;; fledermaus came up with this
(defun fledermaus-maybe-tab ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (indent-region (region-beginning) (region-end) nil)
    (c-indent-command)))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-common-hook (lambda () (local-set-key [(tab)] 'fledermaus-maybe-tab)))


(when (is-modern-emacs)
  ;; google-c-style
  (require-package 'google-c-style)
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)

  ;; company-c-headers
  (when (maybe-require-package 'company-c-headers)
    (after-load 'company
      (add-hook 'c-mode-common-hook
                (lambda () (sanityinc/local-push-company-backend 'company-c-headers)))))


  ;; flycheck
  (add-hook 'c++-mode-hook
            (lambda () (setq flycheck-gcc-include-path
                        '("."
                          "../include/*"
                          "../src/*"
                          "/usr/include"
                          "/usr/local/include/*"
                          "/usr/include/c++/4.8.5/*"
                          "/usr/include/boost/*")))))


;; @see https://stackoverflow.com/questions/7299893/getting-rid-of-buffer-has-running-process-confirmation-when-the-process-is-a-f
;; Stop asking me all the time when killing the buffer
(defadvice flymake-start-syntax-check-process (after
                                               cheeso-advice-flymake-start-syntax-check-1
                                               (cmd args dir)
                                               activate compile)
  ;; set flag to allow exit without query on any
  ;;active flymake processes
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
        (while (re-search-forward "^ *#include +.*$" nil t)
          (beginning-of-line)
          (setq beg-region (point))
          (while (and (< (point) (point-max)) ;; Not end of buffer
                      (or (string-match-p "^ *#include +.*$" (thing-at-point 'line)) ;; Match includes
                          (string-match-p "^\s-*$" (thing-at-point 'line)))) ;; Match blank line
            (end-of-line)
            (setq end-region (point))
            (forward-line))
          (sort-lines nil beg-region end-region))))))


(provide 'init-cc)
