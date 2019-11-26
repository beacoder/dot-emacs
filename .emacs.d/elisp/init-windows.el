;;; init-windows.el --- Working with windows within frames -*- lexical-binding: t -*-
;;; Commentary:

;; This is not about the "Windows" OS, but rather Emacs's "windows"
;; concept: these are the panels within an Emacs frame which contain
;; buffers.

;;; Code:

;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;---------------------------------------------------------------------------
(add-hook 'after-init-hook 'winner-mode)


;;----------------------------------------------------------------------------
;; Make "C-x o" prompt for a target window when there are more than 2
;;----------------------------------------------------------------------------
(require-package 'switch-window)
(setq-default switch-window-shortcut-style 'alphabet
              switch-window-timeout nil)
(global-set-key (kbd "C-x o") #'switch-window)


;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") #'sanityinc/toggle-delete-other-windows)


;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x |") #'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") #'split-window-vertically-instead)


;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(defun sanityinc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))


;; @see https://www.emacswiki.org/emacs/TransposeWindows
(defun transpose-windows ()
  "Transpose two windows.  If more or less than two windows are visible, error."
  (interactive)
  (lv-delete-window) ; don't let hydra-window get in the way
  (unless (= 2 (count-windows))
    (error "There are not 2 windows"))
  (let* ((windows (window-list))
         (w1 (car windows))
         (w2 (nth 1 windows))
         (w1b (window-buffer w1))
         (w2b (window-buffer w2)))
    (set-window-buffer w1 w2b)
    (set-window-buffer w2 w1b)))


(unless (memq window-system '(nt w32))
  (windmove-default-keybindings 'control))


;;----------------------------------------------------------------------------
;; Enlarge/Shrink windows vertically
;;----------------------------------------------------------------------------
(unless (fboundp 'enlarge-window-vertically)
  (defun enlarge-window-vertically (delta)
    "Make selected window DELTA columns wider.
Interactively, if no argument is given, make selected window one
column wider."
    (interactive "p")
    (enlarge-window delta nil)))

(unless (fboundp 'shrink-window-vertically)
  (defun shrink-window-vertically (delta)
    "Make selected window DELTA columns narrower.
Interactively, if no argument is given, make selected window one
column narrower."
    (interactive "p")
    (shrink-window delta nil)))


;;----------------------------------------------------------------------------
;; Enforce rules for popups
;;----------------------------------------------------------------------------
(when (maybe-require-package 'shackle)
  (defvar shackle--popup-window-list nil) ; all popup windows
  (defvar-local shackle--current-popup-window nil) ; current popup window
  (put 'shackle--current-popup-window 'permanent-local t)

  (use-package shackle
    :functions org-switch-to-buffer-other-window
    :commands shackle-display-buffer
    :hook (after-init . shackle-mode)
    :config
    (eval-and-compile
      (defun shackle-last-popup-buffer ()
        "View last popup buffer."
        (interactive)
        (ignore-errors
          (display-buffer shackle-last-buffer)))
      (bind-key "C-h z" #'shackle-last-popup-buffer)

      ;; Add keyword: `autoclose'
      (defun shackle-display-buffer-hack (fn buffer alist plist)
        (let ((window (funcall fn buffer alist plist)))
          (setq shackle--current-popup-window window)

          (when (plist-get plist :autoclose)
            (push (cons window buffer) shackle--popup-window-list))
          window))

      (defun shackle-close-popup-window-hack (&rest _)
        "Close current popup window via `C-g'."
        (setq shackle--popup-window-list
              (cl-loop for (window . buffer) in shackle--popup-window-list
                       if (and (window-live-p window)
                               (equal (window-buffer window) buffer))
                       collect (cons window buffer)))
        ;; `C-g' can deactivate region
        (when (and (called-interactively-p 'interactive)
                   (not (region-active-p)))
          (let (window buffer)
            (if (one-window-p)
                (progn
                  (setq window (selected-window))
                  (when (equal (buffer-local-value 'shackle--current-popup-window
                                                   (window-buffer window))
                               window)
                    (winner-undo)))
              (setq window (caar shackle--popup-window-list))
              (setq buffer (cdar shackle--popup-window-list))
              (when (and (window-live-p window)
                         (equal (window-buffer window) buffer))
                (delete-window window)

                (pop shackle--popup-window-list))))))

      (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
      (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack))

    ;; HACK: compatibility issuw with `org-switch-to-buffer-other-window'
    (advice-add #'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window)

    ;; rules
    (setq shackle-default-size 0.4
          shackle-default-alignment 'below
          shackle-default-rule nil
          shackle-rules
          '(("*Help*" :select t :size 0.3 :align 'below :autoclose t)
            ("*Apropos*" :select t :size 0.3 :align 'below :autoclose t)
            ("*compilation*" :select t :size 0.3 :align 'below :autoclose t)
            ("*Compile-Log*" :select t :size 0.3 :align 'below :autoclose t)
            ("*Completions*" :size 0.3 :align 'below :autoclose t)
            ("*Pp Eval Output*" :size 15 :align 'below :autoclose t)
            ("*ert*" :align 'below :autoclose t)
            ("*Backtrace*" :select t :size 15 :align 'below)
            ("*Warnings*" :size 0.3 :align 'below :autoclose t)
            ("*Messages*" :size 0.3 :align 'below :autoclose t)
            ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
            ("\\*[Wo]*Man.*\\*" :regexp t :select t :align 'below :autoclose t)
            (("*shell*" "*eshell*" "*ielm*") :popup t :align 'below)
            ("*Calendar*" :select t :size 0.3 :align 'below)
            ("\\*ivy-occur .*\\*" :regexp t :select t :align 'below)
            (" *undo-tree*" :select t)
            ("*quickrun*" :select t :size 15 :align 'below)
            ("*tldr*" :align 'below :autoclose t)
            ("*Youdao Dictionary*" :size 0.3 :align 'below :autoclose t)
            ("*Finder*" :select t :size 0.3 :align 'below :autoclose t)
            ("^\\*elfeed-entry" :regexp t :size 0.7 :align 'below :autoclose t)
            ("*lsp-help*" :size 0.3 :align 'below :autoclose t)
            ("*lsp session*" :align 'below :autoclose t)
            (" *Org todo*" :select t :align 'below :autoclose t)
            ("*Org Dashboard*" :select t :align 'below :autoclose t)
            ("*gud-debug*" :select t :align 'below :autoclose t)
            ("^\\*macro expansion\\**" :regexp t :size 0.4 :align 'below)
            (" *Install vterm" :size 0.3 :align 'below)
            ("*Paradox Report*" :size 0.2 :align 'below :autoclose t)
            ("*package update results*" :size 0.2 :align 'below :autoclose t)
            ("*Package-Lint*" :align 'below :autoclose t)
            ("*Gofmt Errors*" :select t :size 0.3 :align 'below :autoclose t)
            ("*Go Test*" :select t :size 0.3 :align 'below :autoclose t)

            (ag-mode :select t :align 'below)
            (grep-mode :select t :align 'below)
            (pt-mode :select t :align 'below)
            (rg-mode :select t :align 'below)

            (flycheck-error-list-mode :select t :size 0.3 :align 'below :autoclose t)
            (flymake-diagnostics-buffer-mode :select t :size 0.3 :align 'below :autoclose t)

            (comint-mode :align 'below)
            (inferior-python-mode :align 'below)
            (inf-ruby-mode :align 'below)
            (swift-repl-mode :align 'below)
            ("*prolog*" :align 'below)

            (Buffer-menu-mode :select t :size 20 :align 'below :autoclose t)
            (helpful-mode :select t :size 0.3 :align 'below :autoclose t)
            (process-menu-mode :select t :size 0.3 :align 'below :autoclose t)
            (cargo-process-mode :select t :size 0.3 :align 'below :autoclose t)
            (list-environment-mode :select t :size 0.3 :align 'below :autoclose t)
            (profiler-report-mode :select t :size 0.5 :align 'below)
            (tabulated-list-mode :align 'below)))))


(provide 'init-windows)
;;; init-windows.el ends here
