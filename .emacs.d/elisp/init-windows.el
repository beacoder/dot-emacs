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
(when (maybe-require-package 'popper)
  (use-package popper
    :defines popper-echo-dispatch-actions
    :commands popper-group-by-projectile
    :bind (:map popper-mode-map
                ("C-h l" . popper-toggle-latest)
                ("C-h c" . popper-cycle)
                ("C-h t" . popper-toggle-type))
    :hook (after-init . popper-mode)
    :init
    (setq popper-reference-buffers
          '("\\*Messages\\*"
            "Output\\*$" "\\*Pp Eval Output\\*$"
            "\\*Completions\\*"
            "\\*Warnings\\*"
            "\\*Async Shell Command\\*"
            "\\*Apropos\\*"
            "\\*Backtrace\\*"
            ;; "\\*Calendar\\*"              ; FIXME: https://github.com/karthink/popper/issues/29

            bookmark-bmenu-mode
            compilation-mode
            help-mode helpful-mode
            tabulated-list-mode
            Buffer-menu-mode

            gnus-article-mode devdocs-mode
            grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
            ivy-occur-mode ivy-occur-grep-mode
            process-menu-mode list-environment-mode cargo-process-mode
            youdao-dictionary-mode osx-dictionary-mode fanyi-mode

            "^\\*eshell.*\\*$" eshell-mode
            "^\\*shell.*\\*$"  shell-mode
            "^\\*term.*\\*$"   term-mode
            "^\\*vterm.*\\*$"  vterm-mode

            "\\*DAP Templates\\*$" dap-server-log-mode
            "\\*ELP Profiling Restuls\\*" profiler-report-mode
            "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
            "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
            "\\*[Wo]*Man.*\\*$"
            "\\*ert\\*$" overseer-buffer-mode
            "\\*gud-debug\\*$"
            "\\*lsp-help\\*$" "\\*lsp session\\*$"
            "\\*quickrun\\*$"
            "\\*tldr\\*$"
            "\\*vc-.*\\*$"
            "^\\*elfeed-entry\\*$"
            "^\\*macro expansion\\**"

            "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
            "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
            "\\*docker-containers\\*" "\\*docker-images\\*" "\\*docker-networks\\*" "\\*docker-volumes\\*"
            "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
            "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
            rustic-cargo-outdated-mode rustic-cargo-test-moed))

    (with-eval-after-load 'projectile
      (setq popper-group-function #'popper-group-by-projectile))
    (setq popper-echo-dispatch-actions t)
    :config
    (popper-echo-mode 1)
    (with-no-warnings
      (defun popper-close-window-hack (&rest _)
        "Close popper window via `C-g'."
        ;; `C-g' can deactivate region
        (when (and (called-interactively-p 'interactive)
                   (not (region-active-p))
                   popper-open-popup-alist)
          (let ((window (caar popper-open-popup-alist)))
            (when (window-live-p window)
              (delete-window window)))))
      (advice-add #'keyboard-quit :before #'popper-close-window-hack))))


(provide 'init-windows)
;;; init-windows.el ends here
