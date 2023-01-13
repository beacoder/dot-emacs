;; init-advice.el --- advice settings -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Advice settings to improve productivity
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; counsel-git-grep enhancement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar counsel-git-grep-advice-window-configuration nil
  "The window configuration to be restored upon closing the buffer.")

(defvar counsel-git-grep-advice-selected-window nil
  "The currently selected window.")

(defvar counsel-git-grep-advice-selected-window-line-nb nil
  "The currently selected window line number.")

(defvar counsel-git-grep-advice-created-buffers ()
  "List of newly created buffers.")

(defvar counsel-git-grep-advice-previous-buffers ()
  "List of buffers created before opening counsel-git-grep-advice.")

(defun counsel-git-grep-advice-save-window-configuration (&rest _)
  "Save current window configuration."
  (setq counsel-git-grep-advice-window-configuration (current-window-configuration)
        counsel-git-grep-advice-selected-window (frame-selected-window)
        counsel-git-grep-advice-selected-window-line-nb (line-number-at-pos)
        counsel-git-grep-advice-created-buffers ()
        counsel-git-grep-advice-previous-buffers (buffer-list))
  (advice-add 'ivy-set-index :after #'counsel-git-grep-advice-iterate-action)
  (advice-add 'ivy--exhibit :after #'counsel-git-grep-advice-iterate-action)
  (add-hook 'minibuffer-exit-hook #'counsel-git-grep-advice-quit))

(defun counsel-git-grep-advice-quit ()
  "Quit `counsel-git-grep'."
  (when-let ((configuration counsel-git-grep-advice-window-configuration)
             (selected-window counsel-git-grep-advice-selected-window))
    (advice-remove 'ivy-set-index #'counsel-git-grep-advice-iterate-action)
    (advice-remove 'ivy--exhibit #'counsel-git-grep-advice-iterate-action)
    (remove-hook 'minibuffer-exit-hook #'counsel-git-grep-advice-quit)
    (set-window-configuration configuration)
    (select-window selected-window)
    (goto-char (point-min))
    (forward-line (1- counsel-git-grep-advice-selected-window-line-nb))
    (mapc 'kill-buffer-if-not-modified counsel-git-grep-advice-created-buffers)
    (setq counsel-git-grep-advice-created-buffers ())))

(defun counsel-git-grep-advice-iterate-action (&optional arg)
  "Preview matched occurrence, ignore ARG."
  (save-selected-window
    (ignore arg)
    (when-let* ((cur-string (nth ivy--index ivy--all-candidates))
                (found (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" cur-string))
                (file-name (match-string-no-properties 1 cur-string))
                (line-nb (match-string-no-properties 2 cur-string)))
      (find-file-read-only-other-window file-name)
      (with-no-warnings (goto-char (point-min))
                        (forward-line (1- (string-to-number line-nb)))
                        (pulse-momentary-highlight-region (line-beginning-position) (line-end-position)))
      (unless (member
               (buffer-name (window-buffer))
               (mapcar (function buffer-name) counsel-git-grep-advice-previous-buffers))
        (add-to-list 'counsel-git-grep-advice-created-buffers (window-buffer))))))

;; hook up with counsel-git-grep
(advice-add 'counsel-git-grep :before #'counsel-git-grep-advice-save-window-configuration)


(provide 'init-advice)
;;; init-advice.el ends here
