;; init-advice.el --- advice settings -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Advice settings to improve productivity
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy-preview functions similar to ivy-file-preview mode
;; but with less code and cleanup files better than ivy-file-preview
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ivy-preview-window-configuration nil
  "The window configuration to be restored upon closing the buffer.")

(defvar ivy-preview-selected-window nil
  "The currently selected window.")

(defvar ivy-preview-selected-window-line-nb nil
  "The currently selected window line number.")

(defvar ivy-preview-created-buffers ()
  "List of newly created buffers.")

(defvar ivy-preview-previous-buffers ()
  "List of buffers created before opening counsel-git-grep-advice.")

(defun ivy-preview-setup (&rest _)
  "Setup `ivy-preview'."
  (setq ivy-preview-window-configuration (current-window-configuration)
        ivy-preview-selected-window (frame-selected-window)
        ivy-preview-selected-window-line-nb (line-number-at-pos)
        ivy-preview-created-buffers ()
        ivy-preview-previous-buffers (buffer-list))
  (advice-add 'ivy-set-index :after #'ivy-preview-iterate-action)
  (advice-add 'ivy--exhibit :after #'ivy-preview-iterate-action)
  (add-hook 'minibuffer-exit-hook #'ivy-preview-quit))

(defun ivy-preview-quit ()
  "Quit `ivy-preview'."
  (when-let ((configuration ivy-preview-window-configuration)
             (selected-window ivy-preview-selected-window))
    (advice-remove 'ivy-set-index #'ivy-preview-iterate-action)
    (advice-remove 'ivy--exhibit #'ivy-preview-iterate-action)
    (remove-hook 'minibuffer-exit-hook #'ivy-preview-quit)
    (set-window-configuration configuration)
    (select-window selected-window)
    (goto-char (point-min))
    (forward-line (1- ivy-preview-selected-window-line-nb))
    (mapc 'kill-buffer-if-not-modified ivy-preview-created-buffers)
    (setq ivy-preview-created-buffers ())))

(defun ivy-preview-iterate-action (&optional arg)
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
               (mapcar (function buffer-name) ivy-preview-previous-buffers))
        (add-to-list 'ivy-preview-created-buffers (window-buffer))))))

;; hook up with counsel-git-grep
;; note: ivy-preview-setup could work with any ivy-command which returns filename:linenumber as entry
(advice-add 'counsel-git-grep :before #'ivy-preview-setup)


(provide 'init-advice)
;;; init-advice.el ends here
