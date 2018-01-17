;;----------------------------------------------------------------------------
;; utilities for productivity
;;----------------------------------------------------------------------------

;; @see http://ergoemacs.org/emacs/emacs_open_file_path_fast.html
(global-set-key (kbd "C-c j") 'open-file-at-cursor)
(defun open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path is starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
This command is similar to `find-file-at-point' but without prompting for confirmation.
"
  (interactive)
  (let ( (path (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'filename) ) ))
    (if (string-match-p "\\`https?://" path)
        (browse-url path)
      (progn ; not starting “http://”
        (if (file-exists-p path)
            (find-file path)
          (if (file-exists-p (concat path ".el"))
              (find-file (concat path ".el"))
            (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" path) )
              (find-file path ))))))))

;;----------------------------------------------------------------------------
;; register-usage
;;----------------------------------------------------------------------------

;; @see https://github.com/xahlee/xah_emacs_init/blob/master/xah_emacs_editing_commands.el
(defun xah-copy-to-register-1 ()
  "Copy current line or text selection to register 1.
See also: `xah-paste-from-register-1', `copy-to-register'."
  (interactive)
  (let* (
         (bds (get-selection-or-unit 'line ))
         (inputStr (elt bds 0) )
         (p1 (elt bds 1) )
         (p2 (elt bds 2)))
    (copy-to-register ?1 p1 p2)
    (message "copied to register 1: 「%s」." inputStr)))

(defun xah-paste-from-register-1 ()
  "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'."
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(global-set-key (kbd "M-1") 'xah-copy-to-register-1)
(global-set-key (kbd "M-2") 'xah-paste-from-register-1)

(defun bright-point-to-register-3()
  "Save current point to register 3."
  (interactive)
  (point-to-register ?3)
  (message "Point saved to register 3"))

(defun bright-jump-to-register-3 ()
  "Jumped to register 3."
  (interactive)
  (jump-to-register ?3))

(defun bright-point-to-register-5 ()
  "Save current point to register 5."
  (interactive)
  (point-to-register ?5)
  (message "Point saved to register 5"))

(defun bright-jump-to-register-5 ()
  "Jumped to register 5."
  (interactive)
  (jump-to-register ?5))

(global-set-key (kbd "M-3") 'bright-point-to-register-3)
(global-set-key (kbd "M-4") 'bright-jump-to-register-3)

(global-set-key (kbd "M-5") 'bright-point-to-register-5)
(global-set-key (kbd "M-6") 'bright-jump-to-register-5)

;;----------------------------------------------------------------------------
;; Copy/Kill Current Line If No Selection
;;----------------------------------------------------------------------------

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').
URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-05-06"
  (interactive)
  (let (p1 p2 msg)
    (if current-prefix-arg
        (progn (setq p1 (point-min))
               (setq p2 (point-max))
               (setq msg "buffer copied"))
      (if (use-region-p)
          (progn (setq p1 (region-beginning))
                 (setq p2 (region-end))
                 (setq msg "region copied"))
        (progn (setq p1 (line-beginning-position))
               (setq p2 (line-end-position))
               (setq msg "line copied"))))
    (kill-ring-save p1 p2)
    (message msg)))
      
(defun kill-line-or-region ()
  "kill whole current line, or text selection.
When `universal-argument' is called first, kill whole buffer (respects `narrow-to-region')."
  (interactive)
  (let (p1 p2 msg)
    (if current-prefix-arg
        (progn (setq p1 (point-min))
               (setq p2 (point-max))
               (kill-region p1 p2)
               (message "buffer killed"))
      (if (use-region-p)
          (progn
            (kill-region (region-beginning) (region-end))
            (message "region killed"))
        (progn
          (kill-whole-line)
          (message "whole line killed"))))))

(global-set-key (kbd "M-9") 'xah-copy-line-or-region)
(global-set-key (kbd "M-0") 'kill-line-or-region)

;;----------------------------------------------------------------------------
;; useful keyboard macros
;;----------------------------------------------------------------------------

;;; How to save a macro for future use
;; name-last-kbd-macro => give macro a name
;; insert-kbd-macro    => generate lisp code for the macro

(fset 'select-whole-line "\C-a\C-@\C-e")
(global-set-key (kbd "M-7") 'select-whole-line)

(defun show-buffer-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))
(global-set-key (kbd "C-c C-f") 'show-buffer-name)

;;----------------------------------------------------------------------------
;; kill line/buffer depending on buffer read-only or not
;;----------------------------------------------------------------------------

(global-set-key (kbd "C-k")
                (lambda (&optional ARG)
                  (interactive "P")
                  (if buffer-read-only
                      (smart/kill-buffer-if-not-modified (current-buffer))
                    (kill-line ARG))))

(provide 'init-productivity)
