;;; missing.el --- pick up those missing features in recent emacs.

(defcustom use-empty-active-region nil
  "Whether \"region-aware\" commands should act on empty regions.
If nil, region-aware commands treat empty regions as inactive.
If non-nil, region-aware commands treat the region as active as
long as the mark is active, even if the region is empty.

Region-aware commands are those that act on the region if it is
active and Transient Mark mode is enabled, and on the text near
point otherwise."
  :type 'boolean
  :version "23.1"
  :group 'editing-basics)

(defun use-region-p ()
  "Return t if the region is active and it is appropriate to act on it.
This is used by commands that act specially on the region under
Transient Mark mode.

The return value is t if Transient Mark mode is enabled and the
mark is active; furthermore, if `use-empty-active-region' is nil,
the region must not be empty.  Otherwise, the return value is nil.

For some commands, it may be appropriate to ignore the value of
`use-empty-active-region'; in that case, use `region-active-p'."
  (and (region-active-p)
       (or use-empty-active-region (> (region-end) (region-beginning)))))

(defun region-active-p ()
  "Return t if Transient Mark mode is enabled and the mark is active.

Some commands act specially on the region when Transient Mark
mode is enabled.  Usually, such commands should use
`use-region-p' instead of this function, because `use-region-p'
also checks the value of `use-empty-active-region'."
  (and transient-mark-mode mark-active))
  
(defun kill-whole-line (&optional arg)
  "Kill current line.
With prefix ARG, kill that many lines starting from the current line.
If ARG is negative, kill backward.  Also kill the preceding newline.
\(This is meant to make \\[repeat] work well with negative arguments.\)
If ARG is zero, kill current line but exclude the trailing newline."
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (> arg 0) (eobp) (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0) (bobp) (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (unless (eq last-command 'kill-region)
    (kill-new "")
    (setq last-command 'kill-region))
  (cond ((zerop arg)
	 ;; We need to kill in two steps, because the previous command
	 ;; could have been a kill command, in which case the text
	 ;; before point needs to be prepended to the current kill
	 ;; ring entry and the text after point appended.  Also, we
	 ;; need to use save-excursion to avoid copying the same text
	 ;; twice to the kill ring in read-only buffers.
	 (save-excursion
	   (kill-region (point) (progn (forward-visible-line 0) (point))))
	 (kill-region (point) (progn (end-of-visible-line) (point))))
	((< arg 0)
	 (save-excursion
	   (kill-region (point) (progn (end-of-visible-line) (point))))
	 (kill-region (point)
		      (progn (forward-visible-line (1+ arg))
			     (unless (bobp) (backward-char))
			     (point))))
	(t
	 (save-excursion
	   (kill-region (point) (progn (forward-visible-line 0) (point))))
	 (kill-region (point)
		      (progn (forward-visible-line arg) (point))))))
		      
(defsubst string-match-p (regexp string &optional start)
  "\
Same as `string-match' except this function does not change the match data."
  (let ((inhibit-changing-match-data t))
    (string-match regexp string start)))

(provide 'missing)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
