;;----------------------------------------------------------------------------
;; Utility functions
;;----------------------------------------------------------------------------

;; @see http://ergoemacs.org/emacs/elisp_get-selection-or-unit.html
(defun get-selection-or-unit (unit)
  "Return the string and boundary of text selection or UNIT under cursor.

If `use-region-p' is true, then the region is the unit.  Else,
it depends on the UNIT. See `unit-at-cursor' for detail about
UNIT.

Returns a vector [text a b], where text is the string and a and b
are its boundary.

Example usage:
 (setq bds (get-selection-or-unit 'line))
 (setq inputstr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )"
  (interactive)
  (unless (region-active-p) (push-mark))
  (let ((p1 (region-beginning)) (p2 (region-end)))
    (if (use-region-p)
        (vector (buffer-substring-no-properties p1 p2) p1 p2 )
      (unit-at-cursor unit))))

(defun unit-at-cursor  (unit)
  "Return the string and boundary of UNIT under cursor.

Returns a vector [text a b], where text is the string and a and b are its boundary.

UNIT can be:
• 'word — sequence of 0 to 9, A to Z, a to z, and hyphen.
• 'glyphs — sequence of visible glyphs. Useful for file name, URL, …, that doesn't have spaces in it.
• 'line — delimited by “\\n”.
• 'block — delimited by “\\n\\n” or beginning/end of buffer.
• 'buffer — whole buffer. (respects `narrow-to-region')
• a vector [beginRegex endRegex] — The elements are regex strings used to determine the beginning/end of boundary chars. They are passed to `skip-chars-backward' and `skip-chars-forward'. For example, if you want paren as delimiter, use [\"^(\" \"^)\"]

Example usage:
    (setq bds (unit-at-cursor 'line))
    (setq myText (elt bds 0) p1 (elt bds 1) p2 (elt bds 2))

This function is similar to `thing-at-point' and `bounds-of-thing-at-point'.
The main differences are:
• this function returns the text and the 2 boundaries as a vector in one shot.
• 'line always returns the line without end of line character, avoiding inconsistency when the line is at end of buffer.
• 'word does not depend on syntax table.
• 'block does not depend on syntax table."
  (let (p1 p2)
    (save-excursion
        (cond
         ( (eq unit 'word)
           (let ((wordcharset "-A-Za-zÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ"))
             (skip-chars-backward wordcharset)
             (setq p1 (point))
             (skip-chars-forward wordcharset)
             (setq p2 (point))))

         ( (eq unit 'glyphs)
           (progn
             (skip-chars-backward "[:graph:]")
             (setq p1 (point))
             (skip-chars-forward "[:graph:]")
             (setq p2 (point))))

         ( (eq unit 'buffer)
           (progn
             (setq p1 (point-min))
             (setq p2 (point-max))))

         ((eq unit 'line)
          (progn
            (setq p1 (line-beginning-position))
            (setq p2 (line-end-position))))
         ((eq unit 'block)
          (progn
            (if (re-search-backward "\n\n" nil t)
                (progn (forward-char 2)
                       (setq p1 (point)))
              (setq p1 (line-beginning-position)))

            (if (re-search-forward "\n\n" nil t)
                (progn (backward-char)
                       (setq p2 (point)))
              (setq p2 (line-end-position)))))

         ((vectorp unit)
          (let (p0)
             (setq p0 (point))
             (skip-chars-backward (elt unit 0))
             (setq p1 (point))
             (goto-char p0)
             (skip-chars-forward (elt unit 1))
             (setq p2 (point))))))

    (vector (buffer-substring-no-properties p1 p2) p1 p2)))

;; find the directory containing a given library
(autoload 'find-library-name "find-func")
(defun directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))

;; define line-number-at-pos
(unless (fboundp 'line-number-at-pos)
  (defun line-number-at-pos (&optional pos)
    "Return (narrowed) buffer line number at position POS.
 If POS is nil, use current buffer location."
    (let ((opoint (or pos (point))) start)
      (save-excursion
	(goto-char (point-min))
	(setq start (point))
	(goto-char opoint)
	(forward-line 0)
	(1+ (count-lines start (point)))))))


;; trim string
(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun getline ()
  "Return line contents as a string."
  (let (beg end line-string)
    (setq beg (line-beginning-position))
    (setq end (line-end-position))
    (setq line-string (buffer-substring-no-properties beg end))
    (trim-string line-string)))

(defun getline-nth (line-number)
  "Return specific line's contents as a string."
  (save-excursion
    ;; when in elisp program use the following two statements
    ;; instead of goto-line
    (goto-char (point-min))
    (forward-line (1- line-number))
    (getline)))


(defconst *is-windows* (eq system-type 'windows-nt))
(defconst *is-a-mac* (eq system-type 'darwin))

(defun is-modern-emacs ()
  "if emacs version is greater than 24.3, return true else false."
  (if (or (and (= emacs-major-version 24) (>= emacs-minor-version 3))
	  (> emacs-major-version 24))
      t nil))


;;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;;; elisp version of try...catch...finally
;;; @see https://curiousprogrammer.wordpress.com/2009/06/08/error-handling-in-emacs-lisp/
(defmacro safe-wrap (fn &rest clean-up)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))


;;; steal from ag/dwim-at-point
(defun smart/dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

;;; improved version, based on ag/read-from-minibuffer
(defun smart/read-from-minibuffer (prompt)
  "Read a value from the minibuffer with PROMPT.
If there's a string at point, use it instead of prompt."
  (let* ((suggested (smart/dwim-at-point))
         (final-prompt
          (if suggested (format "%s (default %s): " prompt suggested)
            (format "%s: " prompt))))
    (if (or current-prefix-arg (string= "" suggested) (not suggested))
        (read-from-minibuffer final-prompt nil nil nil nil suggested)
      suggested)))

;; steal from view.el.gz
(defun smart/kill-buffer-if-not-modified (buf)
  "Like `kill-buffer', but does nothing if the buffer is modified."
  (let ((buf (get-buffer buf)))
    (and buf (not (buffer-modified-p buf))
         (kill-buffer buf))))


;;; simplicistic queue implementation
;;; thanks to wasamasa, @see http://ix.io/DoE
(defun list-to-queue (list)
  (cons list (if (consp list) (last list) '())))

(defun queue-to-list (queue)
  (car queue))

(defun make-queue ()
  (cons '() '()))

(defun queue-empty-p (queue)
  (null (car queue)))

(defun queue-get (queue)
  (if (null (car queue))
      (error "Queue is empty")
      (let ((x (caar queue)))
        (setcar queue (cdar queue))
        (if (null (car queue)) (setcdr queue '()))
        x)))

(defun queue-put (queue x)
  (let ((entry (cons x '())))
    (if (null (car queue))
        (setcar queue entry)
        (setcdr (cdr queue) entry))
    (setcdr queue entry)
    x))

(provide 'init-utils)
