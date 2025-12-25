;;; init-gptel-completion.el --- GPTel-powered C++ completion -*- lexical-binding: t -*-

;;; Commentary:
;; C++ code completion using eglot + gptel + ag

;;; Code:

;; ------------------------------------------------------------
;; Prerequisites
;; ------------------------------------------------------------
;; 1. clangd --background-index --clang-tidy
;; 2. silver search install
;; 3. gptel configured

;; ------------------------------------------------------------
;; Configuration
;; ------------------------------------------------------------
(setq eglot-extend-to-xref t)

(defgroup my/gptel-completion nil
  "GPTel-based C++ code completion."
  :group 'tools)

(defcustom my/gptel-idle-delay 0.25
  "Idle time before regenerating GPTel completion."
  :type 'number
  :group 'my/gptel-completion)

;; ------------------------------------------------------------
;; Context Extraction
;; ------------------------------------------------------------
(defun my/gptel--cpp-current-function ()
  "Return current C++ function definition as string."
  (when (treesit-ready-p 'cpp)
    (save-excursion
      (ignore-errors
        (treesit-beginning-of-defun)
        (let ((beg (point)))
          (treesit-end-of-defun)
          (buffer-substring-no-properties beg (point)))))))

(defun my/gptel--symbol-at-point ()
  "Return the symbol at point or active region as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties (symbol-name (symbol-at-point))))))

(defun my/gptel--eglot-local-symbols ()
  "Return list of local symbols from Eglot."
  (when-let* ((server (eglot-current-server))
              (backend (xref-find-backend))
              (query (my/gptel--symbol-at-point)))
    (mapcar
     (lambda (item)
       (substring-no-properties
        (xref-match-item-summary item)))
     (xref-backend-apropos backend query))))

(defun my/gptel--ag-search (query)
  "Search QUERY using ag and return trimmed results."
  (when (and query (executable-find "ag"))
    (string-trim
     (shell-command-to-string
      (format
       "ag --cpp --nobreak --noheading %S | head -n 20"
       query)))))

;; ------------------------------------------------------------
;; Prompt Construction
;; ------------------------------------------------------------
(defconst my/gptel--system-prompt
  "You are an expert C++ programmer."
  "Completion system prompt.")

(defconst my/gptel--completion-prompt
  "You are completing C++ code inside a large code base.
Current function:
%s
In-scope symbols:
%s
Similar patterns in this repository:
%s
Rules:
- Continue the code naturally
- Do NOT add code fences
- Do NOT invent APIs
- Reuse existing patterns
- Match formatting and style
- Output ONLY the code to be inserted"
  "Completion core prompt.")

(defun my/gptel--build-prompt ()
  "Assemble GPTel completion prompt."
  (let* ((func (or (my/gptel--cpp-current-function) "N/A"))
         (symbols (or (my/gptel--eglot-local-symbols) '()))
         (symbol (my/gptel--symbol-at-point))
         (patterns (or (my/gptel--ag-search symbol) "None found")))
    (format my/gptel--completion-prompt
            func
            (string-join symbols ", ")
            patterns)))

;; ------------------------------------------------------------
;; Overlay Management
;; ------------------------------------------------------------
(defvar my/gptel--overlay nil
  "Overlay used to display GPTel completions.")

(defun my/gptel--clear-overlay ()
  "Remove GPTel completion overlay."
  (when (overlayp my/gptel--overlay)
    (delete-overlay my/gptel--overlay))
  (setq my/gptel--overlay nil))

(defun my/gptel--overlay-active-p ()
  "Return non-nil if GPTel overlay is active."
  (overlayp my/gptel--overlay))

(defun my/gptel--show-overlay (text)
  "Show TEXT as ghost completion at point."
  (my/gptel--clear-overlay)
  (setq my/gptel--overlay (make-overlay (point) (point)))
  (overlay-put my/gptel--overlay
               'after-string
               (propertize text 'face 'shadow)))

(defun my/gptel--accept-overlay ()
  "Insert overlay text into buffer."
  (when (my/gptel--overlay-active-p)
    (let ((text (overlay-get my/gptel--overlay 'after-string)))
      (my/gptel--clear-overlay)
      (insert text))))

;; ------------------------------------------------------------
;; GPTel Interaction
;; ------------------------------------------------------------
(defvar my/gptel--regenerate-timer nil
  "Idle timer for GPTel regeneration.")

(defun my/gptel--handle-response (response _info)
  "Display GPTel RESPONSE."
  (when (and response (stringp response))
    (message "")
    (my/gptel--show-overlay response)))

;;;###autoload
(defun my/gptel-complete ()
  "Request GPTel code completion."
  (interactive)
  (message "Generating completion...")
  (gptel-request
      (my/gptel--build-prompt)
    :system my/gptel--system-prompt
    :callback #'my/gptel--handle-response))

(defun my/gptel--schedule-regenerate ()
  "Schedule GPTel completion after idle delay."
  (when my/gptel--regenerate-timer
    (cancel-timer my/gptel--regenerate-timer))
  (setq my/gptel--regenerate-timer
        (run-with-idle-timer
         my/gptel-idle-delay nil
         #'my/gptel-complete)))

;; ------------------------------------------------------------
;; Input Handling
;; ------------------------------------------------------------
(defun my/gptel--self-insert-p ()
  "Return non-nil if command was self insert."
  (eq this-command 'self-insert-command))

(defun my/gptel--last-command-was-ret-p ()
  "Return non-nil if last command was RET/RETURN."
  (memq last-command-event '(?\r return)))

(defun my/gptel--post-command ()
  "Post-command hook driving GPTel completion."
  (when (derived-mode-p 'c++-mode)
    (cond
     ((my/gptel--last-command-was-ret-p)
      (when (my/gptel--overlay-active-p)
        (delete-char -1))
      (my/gptel--accept-overlay))
     ((my/gptel--self-insert-p)
      (my/gptel--clear-overlay)
      (my/gptel--schedule-regenerate))
     (t
      (my/gptel--clear-overlay)))))

(add-hook 'post-command-hook #'my/gptel--post-command)


(provide 'init-gptel-completion)
;;; init-gptel-completion.el ends here
