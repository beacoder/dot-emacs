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

;; ------------------------------------------------------------
;; Context Extraction Functions
;; ------------------------------------------------------------
(defun my/cpp-current-function ()
  "Return the current C++ function definition as a string."
  (save-excursion
    (ignore-errors
      (treesit-beginning-of-defun)
      (let ((func-beginning (point)))
        (treesit-end-of-defun)
        (buffer-substring-no-properties func-beginning (point))))))

(defun my/symbol-at-point ()
  "Return the symbol at point or active region as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties (symbol-name (symbol-at-point))))))

(defun my/eglot-local-symbols ()
  "Return a list of local symbols from Eglot."
  (when (eglot-current-server)
    (let ((symbols (xref-backend-apropos (xref-find-backend) (my/symbol-at-point))))
      (mapcar (lambda (s)
                (substring-no-properties (xref-match-item-summary s)))
              symbols))))

(defun my/ag-search (query)
  "Search for QUERY using ag and return trimmed results."
  (string-trim
   (shell-command-to-string
    (format "ag --cpp --nobreak --noheading \"%s\" | head -n 20" query))))

;; ------------------------------------------------------------
;; Prompt Construction
;; ------------------------------------------------------------
(defconst my/completion-system-prompt
  "You are an expert C++ programmer."
  "Completion system prompt.")

(defconst my/completion-prompt
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

(defun my/build-gptel-prompt ()
  "Build a structured prompt for GPTel completion."
  (let* ((current-func (my/cpp-current-function))
         (local-symbols (my/eglot-local-symbols))
         (current-symbol (my/symbol-at-point))
         (similar-patterns (and current-symbol
                                (my/ag-search current-symbol))))
    (format my/completion-prompt
            current-func
            (string-join local-symbols ", ")
            (or similar-patterns "None found"))))

;; ------------------------------------------------------------
;; Overlay Management
;; ------------------------------------------------------------
(defvar my/gptel-overlay nil
  "Overlay for displaying GPTel completions.")

(defun my/clear-overlay ()
  "Remove the completion overlay."
  (when my/gptel-overlay
    (delete-overlay my/gptel-overlay)
    (setq my/gptel-overlay nil)))

(defun my/show-overlay (text)
  "Display TEXT in an overlay at point."
  (my/clear-overlay)
  (setq my/gptel-overlay (make-overlay (point) (point)))
  (overlay-put my/gptel-overlay
               'after-string
               (propertize text 'face 'shadow)))

(defun my/gptel-overlay-active-p ()
  "Check if completion overlay is active."
  (and my/gptel-overlay (overlay-buffer my/gptel-overlay)))

;; ------------------------------------------------------------
;; GPTel Interaction
;; ------------------------------------------------------------
(defvar my/gptel-regenerate-timer nil
  "Timer for delayed regeneration.")

(defun my/gptel-handle-response (response _info)
  "Handle GPTel RESPONSE by displaying it in an overlay."
  (when response
    (message "")
    (my/show-overlay response)))

;;;###autoload
(defun my/gptel-complete ()
  "Request GPTel code completion."
  (interactive)
  (message "Generating completion...")
  (gptel-request (my/build-gptel-prompt)
    :system my/completion-system-prompt
    :callback #'my/gptel-handle-response))

(defun my/self-insert-p ()
  "Check if current command is self-insert."
  (eq this-command 'self-insert-command))

(defun my/last-command-was-ret-p ()
  "Return non-nil if the last command was triggered by RET or RETURN."
  (memq last-command-event '(?\r return)))

(defun my/accept-gptel-completion ()
  "Accept completion."
  (when (my/gptel-overlay-active-p)
    (let ((text (overlay-get my/gptel-overlay 'after-string)))
      (my/clear-overlay)
      (insert text))))

(defun my/reject-gptel-completion ()
  "Reject completion."
  (when (my/gptel-overlay-active-p)
    (my/clear-overlay)))

(defun my/delayed-regenerate ()
  "Delayed regeneration."
  (when my/gptel-regenerate-timer
    (cancel-timer my/gptel-regenerate-timer))
  (setq my/gptel-regenerate-timer
        (run-with-idle-timer
         0.25 nil   ;; adjust: 0.2–0.4 works well
         #'my/gptel-complete)))

(defun my/gptel-regenerate-on-input ()
  "Trigger regeneration when conditions met."
  (when (derived-mode-p 'c++-mode)
    (if (my/last-command-was-ret-p)
        (my/accept-gptel-completion)
      (my/reject-gptel-completion)
      (when (my/self-insert-p)
        (my/delayed-regenerate)))))

(add-hook 'post-command-hook #'my/gptel-regenerate-on-input)


(provide 'init-gptel-completion)
;;; init-gptel-completion.el ends here
