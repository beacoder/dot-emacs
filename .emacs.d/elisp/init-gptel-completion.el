;;; init-gptel-completion.el --- GPTel-powered C++ completion -*- lexical-binding: t -*-

;;; Commentary:
;; C++ code completion using eglot + gptel + ag

;;; Code:

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

(defun my/eglot--in-scope-symbols+kind ()
  "Return list of local symbols from Eglot."
  (when-let* ((server (eglot--current-server-or-lose))
              (pos (eglot--pos-to-lsp-position (point)))
              (params `(:textDocument (:uri ,(eglot--path-to-uri (buffer-file-name)))
                                      :position ,pos
                                      :context (:triggerKind 1)))
              (completion (jsonrpc-request server
                                           :textDocument/completion
                                           params)))
    (let ((items (cond
                  ((vectorp completion) completion)
                  ((plist-get completion :items))
                  (t nil))))
      (mapcar (lambda (item)
                (cons
                 (plist-get item :label)
                 (list :label (plist-get item :label)
                       :kind  (plist-get item :kind))))
              items))))

(defun my/gptel--classify-symbols (symbols)
  "Classify SYMBOLS into different kind."
  (cl-loop for s in symbols
           if (memq (plist-get s :kind) '(2 3 4)) collect s into funcs
           else if (memq (plist-get s :kind) '(6 21)) collect s into vars
           else if (memq (plist-get s :kind) '(5 10 20)) collect s into members
           finally return `(:funcs ,funcs :vars ,vars :members ,members)))

(defun my/gptel--select-search-symbols (classified)
  "Select symbols to search based on CLASSIFIED."
  (append
   (cl-subseq (plist-get classified :funcs) 0 2)
   (cl-subseq (plist-get classified :members) 0 1)))

(defun my/gptel--ag-pattern-for-symbol (symbol)
  "Format SYMBOL for searching with ag."
  (let ((name (plist-get symbol :label)))
    (cond
     ((memq (plist-get symbol :kind) '(2 3 4))
      (when (string-match "\\b\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*(" name)
        (setq name (match-string 1 name)))
      (format "%s\\s*\\(" name))
     ((memq (plist-get symbol :kind) '(5 10 20))
      (format "(\\.|->)%s\\b" name))
     (t name))))

(defun my/gptel--ag-search-pattern (pattern)
  "Search PATTERN using ag."
  (shell-command-to-string
   (format "ag --cpp --nobreak --noheading -C 3 \"%s\" | head -n 30"
           pattern)))

(defun my/gptel--ag-similar-patterns (s-k)
  "Search similar patterns based on S-K."
  (let* ((symbols s-k)
         (classified (my/gptel--classify-symbols symbols))
         (targets (my/gptel--select-search-symbols classified)))
    (string-join
     (cl-loop for sym in targets
              for pat = (my/gptel--ag-pattern-for-symbol sym)
              collect (my/gptel--ag-search-pattern pat))
     "\n\n")))

;; ------------------------------------------------------------
;; Prompt Construction
;; ------------------------------------------------------------
(defconst my/gptel--system-prompt
  "You are an expert C++ language-server–style code completion engine.

You are operating inside a very large, existing C++ codebase.

Your task:
- Continue or complete the code at the cursor position
- Produce code that would compile in this codebase

You are given:
- The current function body
- The list of in-scope symbols (authoritative)
- Examples of similar patterns retrieved from this repository

Hard rules (must follow):
- Use ONLY the provided in-scope symbols and patterns
- Do NOT invent new functions, types, macros, or headers
- Do NOT change existing code outside the completion
- Respect C++ syntax, constness, references, and ownership
- Match formatting, indentation, and naming style
- Prefer existing helper functions and idioms
- If unsure, produce the smallest reasonable completion

Output rules:
- Output ONLY the code to be inserted
- Do NOT include explanations, comments, or markdown
- Do NOT repeat existing code unless necessary for completion"
  "Completion system prompt.")

(defconst my/gptel--completion-prompt
  "Current function:
```cpp
%s
```

In-scope symbols:
%s

Similar patterns in this repository:
%s"
  "Completion user prompt.")

(defun my/gptel--build-prompt ()
  "Assemble GPTel completion prompt."
  (let* ((func (or (my/gptel--cpp-current-function) "N/A"))
         (symbols+kind (or (my/eglot--in-scope-symbols+kind) '()))
         (symbols (or (delete-dups (mapcar #'car symbols+kind)) '()))
         (s+k (or (mapcar #'cdr symbols+kind) '()))
         (patterns (or (my/gptel--ag-similar-patterns s+k) "None found")))
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
