;;; init-gptel-completion.el --- GPTel-powered C++ code completion -*- lexical-binding: t -*-

;;; Commentary:
;; C++ code completion powered by eglot + gptel + ag

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
- Do NOT add code fence
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
;; Completion Interface
;; ------------------------------------------------------------
(defun my/gptel-handle-response (response _info)
  "Handle GPTel RESPONSE by displaying it in an overlay."
  (when response
    (my/show-overlay response)))

(defun my/gptel-complete ()
  "Request code completion from GPTel."
  (interactive)
  (gptel-request (my/build-gptel-prompt)
    :system my/completion-system-prompt
    :callback #'my/gptel-handle-response))

(defun my/accept-gptel-completion ()
  "Insert the current completion and clear overlay."
  (interactive)
  (when my/gptel-overlay
    (insert (overlay-get my/gptel-overlay 'after-string))
    (my/clear-overlay)))

(defun my/reject-gptel-completion ()
  "Dismiss the current completion."
  (interactive)
  (my/clear-overlay))

(global-set-key (kbd "C-c <TAB>") #'my/gptel-complete)


(provide 'init-gptel-completion)
;;; init-gptel-completion.el ends here
