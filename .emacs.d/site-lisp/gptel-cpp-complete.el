;;; gptel-cpp-complete.el --- GPTel-powered C++ completion -*- lexical-binding: t -*-

;; Copyright (C) 2025 by Huming Chen

;; Author: Huming Chen <chenhuming@gmail.com>
;; URL: https://github.com/beacoder/gptel-cpp-complete
;; Version: 0.0.1
;; Created: 2025-12-26
;; Keywords: programming, convenience
;; Package-Requires: ((emacs "30.1") (eglot "1.19") (gptel "0.9.8"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; C++ code completion powered by eglot, gptel, ag

;;; Code:

(require 'eglot)
(require 'gptel)
(require 'treesit)

;; ------------------------------------------------------------
;; Configuration
;; ------------------------------------------------------------
(setq eglot-extend-to-xref t)

(defgroup gptel-cpp-complete nil
  "GPTel-based C++ code completion."
  :group 'tools)

(defcustom gptel-cpp-complete-idle-delay 0.25
  "Idle time before regenerating GPTel completion."
  :type 'number
  :group 'gptel-cpp-complete)

;; ------------------------------------------------------------
;; Context Extraction
;; ------------------------------------------------------------
(defun gptel-cpp-complete--cpp-current-function ()
  "Return current C++ function definition as string."
  (when (treesit-ready-p 'cpp)
    (save-excursion
      (ignore-errors
        (treesit-beginning-of-defun)
        (let ((beg (point)))
          (treesit-end-of-defun)
          (buffer-substring-no-properties beg (point)))))))

(defun gptel-cpp-complete--in-scope-symbols+kind ()
  "Return list of local symbols from Eglot."
  (when-let* ((server (eglot--current-server-or-lose))
              (pos (eglot--pos-to-lsp-position (point)))
              (params `(:textDocument (:uri ,(eglot-path-to-uri (buffer-file-name)))
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

(defun gptel-cpp-complete--classify-symbols (symbols)
  "Classify SYMBOLS into different kind."
  (cl-loop for s in symbols
           if (memq (plist-get s :kind) '(2 3 4)) collect s into funcs
           else if (memq (plist-get s :kind) '(6 21)) collect s into vars
           else if (memq (plist-get s :kind) '(5 10 20)) collect s into members
           finally return `(:funcs ,funcs :vars ,vars :members ,members)))

(defun gptel-cpp-complete--safe-subseq (seq start end)
  "Safely extracts a subseq from SEQ from START to END (not included)."
  (when seq
    (cl-subseq seq start (min end (length seq)))))

(defun gptel-cpp-complete--select-search-symbols (classified)
  "Select symbols to search based on CLASSIFIED."
  (append
   (gptel-cpp-complete--safe-subseq (plist-get classified :funcs) 0 2)
   (gptel-cpp-complete--safe-subseq (plist-get classified :members) 0 1)))

(defun gptel-cpp-complete--ag-pattern-for-symbol (symbol)
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

(defun gptel-cpp-complete--ag-search-pattern (pattern)
  "Search PATTERN using ag."
  (shell-command-to-string
   (format "ag --cpp --nobreak --noheading -C 3 \"%s\" | head -n 30"
           pattern)))

(defun gptel-cpp-complete--ag-similar-patterns (s-k)
  "Search similar patterns based on S-K."
  (let* ((symbols s-k)
         (classified (gptel-cpp-complete--classify-symbols symbols))
         (targets (gptel-cpp-complete--select-search-symbols classified)))
    (string-join
     (cl-loop for sym in targets
              for pat = (gptel-cpp-complete--ag-pattern-for-symbol sym)
              collect (gptel-cpp-complete--ag-search-pattern pat))
     "\n\n")))

;; ------------------------------------------------------------
;; Prompt Construction
;; ------------------------------------------------------------
(defconst gptel-cpp-complete--system-prompt
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

(defconst gptel-cpp-complete--completion-prompt
  "Current function:
```cpp
%s
```
In-scope symbols:
%s

Similar patterns in this repository:
%s"
  "Completion user prompt.")

(defun gptel-cpp-complete--build-prompt ()
  "Assemble GPTel completion prompt."
  (let* ((func (or (gptel-cpp-complete--cpp-current-function) "N/A"))
         (symbols+kind (or (gptel-cpp-complete--in-scope-symbols+kind) '()))
         (symbols (or (delete-dups (mapcar #'car symbols+kind)) '()))
         (s+k (or (mapcar #'cdr symbols+kind) '()))
         (patterns (or (gptel-cpp-complete--ag-similar-patterns s+k) "None found")))
    (format gptel-cpp-complete--completion-prompt
            func
            (string-join symbols ", ")
            patterns)))

;; ------------------------------------------------------------
;; Overlay Management
;; ------------------------------------------------------------
(defvar-local gptel-cpp-complete--overlay nil)

(defun gptel-cpp-complete--clear-overlay ()
  "Remove GPTel completion overlay."
  (when (overlayp gptel-cpp-complete--overlay)
    (delete-overlay gptel-cpp-complete--overlay))
  (setq gptel-cpp-complete--overlay nil))

(defun gptel-cpp-complete--overlay-active-p ()
  "Return non-nil if GPTel overlay is active."
  (overlayp gptel-cpp-complete--overlay))

(defun gptel-cpp-complete--show-overlay (text)
  "Show TEXT as ghost completion at point."
  (gptel-cpp-complete--clear-overlay)
  (setq gptel-cpp-complete--overlay (make-overlay (point) (point)))
  (overlay-put gptel-cpp-complete--overlay
               'after-string
               (propertize text 'face 'shadow)))

(defun gptel-cpp-complete--accept-overlay ()
  "Insert overlay text into buffer."
  (when (gptel-cpp-complete--overlay-active-p)
    (let ((text (overlay-get gptel-cpp-complete--overlay 'after-string)))
      (gptel-cpp-complete--clear-overlay)
      (insert text))))

;; ------------------------------------------------------------
;; GPTel Interaction
;; ------------------------------------------------------------
(defvar-local gptel-cpp-complete--regenerate-timer nil)
(defvar-local gptel-cpp-complete--request nil)
(defvar-local gptel-cpp-complete--in-flight nil)

(defun gptel-cpp-complete--cancel-request ()
  "Cancel any in-flight gptel request for this buffer."
  (when gptel-cpp-complete--request
    (ignore-errors
      (gptel-abort gptel-cpp-complete--request))
    (setq gptel-cpp-complete--request nil)))

(defun gptel-cpp-complete--handle-response (response _info)
  "Display GPTel RESPONSE."
  (setq gptel-cpp-complete--in-flight nil
        gptel-cpp-complete--request nil)
  (when (and response (stringp response))
    (message "")
    (gptel-cpp-complete--show-overlay response)))

(defun gptel-cpp-complete--fire-request ()
  "Start a new AI completion request, canceling any in-flight one."
  (gptel-cpp-complete--cancel-request)
  (setq gptel-cpp-complete--in-flight t
        gptel-cpp-complete--request
        (gptel-request
            (gptel-cpp-complete--build-prompt)
          :system gptel-cpp-complete--system-prompt
          :callback #'gptel-cpp-complete--handle-response)))

;;;###autoload
(defun gptel-cpp-complete ()
  "Request GPTel code completion."
  (interactive)
  (message "Generating completion...")
  (gptel-cpp-complete--fire-request))

(defun gptel-cpp-complete--schedule-regenerate ()
  "Schedule GPTel completion after idle delay."
  (when gptel-cpp-complete--regenerate-timer
    (cancel-timer gptel-cpp-complete--regenerate-timer))
  (setq gptel-cpp-complete--regenerate-timer
        (run-with-idle-timer
         gptel-cpp-complete-idle-delay nil
         #'gptel-cpp-complete)))

;; ------------------------------------------------------------
;; Input Handling
;; ------------------------------------------------------------
(defun gptel-cpp-complete--self-insert-p ()
  "Return non-nil if command was self insert."
  (eq this-command 'self-insert-command))

(defun gptel-cpp-complete--last-command-was-ret-p ()
  "Return non-nil if last command was RET/RETURN."
  (memq last-command-event '(?\r return)))

(defun gptel-cpp-complete--post-command ()
  "Post-command hook driving GPTel completion."
  (when (derived-mode-p 'c++-mode)
    (cond
     ;; accept
     ((gptel-cpp-complete--last-command-was-ret-p)
      (when (gptel-cpp-complete--overlay-active-p)
        (undo-only))
      (gptel-cpp-complete--accept-overlay))
     ;; regenerate
     ((gptel-cpp-complete--self-insert-p)
      (gptel-cpp-complete--clear-overlay)
      (gptel-cpp-complete--cancel-request)
      (gptel-cpp-complete--schedule-regenerate))
     ;; cancel
     (t
      (gptel-cpp-complete--clear-overlay)
      (gptel-cpp-complete--cancel-request)))))

(add-hook 'post-command-hook #'gptel-cpp-complete--post-command)


(provide 'gptel-cpp-complete)
;;; gptel-cpp-complete.el ends here
