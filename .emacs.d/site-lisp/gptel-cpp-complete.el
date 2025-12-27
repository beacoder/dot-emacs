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

;;; Install:

;; Put this file into load-path directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.d
;;
;; (require 'gptel-cpp-complete)
;; (dolist (c-mode-hook '(c-mode-common-hook c-ts-mode-hook c++-ts-mode-hook))
;;   (add-hook c-mode-hook #'gptel-cpp-complete-mode))

;;; Code:

(require 'eglot)
(require 'gptel)
(require 'treesit)
(require 'which-func)

;; ------------------------------------------------------------
;; Configuration
;; ------------------------------------------------------------
(defgroup gptel-cpp-complete nil
  "GPTel-based C++ code completion."
  :group 'tools)

(defcustom gptel-cpp-complete-idle-delay 0.25
  "Idle time before regenerating GPTel completion."
  :type 'number
  :group 'gptel-cpp-complete)

(defcustom gptel-cpp-complete-include-call-hierarchy t
  "Include caller-hierarchy, takes more time."
  :type 'boolean
  :group 'gptel-cpp-complete)

;; ------------------------------------------------------------
;; Helpers
;; ------------------------------------------------------------
(defun gptel-cpp-complete--extract-method-name (func-name)
  "Given FUNC-NAME, return a SHORT-FUNC, e.g: class::method(arg1, arg2) => method."
  (when-let* ((temp-split (split-string func-name "("))
              (short-func-with-namespace (car temp-split))
              (short-func (car (last (split-string short-func-with-namespace "::")))))
    short-func))

(defun gptel-cpp-complete--goto-function-name ()
  "Move cursor to current function name."
  (treesit-beginning-of-defun)
  (when-let* ((func-name (which-function))
              (not-empty (not (string-empty-p func-name)))
              (func-name (gptel-cpp-complete--extract-method-name func-name))
              (not-empty (not (string-empty-p func-name))))
    (search-forward func-name)))

(defun gptel-cpp-complete--safe-subseq (seq start end)
  "Safely extracts a subseq from SEQ from START to END (not included)."
  (when seq
    (cl-subseq seq start (min end (length seq)))))

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

(defun gptel-cpp-complete--call-hierarchy-item ()
  "Prepare call hierarchy item at point."
  (save-excursion
    (gptel-cpp-complete--goto-function-name)
    (when-let* ((server (eglot--current-server-or-lose))
                (pos (eglot--pos-to-lsp-position (point)))
                (params `(:textDocument (:uri ,(eglot-path-to-uri
                                                (buffer-file-name)))
                                        :position ,pos))
                (result (jsonrpc-request server
                                         :textDocument/prepareCallHierarchy
                                         params))
                (valid (not (seq-empty-p result))))
      (aref result 0))))

(defun gptel-cpp-complete--incoming-calls (item)
  "Query incomming call of ITEM."
  (when item
    (ignore-errors
      (jsonrpc-request
       (eglot--current-server-or-lose)
       :callHierarchy/incomingCalls
       `(:item ,item)))))

(defun gptel-cpp-complete--outgoing-calls (item)
  "Query outgoing call of ITEM."
  (when item
    (ignore-errors
      (jsonrpc-request
       (eglot--current-server-or-lose)
       :callHierarchy/outgoingCalls
       `(:item ,item)))))

(defun gptel-cpp-complete--snippet-from-range (uri range)
  "Extract code snippets based on URI and RANGE."
  (with-temp-buffer
    (insert-file-contents-literally (eglot-uri-to-path uri))
    (save-excursion
      (goto-char (eglot--lsp-position-to-point
                  (plist-get (aref range 0) :start)))
      (let ((beg (line-beginning-position))
            (end (line-end-position 5)))
        (buffer-substring-no-properties beg end)))))

(defun gptel-cpp-complete--format-callers (callers)
  "Format CALLERS."
  (when (and callers (not (seq-empty-p callers)))
    (string-join
     (cl-loop for call across (cl-subseq callers 0 (min 3 (length callers)))
              collect
              (gptel-cpp-complete--snippet-from-range
               (plist-get (plist-get call :from) :uri)
               (plist-get call :fromRanges)))
     "\n\n")))

(defun gptel-cpp-complete--format-callees (callees)
  "Format CALLEES."
  (when (and callees (not (seq-empty-p callees)))
    (string-join
     (cl-loop for call across (cl-subseq callees 0 (min 3 (length callees)))
              collect
              (gptel-cpp-complete--snippet-from-range
               (plist-get (plist-get call :to) :uri)
               (plist-get call :toRange)))
     "\n\n")))

(defun gptel-cpp-complete--call-hierarchy-context ()
  "Retrieve caller and callee infomation."
  (when-let* ((item (gptel-cpp-complete--call-hierarchy-item)))
    (let ((incoming (gptel-cpp-complete--incoming-calls item))
          (outgoing (gptel-cpp-complete--outgoing-calls item)))
      (cons
       (or (gptel-cpp-complete--format-callers incoming) "None")
       (or (gptel-cpp-complete--format-callees outgoing) "None")))))

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
- Callers of this function
- Callees of this function

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

(defconst gptel-cpp-complete--user-prompt
  "Current function:
```cpp
%s
```
In-scope symbols:
%s

Similar patterns in this repository:
%s

Callers of this function:
%s

Callees of this function:
%s
"
  "Completion user prompt.")

(defun gptel-cpp-complete--build-prompt ()
  "Assemble GPTel completion prompt."
  (let* ((func (or (gptel-cpp-complete--cpp-current-function) "N/A"))
         (symbols+kind (or (gptel-cpp-complete--in-scope-symbols+kind) '()))
         (symbols (or (delete-dups (mapcar #'car symbols+kind)) '()))
         (s+k (or (mapcar #'cdr symbols+kind) '()))
         (patterns (or (gptel-cpp-complete--ag-similar-patterns s+k) "None found"))
         (calls (and gptel-cpp-complete-include-call-hierarchy
                     (gptel-cpp-complete--call-hierarchy-context)))
         (incomming (or (car calls) "None found"))
         (outgoing (or (cdr calls) "None found")))
    (format gptel-cpp-complete--user-prompt
            func
            (string-join symbols ", ")
            patterns
            incomming
            outgoing)))

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

(defun gptel-cpp-complete--cancel-request ()
  "Cancel any in-flight gptel request for this buffer."
  (when gptel-cpp-complete--regenerate-timer
    (cancel-timer gptel-cpp-complete--regenerate-timer))
  (when gptel-cpp-complete--request
    (ignore-errors
      (gptel-abort gptel-cpp-complete--request))
    (setq gptel-cpp-complete--request nil)))

(defun gptel-cpp-complete--handle-response (response _info)
  "Display GPTel RESPONSE."
  (setq gptel-cpp-complete--request nil)
  (when (and response (stringp response))
    (message "")
    (gptel-cpp-complete--show-overlay response)))

(defun gptel-cpp-complete--fire-request ()
  "Start a new AI completion request, canceling any in-flight one."
  (setq gptel-cpp-complete--request
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

;; ------------------------------------------------------------
;; Mode Definition
;; ------------------------------------------------------------
(defvar gptel-cpp-complete-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c TAB") #'gptel-cpp-complete)
    map))

(define-minor-mode gptel-cpp-complete-mode
  "Mode for ai-assisted C++ completion powered by eglot + gptel."
  :init-value nil :lighter nil :keymap gptel-cpp-complete-mode-map :interactive nil
  (if gptel-cpp-complete-mode
      (progn
        (add-hook 'post-command-hook #'gptel-cpp-complete--post-command nil t)
        (setq eglot-extend-to-xref t))
    (remove-hook 'post-command-hook #'gptel-cpp-complete--post-command t)
    (gptel-cpp-complete--clear-overlay)
    (gptel-cpp-complete--cancel-request)))


(provide 'gptel-cpp-complete)
;;; gptel-cpp-complete.el ends her
