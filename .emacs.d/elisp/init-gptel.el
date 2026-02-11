;;; init-gptel.el --- config gptel for querying llm. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ============================================================================
;; Package Configuration
;; ============================================================================

;;; LLM client
;;  "gptel"         => start a LLM session
;;  "C-c RET"       => send to LLM
;;  "gptel-send"    => send to LLM
;;  "gptel-rewrite" => Rewrite, refactor
(use-package gptel
  :ensure t
  :config
  (progn
    (setq gptel-log-level 'info
          gptel-confirm-tool-calls nil
          gptel-model 'deepseek-ai/DeepSeek-V3.2
          ;; Randomness in response text, 0 to 2
          gptel-temperature 0
          gptel-backend
          ;; free 2000 request per-day, each model 500
          (gptel-make-openai "Free"
            :host "api-inference.modelscope.cn"
            :stream t
            :key ""
            :models '(Qwen/Qwen2.5-32B-Instruct
                      deepseek-ai/DeepSeek-V3.2)))
    (gptel-make-deepseek "DeepSeek"
      :stream t
      :key "")
    (gptel-make-ollama "Ollama"
      :stream t
      :models '(qwen3:8b))))

;; ============================================================================
;; Additional Packages
;; ============================================================================

(use-package gptel-cpp-complete
  :ensure t
  :config
  (when (display-graphic-p)
    (dolist (c-mode-hook '(c-mode-common-hook c-ts-mode-hook c++-ts-mode-hook))
      (add-hook c-mode-hook #'gptel-cpp-complete-mode))))

(use-package gptel-agent
  :ensure t
  :config
  (progn
    (gptel-agent-update)
    ;; use git-grep since it's much faster in git repo
    (defalias 'gptel-agent--grep 'gptel-agent--git-grep)
    (defun gptel-agent--git-grep (regex path &optional glob context-lines)
      (unless (file-readable-p path)
        (error "Error: File or directory %s is not readable" path))
      (let* ((path (expand-file-name (substitute-in-file-name path)))
             (git-root (vc-git-root path))
             (grepper "git"))
        (with-temp-buffer
          (let* ((default-directory (or git-root default-directory))
                 (args
                  (pcase grepper
                    ("git"
                     (delq nil
                           (append
                            (list "grep"
                                  "--line-number"
                                  "--no-color"
                                  (and (natnump context-lines)
                                       (format "-C%d" context-lines))
                                  "--max-count=1000"
                                  ;; pcre regex
                                  "-P" regex
                                  "--")
                            ;; glob restriction
                            (when glob
                              (list (format "%s" glob)))))))))
            (message (format "running: git %s" (mapconcat 'identity args " ")))
            (let ((exit-code (apply #'call-process grepper nil '(t t) nil args)))
              (when (and (/= exit-code 0)
                         ;; git grep returns 1 if no matches
                         (not (and (string= grepper "git") (= exit-code 1))))
                (goto-char (point-min))
                (insert (format "Error: search failed with exit-code %d\n\n" exit-code)))
              (buffer-string))))))
    ;; use git-ls-files since it's much faster in git repo
    (defalias 'gptel-agent--glob 'gptel-agent---git-glob)
    (defun gptel-agent---git-glob (pattern &optional path depth)
      (when (string-empty-p pattern)
        (error "Error: pattern must not be empty"))
      (if path
          (unless (and (file-readable-p path) (file-directory-p path))
            (error "Error: path %s is not readable" path))
        (setq path "."))
      (unless (or (executable-find "git") (executable-find "tree"))
        (error "Error: Neither `git` nor `tree` not found.  This tool cannot be used"))
      (let* ((full-path (expand-file-name path))
             (is-git (and (executable-find "git")
                          (zerop (call-process "git" nil nil nil "-C" full-path "rev-parse" "--is-inside-work-tree")))))
        (with-temp-buffer
          (if is-git
              ;; --- Git Strategy ---
              (let* ((default-directory full-path)
                     (exit-code
                      (apply #'call-process "git" nil t nil
                             "ls-files" "-z"
                             "--full-name"
                             "--cached"      ; Tracked files
                             "--others"      ; Untracked files
                             "--exclude-standard" ; Respect .gitignore
                             (list (concat "*" pattern "*")))))
                (if (/= exit-code 0)
                    (progn (goto-char (point-min))
                           (insert (format "Glob failed with exit code %d\n.STDOUT:\n\n"
                                           exit-code)))
                  ;; Convert null-terminated strings to newline-separated full paths
                  (goto-char (point-min))
                  (while (search-forward "\0" nil t)
                    (replace-match "\n"))
                  ;; Prepend the path to make them absolute
                  (goto-char (point-min))
                  (let ((path-prefix (file-name-as-directory full-path)))
                    (while (not (eobp))
                      (unless (looking-at-p "^$") ; Skip empty lines
                        (insert path-prefix))
                      (forward-line 1)))))
            ;; --- Tree Strategy (Fallback) ---
            (let* ((args (list "-l" "-f" "-i" "-I" ".git"
                               "--sort=mtime" "--ignore-case"
                               "--prune" "-P" pattern full-path))
                   (args (if (natnump depth)
                             (nconc args (list "-L" (number-to-string depth)))
                           args))
                   (exit-code (apply #'call-process "tree" nil t nil args)))
              (when (/= exit-code 0)
                (goto-char (point-min))
                (insert (format "Glob failed with exit code %d\n.STDOUT:\n\n"
                                exit-code)))))
          (when (> (buffer-size) 20000)
            ;; Too large - save to temp file and return truncated info
            (let* ((temp-dir (expand-file-name "gptel-agent-temp"
                                               (temporary-file-directory)))
                   (temp-file (expand-file-name
                               (format "glob-%s-%s.txt"
                                       (format-time-string "%Y%m%d-%H%M%S")
                                       (random 10000))
                               temp-dir)))
              (unless (file-directory-p temp-dir) (make-directory temp-dir t))
              (write-region nil nil temp-file)
              (let ((max-lines 50)
                    (orig-size (buffer-size))
                    (orig-lines (line-number-at-pos (point-max))))
                ;; Insert header
                (goto-char (point-min))
                (insert (format "Glob results too large (%d chars, %d lines)\
 for context window.\nStored in: %s\n\nFirst %d lines:\n\n"
                                orig-size orig-lines temp-file max-lines))
                ;; Truncate to first max-lines lines
                (forward-line max-lines)
                (delete-region (point) (point-max))
                ;; Insert footer
                (goto-char (point-max))
                (insert (format "\n\n[Use Read tool with file_path=\"%s\" to view full results]"
                                temp-file)))))
          (buffer-string))))
    ;; add project related information as llm context, e.g: coding guideline, etc.
    (require 'gptel-context)
    (gptel-context--add-directory (expand-file-name "~/.emacs.d/contexts") 'add)))

;; ============================================================================
;; Custom Prompts and Variables
;; ============================================================================

(defconst my-gptel--default-prompt
  "You are a large language model and a helpful assistant. Respond concisely."
  "Default system prompt for general QA tasks.")

(defvar my-gptel--user-prompt ""
  "Current user prompt for gptel requests.")

;; ============================================================================
;; Core Functions
;; ============================================================================

(defun my-gptel--request ()
  "Initiate a gptel request with the current user prompt."
  (gptel--sanitize-model)
  (gptel-request my-gptel--user-prompt
    :system my-gptel--default-prompt
    :stream t
    :callback #'my-gptel--response-callback))

(defun my-gptel--response-callback (response info)
  "Callback function for gptel requests.
RESPONSE is the LLM response text.
INFO contains metadata about the request."
  (if (not response)
      (message "gptel-dwim failed with message: %s" (plist-get info :status))
    (display-buffer
     (with-current-buffer (get-buffer-create "*LLM response*")
       (let ((inhibit-read-only t))
         (deactivate-mark)
         (visual-line-mode 1)
         (goto-char (point-max))
         (ignore-errors
           (insert response))
         (markdown-mode)
         (gptel-mode)
         (current-buffer)))
     '((display-buffer-reuse-window
        display-buffer-pop-up-window)
       (reusable-frames . visible)))))

(defun gptel-dwim (prompt)
  "Request a response from the `gptel-backend' for PROMPT.
The request is asynchronous, this function returns immediately.

If PROMPT is:
- current-prefix-arg enabled, create a full prompt from both minibuffer
  and active_region/symbol_at_point suitable for sending to the LLM.
- a string, it is used to create a full prompt suitable for
  sending to the LLM."
  (declare (indent 1))
  (interactive (list (smart/read-from-minibuffer "Ask ChatGPT")))

  (let ((local-prefix-arg
         (if (listp current-prefix-arg) (car current-prefix-arg) current-prefix-arg))
        (context (smart/dwim-at-point)))

    ;; Add context from active region or symbol at point if prefix arg is given
    (when local-prefix-arg
      (and context
           (setq prompt (concat prompt "\n\n" context))))

    (setq my-gptel--user-prompt prompt)
    (message "Querying %s..." (gptel-backend-name gptel-backend))
    (my-gptel--request)))

;; ============================================================================
;; Preset Configurations
;; ============================================================================

;;; Preset selection
;;  "gtpel-qa"         => general QA/code-completion
;;  "gptel-plan"       => handle complicated tasks with read-only tools
;;  "gptel-agent"      => handle complicated tasks

;; Preset for general QA tasks
(gptel-make-preset 'gptel-qa
  :description "A preset optimized for general QA tasks"
  :backend "Free"
  :model 'deepseek-ai/DeepSeek-V3.2
  :stream t
  :system my-gptel--default-prompt
  :tools nil
  :temperature 1)

;; ============================================================================
;; Provide the module
;; ============================================================================

(provide 'init-gptel)
;;; init-gptel.el ends here
