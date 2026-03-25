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

(defun gptel-agent--git-glob (pattern &optional path depth)
  (when (string-empty-p pattern)
    (error "Error: pattern must not be empty"))
  (if path
      (unless (and (file-readable-p path) (file-directory-p path))
        (error "Error: path %s is not readable" path))
    (setq path "."))
  (unless (executable-find "tree")
    (error "Error: Executable `tree` not found.  This tool cannot be used"))
  (let* ((full-path (expand-file-name path))
         (git-root
          (and (executable-find "git") (locate-dominating-file full-path ".git"))))
    (with-temp-buffer
      (if git-root
          ;; --- Git Strategy ---
          (let* ((default-directory git-root)
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
      (gptel-agent--truncate-buffer "glob")
      (buffer-string))))

(defun gptel-agent--git-grep (regex path &optional glob context-lines)
  (unless (file-readable-p path)
    (error "Error: File or directory %s is not readable" path))
  (let ((grepper (or (executable-find "rg") (executable-find "grep"))))
    (unless grepper
      (error "Error: ripgrep/grep not available, this tool cannot be used"))
    (with-temp-buffer
      (let* ((full-path (expand-file-name (substitute-in-file-name path)))
             (git-root (and (executable-find "git") (locate-dominating-file full-path ".git")))
             (cmd (file-name-sans-extension (file-name-nondirectory grepper)))
             (grepper (if git-root "git" grepper))
             (default-directory (or git-root default-directory))
             (args
              (cond
               (git-root
                (delq nil (append
                           (list "grep"
                                 "--line-number"
                                 "--no-color"
                                 (and (natnump context-lines)
                                      (format "-C%d" context-lines))
                                 "--max-count=1000"
                                 ;; pcre regex
                                 "-P" regex
                                 "--untracked"
                                 "--")
                           ;; glob restriction
                           (when glob
                             (list (format "%s" glob))))))
               ((string= "rg" cmd)
                (delq nil (list "--sort=modified"
                                (and (natnump context-lines)
                                     (format "--context=%d" context-lines))
                                (and glob (format "--glob=%s" glob))
                                ;; "--files-with-matches"
                                "--max-count=1000"
                                "--heading" "--line-number" "-e" regex
                                full-path)))
               ((string= "grep" cmd)
                (delq nil (list "--recursive"
                                (and (natnump context-lines)
                                     (format "--context=%d" context-lines))
                                (and glob (format "--include=%s" glob))
                                "--max-count=1000"
                                "--line-number" "--regexp" regex
                                full-path)))
               (t (error "Error: failed to identify grepper"))))
             (exit-code (apply #'call-process grepper nil '(t t) nil args)))
        (when (/= exit-code 0)
          (goto-char (point-min))
          (insert (format "Error: search failed with exit-code %d.  Tool output:\n\n" exit-code)))
        (gptel-agent--truncate-buffer "grep")
        (buffer-string)))))

(use-package gptel-agent
  :ensure t
  :config
  (progn
    (gptel-agent-update)
    ;; use git-grep since it's much faster in git repo
    (defalias 'gptel-agent--grep 'gptel-agent--git-grep)
    ;; use git-ls-files since it's much faster in git repo
    (defalias 'gptel-agent--glob 'gptel-agent--git-glob)
    ;; add project related information as llm context, e.g: coding guideline, etc.
    (require 'gptel-context)
    (gptel-add-file (expand-file-name "~/.emacs.d/contexts"))
    ;; add agent skills, e.g: https://github.com/anthropics/skills
    (add-to-list 'gptel-agent-skill-dirs "~/.emacs.d/skills")
    ;; add gptel-telegram agent
    (add-to-list 'gptel-agent-dirs "~/.emacs.d/agents")
    ;; start gptel-telegram agent
    (defun gptel-telegram (&optional project-dir)
      (interactive
       (list (if-let ((proj (project-current)))
                 (project-root proj)
               default-directory)))
      (let ((gptel-buf
             (gptel (generate-new-buffer-name
                     (format "*gptel-telegram:%s*"
                             (cadr (nreverse (file-name-split project-dir)))))
                    nil
                    (and (use-region-p)
                         (buffer-substring (region-beginning)
                                           (region-end)))
                    'interactive)))
        (with-current-buffer gptel-buf
          (setq default-directory project-dir)
          (gptel-agent-update)              ;Update all agent definitions
          ;; Apply gptel-agent preset if it exists
          (when-let* ((gptel-agent-plist (assoc-default "gptel-telegram" gptel-agent--agents nil nil)))
            (apply #'gptel-make-preset 'gptel-telegram gptel-agent-plist))
          (gptel--apply-preset              ;Apply the gptel-agent preset
           'gptel-telegram
           (lambda (sym val) (set (make-local-variable sym) val)))
          (unless gptel-max-tokens          ;Agent tasks typically need a higher than usual value
            (setq gptel-max-tokens 8192)))))))

(defun my/gptel-clean-temp-files ()
  "Remove old gptel temp files owned by current user."
  (let* ((tmpdir temporary-file-directory)
         (patterns '("^gptel-curl" "^gptel-agent"))
         (max-age (* 30 60))
         (uid (user-uid))
         (deleted-count 0))
    (dolist (file (directory-files tmpdir t))
      (when (and (file-exists-p file)
                 (cl-some (lambda (pat)
                            (string-match-p pat (file-name-nondirectory file)))
                          patterns))
        (let* ((attrs (file-attributes file))
               (owner (nth 2 attrs)) ;; uid
               (mtime (nth 5 attrs))
               (age (float-time (time-subtract (current-time) mtime))))
          (when (and (eq owner uid)
                     (> age max-age))
            (ignore-errors
              (if (file-directory-p file)
                  (delete-directory file t)
                (delete-file file))
              (setq deleted-count (1+ deleted-count)))))))
    (when (> deleted-count 0)
      (message "[%s] Cleaned up %d old gptel temp files"
               (format-time-string "%Y-%m-%d %H:%M:%S")
               deleted-count))))

;; clean temporary files generated by gptel
(run-at-time "0" 600 #'my/gptel-clean-temp-files)

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
