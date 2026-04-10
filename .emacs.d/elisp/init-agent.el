;;; init-agent.el --- config llm agents. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ============================================================================
;; Package Configuration
;; ============================================================================

(defmacro gptel-define-agent (name mcp-servers)
  "Define a gptel agent function named gptel-NAME connecting to MCP-SERVERS."
  (let ((func-name (intern (format "gptel-%s" name)))
        (agent-name (format "gptel-%s" name)))
    `(defun ,func-name (&optional project-dir)
       (interactive
        (list (if-let ((proj (project-current)))
                  (project-root proj)
                default-directory)))
       (progn
         (require 'gptel-integrations)
         (gptel-mcp-connect ',mcp-servers)
         (while (not (gptel-mcp--get-tools ',mcp-servers))
           (sleep-for 0.1)))
       (let ((gptel-use-tools t)
             (gptel-tools gptel-tools)
             (gptel-buf
              (gptel (generate-new-buffer-name
                      (format ,(format "*%s:%%s*" agent-name)
                              (cadr (nreverse (file-name-split project-dir)))))
                     nil
                     (and (use-region-p)
                          (buffer-substring (region-beginning) (region-end)))
                     'interactive)))
         (with-current-buffer gptel-buf
           (setq default-directory project-dir)
           (gptel-agent-update)
           (when-let* ((gptel-agent-plist
                        (assoc-default ,agent-name gptel-agent--agents nil nil)))
             (apply #'gptel-make-preset ',func-name gptel-agent-plist))
           (gptel--apply-preset
            ',func-name
            (lambda (sym val) (set (make-local-variable sym) val)))
           (unless gptel-max-tokens
             (setq gptel-max-tokens 8192)))))))

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
                                 "--untracked"
                                 ;; pcre regex
                                 "-P" regex
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
    ;; define gptel-telegram agent
    (gptel-define-agent telegram ("chrome"))
))

;; ============================================================================
;; Provide the module
;; ============================================================================

(provide 'init-agent)
;;; init-agent.el ends here
