;;; init-agent.el --- config llm agents. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ============================================================================
;; Package Configuration
;; ============================================================================

(defmacro gptel-define-agent (name mcp-servers)
  "Define a gptel agent function with gptel-name as NAME and connect it to MCP-SERVERS."
  (let ((func-name (intern (format "gptel-%s" name)))
        (agent-name (format "gptel-%s" name)))
    `(defun ,func-name (&optional project-dir)
       (interactive
        (list (if-let ((proj (project-current)))
                  (project-root proj)
                default-directory)))
       (when ',mcp-servers
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
            (lambda (sym val) (set (make-local-variable sym) val))))))))

(use-package gptel-agent
  :ensure t
  :config
  (progn
    (gptel-agent-update)
    ;; add project related information into llm context, e.g: coding guideline, etc.
    (require 'gptel-context)
    (gptel-add-file (expand-file-name "~/.emacs.d/contexts"))
    ;; add agent skills, e.g: https://github.com/anthropics/skills
    (add-to-list 'gptel-agent-skill-dirs "~/.emacs.d/skills")
    ;; replace with my own agent
    (setq gptel-agent-dirs '("~/.emacs.d/agents"))
    ;; define and use gptel-telegram
    (gptel-define-agent telegram ("chrome"))
    ;; define and use gptel-opencode-agent
    (gptel-define-agent opencode-agent nil)
    (fset 'gptel-agent #'gptel-opencode-agent)
    ;; suppress gptel warning
    (add-to-list 'warning-suppress-types '(gptel))
    (require 'gptel-agent-harness)
    (gptel-agent-harness-mode 1)
    ;; add task-completion-rules into llm context
    (gptel-add-file
     (expand-file-name
      "task-completion-rules.md"
      (file-name-directory
       (or (locate-library "gptel-agent-harness")
           (error "gptel‑agent‑harness not found")))))
    (add-to-list 'gptel-agent-harness-context-windows '("openai/gpt-oss-120b" . 128000))))

;; ============================================================================
;; Provide the module
;; ============================================================================

(provide 'init-agent)
;;; init-agent.el ends here
