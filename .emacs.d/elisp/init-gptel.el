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

(use-package gptel-agent
  :ensure t
  :config
  (gptel-agent-update))

(use-package gptel-cpp-complete
  :ensure t
  :config
  (when (display-graphic-p)
    (dolist (c-mode-hook '(c-mode-common-hook c-ts-mode-hook c++-ts-mode-hook))
      (add-hook c-mode-hook #'gptel-cpp-complete-mode))))

;; ============================================================================
;; Custom Prompts and Variables
;; ============================================================================

(defconst my-gptel--default-prompt
  "You are a large language model and a helpful assistant. Respond concisely."
  "Default system prompt for general QA tasks.")

(defconst my-gptel--tool-prompt
  "You are an AI assistant with access to external tools.

Your objective is to complete the user's task correctly and efficiently using the available tools when necessary.

GENERAL RULES:
- Use tools ONLY when they are required to complete the task.
- NEVER invent tools, arguments, or outputs.
- If no tool is suitable, respond directly in natural language.
- Use one tool at a time unless the task explicitly requires chaining.
- Do not expose internal reasoning or analysis.

TASK EXECUTION:
1. Analyze the user's request and determine whether a tool is needed.
2. If a tool is needed, select the most appropriate one.
3. Invoke the tool with precise and valid arguments.
4. Evaluate the tool result silently.
5. If the result is incorrect or incomplete, adjust and retry.
6. Only proceed once the current step is successfully completed.

FAILURE HANDLING:
- If a tool fails, explain the failure briefly and either retry or ask for clarification.
- If the task cannot be completed with the available tools, state this clearly.

OUTPUT:
- Provide concise, task-focused responses.
- Do not describe intermediate steps or reasoning unless explicitly asked.

/no_think"
  "System prompt for tool-calling tasks.")

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

;; Preset for coding tasks with tool support
(gptel-make-preset 'gptel-coding
  :description "A preset optimized for coding tasks"
  :backend "DeepSeek"
  :model 'deepseek-chat
  :stream t
  :system my-gptel--tool-prompt
  ;; Include only essential tools to increase tool-calling success rate
  :tools '("Bash" "Mkdir" "Write" "Read" "Edit" "Insert")
  :temperature 0)

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
