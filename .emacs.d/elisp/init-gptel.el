;;; init-gptel.el --- config gptel for querying llm. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; LLM client
;;  "gptel"         => start a LLM session
;;  "C-c RET"       => send to LLM
;;  "gptel-send"    => send to LLM
;;  "gptel-rewrite" => Rewrite, refactor
(use-package gptel
  :ensure t
  :config
  (setq gptel-model   'deepseek-reasoner
        gptel-backend (gptel-make-deepseek "DeepSeek"
                        :stream t
                        :key "deepseek-api-key"))
  (gptel-make-ollama "Ollama"
    :stream t
    :models '(qwen2.5:latest)))

(require 'init-gptel-tools)

(setq gptel-log-level 'info)

(defconst my-gptel--completion-prompt
  "You are an expert %s programmer.
Follow my instructions to complete the following %s code snippet in a clean, efficient, and idiomatic way.
1.Ensure the code is functional, well-formatted, and follows best practices for the given programming language.
2.Generate ONLY %s code as output, without any explanation or markdown code fences.
3.Generate code in full, do not abbreviate or omit code.
4.Generate completion code only, do not repeat the original code.
5.Do not ask for further clarification, and make any assumptions you need to follow instructions.\n\n
%s"
  "Completion prompt.")

(defconst my-gptel--tool-prompt
  "You are an AI assistant equipped with a set of tools to complete tasks.
Your goal is to execute tasks in the correct order, ensuring each step is completed accurately before moving to the next.
Follow these instructions precisely:
1.Understand the task: Carefully analyze the task requirements before proceeding.
2.Select the appropriate tool: Choose the most suitable tool from the provided list to accomplish the task.
3.Execute the task: Use the selected tool to perform the task step-by-step.
4.Verify the output: Check if the result meets the task's requirements. If not, retry or adjust your approach.
5.Proceed to the next task: Only move to the next task after successfully completing the current one.\n\n
Tasks:\n"
  "Tool prompt.")

(defconst my-gptel--system-prompt "You are a large language model and a helpful assistant. Respond concisely."
  "System prompt.")

(defvar my-gptel--user-prompt ""
  "User prompt.")

(defvar my-gptel--use-stream-p t
  "Whether use steaming.")

(defvar my-gptel--completion-position nil
  "Current completion position.")

(defvar my-gptel--completion-buffer nil
  "Buffer for code completion.")

(defun my-gptel--request ()
  "Initiate gptel request."
  (gptel--sanitize-model)
  (gptel-request my-gptel--user-prompt
    :system my-gptel--system-prompt
    :stream my-gptel--use-stream-p
    :callback #'my-gptel--response-callback))

(defun my-gptel-retry ()
  "Retry previous gptel request."
  (interactive)
  (my-gptel--request))

(defun my-gptel--completion-callback (response info)
  "Callback function for gptel complete."
  (if (not response)
      (message "gptel-complete failed with message: %s" (plist-get info :status))
    (display-buffer
     (with-current-buffer my-gptel--completion-buffer
       (let ((inhibit-read-only t))
         (deactivate-mark)
         (visual-line-mode 1)
         (goto-char my-gptel--completion-position)
         (ignore-errors(insert response))
         (setq my-gptel--completion-position (point))
         (current-buffer)))
     '((display-buffer-reuse-window
        display-buffer-pop-up-window)
       (reusable-frames . visible)))))

(defun my-gptel--response-callback (response info)
  "Callback function for gptel request."
  (if (not response)
      (message "gptel-dwim failed with message: %s" (plist-get info :status))
    (display-buffer
     (with-current-buffer (get-buffer-create "*LLM response*")
       (let ((inhibit-read-only t))
         (deactivate-mark)
         (visual-line-mode 1)
         (goto-char (point-max))
         (ignore-errors (insert response))
         (current-buffer)))
     '((display-buffer-reuse-window
        display-buffer-pop-up-window)
       (reusable-frames . visible)))))

(defun gptel-dwim (prompt)
  "Request a response from the `gptel-backend' for PROMPT.
The request is asynchronous, this function returns immediately.
If PROMPT is
- current-prefix-arg enabled, create a full prompt from both minibuffer
  and active_region/symbol_at_point suitable for sending to the LLM.
- a string, it is used to create a full prompt suitable for
  sending to the LLM."
  (declare (indent 1))
  (interactive (list (smart/read-from-minibuffer "Ask ChatGPT")))
  (let ((local-prefix-arg
         (if (listp current-prefix-arg) (car current-prefix-arg) current-prefix-arg))
        (context (smart/dwim-at-point)))
    (when local-prefix-arg
      (if (= local-prefix-arg 8)
          ;; e.g: qwen support tool-use.
          ;; handle tool-use: add context for prompt
          (if (and prompt (not (string= prompt "")))
              (setq prompt (concat my-gptel--tool-prompt "\n\n" prompt))
            (setq prompt (concat my-gptel--tool-prompt "\n\n" context)))
        ;; otherwise: add prompt for context
        (and context (setq prompt (concat prompt "\n\n" context))))))
  (setq my-gptel--user-prompt prompt)
  (message "Querying %s..." (gptel-backend-name gptel-backend))
  (my-gptel--request))

(defun my-gptel-complete ()
  "Code completion."
  (interactive)
  (gptel--sanitize-model)
  (unless (use-region-p)
    (user-error "`gptel-complete' requires an active region"))
  (when (derived-mode-p 'prog-mode)
    (let* ((gptel-model 'qwen2.5:latest)
           (gptel-backend (gptel-get-backend "Ollama"))
           (lang (downcase (gptel--strip-mode-suffix major-mode)))
           (code (buffer-substring (region-beginning) (region-end)))
           (prompt (format my-gptel--completion-prompt lang lang lang code)))
      (setq my-gptel--completion-position (region-end)
            my-gptel--completion-buffer (current-buffer))
      (message "Completing with %s..." (gptel-backend-name gptel-backend))
      (gptel-request prompt
        :system my-gptel--system-prompt
        :stream my-gptel--use-stream-p
        :callback #'my-gptel--completion-callback))))

(global-set-key (kbd "C-c <TAB>") #'my-gptel-complete)


(provide 'init-gptel)
;;; init-gptel.el ends here
