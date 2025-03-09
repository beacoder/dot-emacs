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
    :models '(qwen2.5:latest
              deepseek-r1:7b)))

(require 'init-gptel-tools)

(defconst my-gptel--tool-prompt
  "You are an AI assistant equipped with a set of tools to help complete tasks.
For each task provided, follow these steps:
1.Understand the task: Carefully analyze the task requirements.
2.Select the appropriate tool: Choose the most suitable tool from the provided list to accomplish the task.
3.Execute the task: Use the selected tool to perform the task step-by-step.
4.Verify the output: Ensure the result meets the task's requirements.
5.Proceed to the next task: Move on to the next task only after completing the current one.
Tasks:"
  "Tool prompt.")

(defconst my-gptel--system-prompt "You are a large language model and a helpful assistant. Respond concisely."
  "System prompt.")

(defvar my-gptel--user-prompt ""
  "User prompt.")

(defvar my-gptel--use-stream-p t
  "Whether use steaming.")

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

;; enable gptel logging
(setq gptel-log-level 'info)


(provide 'init-gptel)
;;; init-gptel.el ends here
