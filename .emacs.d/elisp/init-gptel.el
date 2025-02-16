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
  (setq
   gptel-model 'qwen2.5:latest
   gptel-backend (gptel-make-ollama "Ollama"
                   :stream t
                   :models '(qwen2.5:latest
                             deepseek-r1:7b)))
  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key "deepseek_api_key"
    :models '(deepseek-chat deepseek-coder)))

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
  (when (string= prompt "") (user-error "A prompt is required."))
  (when current-prefix-arg
    (when-let ((context (smart/dwim-at-point)))
      (setq prompt (concat prompt "\n\n" context))))
  (message "Querying %s..." (gptel-backend-name gptel-backend))
  (gptel--sanitize-model)
  (gptel-request
      prompt
    :system "You are a large language model and a helpful assistant. Respond concisely."
    :stream t
    :callback
    (lambda (response info)
      (if (not response)
          (message "gptel-dwim failed with message: %s" (plist-get info :status))
        (with-current-buffer (get-buffer-create "*gptel-dwim*")
          (let ((inhibit-read-only nil))
            (goto-char (point-max))
            (deactivate-mark)
            (ignore-errors (insert response))
            (display-buffer
             (current-buffer)
             '((display-buffer-reuse-window
                display-buffer-pop-up-window)
               (reusable-frames . visible)))))))))


(provide 'init-gptel)
;;; init-gptel.el ends here
