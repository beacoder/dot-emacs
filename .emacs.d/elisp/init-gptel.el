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
    :key "kkkkkkkk"
    :models '(deepseek-chat deepseek-coder)))

(defconst my--coding-prompt (read "
\"You are a large language model and a careful programmer.

Available APIs:
- create file with: touch file_name
- delete file with: rm file_name
- change file with: mv old_file new_file
- open   file with: find-file file_name

Follow my instructions to generate response:
- generate code and api only, without any additional text, prompt or note.
- generate code in full, do not abbreviate or omit code.

Example:

requirement:
Please create a Python script named echo.py. The functionality of this script is as follows:
- read file content.
- print file content.
- If the file path is invalid or the file cannot be read, output an error message.

assistant:
touch echo.py

```python
def echo_file_content(file_path):
    try:
        with open(file_path, 'r') as file:
            content = file.read()
            print(content)
    except FileNotFoundError:
        print(f\\\"Error: The specified file path is invalid or the file does not exist.\\\")
    except IOError:
        print(\\\"Error: An I/O error occurred while trying to read the file.\\\")

if __name__ == \\\"__main__\\\":
    import sys
    if len(sys.argv) != 2:
        print(\\\"Usage: python echo.py <file_path>\\\")
    else:
        file_path = sys.argv[1]
        echo_file_content(file_path)
```

requirement:\"
")
  "Coding prompt.")

(defconst my-gptel--system-prompt "You are a large language model and a helpful assistant. Respond concisely."
  "System prompt.")

(defvar my-gptel--user-prompt ""
  "User prompt.")

(defvar my-gptel--use-stream-p t
  "Whether use steaming.")

(defvar my-gptel--param nil
  "prefix-arg for gptel-dwim.")

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
  (setq my-gptel--param (if (listp current-prefix-arg) (car current-prefix-arg) current-prefix-arg))
  (when-let* (my-gptel--param
              (context
               (cond
                ;; handle context as text file
                ((= my-gptel--param 7) (ignore-errors (read-file-as-string (smart/dwim-at-point))))
                ;; handle context as binary file (image)
                ;; ((= my-gptel--param 9) (ignore-errors (base64-encode-file (smart/dwim-at-point))))
                ;; handle context as text
                (t (smart/dwim-at-point)))))
    (if (= my-gptel--param 8) ;; handle context as coding requirement, ai-agent
        (setq prompt (concat my--coding-prompt "\n\n" context))
      (setq prompt (concat prompt "\n\n" context))))
  (setq my-gptel--user-prompt prompt)
  (message "Querying %s..." (gptel-backend-name gptel-backend))
  (my-gptel--request))


(provide 'init-gptel)
;;; init-gptel.el ends here
