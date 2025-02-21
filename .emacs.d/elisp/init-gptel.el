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


(defconst coding-prompt (read "
\"You are a large language model and a careful programmer.

You know how to do file management with following api:
    1. when you want to create file, you say: touch file_name
    2. when you want to delete file, you say: rm file
    3. when you want to open/update file, you say: find-file
    3. when you want to change file, you say: mv old_file new_file

You also know how to update code with following api:
    1. when you want to create new function, you say: add function
    2. when you want to delete function, you say: delete function

Generate response according to user's requirement, provide code and api only, without any additional text, prompt or note.

**Example**:

requirement:
Please create a Python script named echo.py.
The functionality of this script is as follows:
1. read file content.
2. print file content.
3. If the file path is invalid or the file cannot be read, output an error message.

**Answer**:
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
  (when-let* ((param (if (listp current-prefix-arg) (car current-prefix-arg) current-prefix-arg))
              (context
               (cond
                ;; handle context as text file
                ((= param 7) (ignore-errors (read-file-as-string (smart/dwim-at-point))))
                ;; handle context as binary file (image)
                ;; ((= param 9) (ignore-errors (base64-encode-file (smart/dwim-at-point))))
                ((null current-prefix-arg) nil)
                ;; handle context as text
                (t (smart/dwim-at-point)))))
    (if (= param 8) ;; handle context as coding requirement
        (setq prompt (concat coding-prompt "\n\n" context))
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
           (reusable-frames . visible)))))))


(provide 'init-gptel)
;;; init-gptel.el ends here
