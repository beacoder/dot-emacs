;;; init-gptel-tools.el --- config gptel tools for gptel to use. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Define tool-callbacks
;; @note return either a result or a message to inform the LLM
(progn
  (defun my-gptel--run_async_command (callback command)
    "Run COMMAND asynchronously and pass output to CALLBACK."
    (let ((buffer (generate-new-buffer " *async output*")))
      (async-shell-command command buffer nil) ; Disable auto-display
      (let ((proc (get-buffer-process buffer)))
        (when proc
          (set-process-sentinel
           proc
           (λ (process _event)
             (unless (process-live-p process)
               (with-current-buffer (process-buffer process)
                 (let ((output (buffer-substring-no-properties (point-min) (point-max))))
                   (kill-buffer (current-buffer))
                   (funcall callback output))))))))))
  
  (defun my-gptel--read_file(filepath)
    (with-temp-message (format "Reading file: %s" filepath)
      (with-temp-buffer
        (insert-file-contents (expand-file-name filepath))
        (buffer-string))))
  
  (defun my-gptel--create_file(path filename content)
    (let ((full-path (expand-file-name filename path)))
      (with-temp-buffer
        (insert content)
        (write-file full-path))
      (format "Created file %s in %s" filename path)))

  (defun my-gptel--open_file(filepath)
    (find-file (expand-file-name filepath))
    (format "Opened file %s" filepath))

  (defun my-gptel--run_script (script_program script_file script_args)
    (let ((command
           (concat script_program " " (expand-file-name script_file) " " script_args)))
      (with-temp-message (format "Running script: %s" command)
        (shell-command-to-string command))))

  (defun my-gptel--append_to_buffer (buffer text)
    (with-current-buffer (get-buffer-create buffer)
      (save-excursion
        (goto-char (point-max))
        (insert text)))
    (format "Appended text to buffer %s" buffer))

  (defun my-gptel--read_buffer (buffer)
    (with-temp-message "Reading buffer"
      (unless (buffer-live-p (get-buffer buffer))
        (error "Error: buffer %s is not live." buffer))
      (with-current-buffer  buffer
        (buffer-substring-no-properties (point-min) (point-max)))))

  (defun my-gptel--make_directory (parent name)
    (condition-case nil
        (progn
          (make-directory (expand-file-name name parent) t)
          (format "Directory %s created/verified in %s" name parent))
      (error (format "Error creating directory %s in %s" name parent)))))

;; Holds a list of tools available for LLM to use
;; @see https://github.com/karthink/gptel/issues/514
(progn
  (gptel-make-tool
   :function (lambda (url)
               (with-current-buffer (url-retrieve-synchronously url)
                 (goto-char (point-min)) (forward-paragraph)
                 (let ((dom (libxml-parse-html-region (point) (point-max))))
                   (run-at-time 0 nil #'kill-buffer (current-buffer))
                   (with-temp-buffer
                     (shr-insert-document dom)
                     (buffer-substring-no-properties (point-min) (point-max))))))
   :name "read_url"
   :description "Fetch and read the contents of a URL."
   :args (list '(:name "url"
                       :type "string"
                       :description "The URL to read"))
   :category "web")

  (gptel-make-tool
   :function #'my-gptel--append_to_buffer
   :name "append_to_buffer"
   :description "Append text to the an Emacs buffer.  If the buffer does not exist, it will be created."
   :args (list '(:name "buffer"
                       :type "string"
                       :description "The name of the buffer to append text to.")
               '(:name "text"
                       :type "string"
                       :description "The text to append to the buffer."))
   :category "emacs")

  (gptel-make-tool
   :function #'my-gptel--read_buffer
   :name "read_buffer"
   :description "Return the contents of an Emacs buffer."
   :args (list '(:name "buffer"
                       :type "string"
                       :description "The name of the buffer whose contents are to be retrieved"))
   :category "emacs")

  (gptel-make-tool
   :function (lambda (directory) (mapconcat #'identity (directory-files directory) "\n"))
   :name "list_directory"
   :description "List the contents of a given directory."
   :args (list '(:name "directory"
                       :type "string"
                       :description "The path to the directory to list"))
   :category "filesystem")

  (gptel-make-tool
   :function #'my-gptel--make_directory
   :name "make_directory"
   :description "Create a new directory with the given name in the specified parent directory."
   :args (list '(:name "parent"
                       :type "string"
                       :description "The parent directory where the new directory should be created, e.g. /tmp")
               '(:name "name"
                       :type "string"
                       :description "The name of the new directory to create, e.g. testdir"))
   :category "filesystem")

  (gptel-make-tool
   :function #'my-gptel--create_file
   :name "create_file"
   :description "Create a new file with the specified content."
   :args (list '(:name "path"
                       :type "string"
                       :description "The directory where to create the file")
               '(:name "filename"
                       :type "string"
                       :description "The name of the file to create")
               '(:name "content"
                       :type "string"
                       :description "The content to write to the file"))
   :category "filesystem")

  (gptel-make-tool
   :function #'my-gptel--read_file
   :name "read_file"
   :description "Read and display the contents of a file."
   :args (list '(:name "filepath"
                       :type "string"
                       :description "Path to the file to read.  Supports relative paths and ~."))
   :category "filesystem")
  
  (gptel-make-tool
   :function #'my-gptel--open_file
   :name "open_file"
   :description "Open and display the contents of a file."
   :args (list '(:name "filepath"
                       :type "string"
                       :description "Path to the file to open.  Supports relative paths and ~."))
   :category "filesystem")

  (gptel-make-tool
   :function #'my-gptel--run_script
   :name "run_script"
   :description "Run the script along with its specified arguments using the program."
   :args (list
          '(:name "script_program"
                  :type "string"
                  :description "Program to run the the script.")
          '(:name "script_file"
                  :type "string"
                  :description "Path to the script to run.  Supports relative paths and ~.")
          '(:name "script_args"
                  :type "string"
                  :description "Args for script to run."))
   :category "command")

  ;; super-powerful, capable of replacing numerous existing tools.
  (gptel-make-tool
   :function (lambda (command)
               (with-temp-message (format "Running command: %s" command)
                 (shell-command-to-string command)))
   :name "run_command"
   :description "Run a command."
   :args (list
          '(:name "command"
                  :type "string"
                  :description "Command to run."))
   :category "command")

  ;; async-tools make multiple agents possible.
  (gptel-make-tool
   :function #'my-gptel--run_async_command
   :name "run_async_command"
   :description "Run an async command."
   :args (list
          '(:name "command"
                  :type "string"
                  :description "Command to run."))
   :category "command"
   :async t
   :include t))


(provide 'init-gptel-tools)
;;; init-gptel-tools ends here
