;;; init-gptel-tools.el --- config gptel tools for gptel to use. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
   :description "Fetch and read the contents of a URL"
   :args (list '(:name "url"
                       :type "string"
                       :description "The URL to read"))
   :category "web")

  (gptel-make-tool
   :function (lambda (buffer text)
               (with-current-buffer (get-buffer-create buffer)
                 (save-excursion
                   (goto-char (point-max))
                   (insert text)))
               (format "Appended text to buffer %s" buffer))
   :name "append_to_buffer"
   :description "Append text to the an Emacs buffer.  If the buffer does not exist, it will be created."
   :args (list '(:name "buffer"
                       :type "string"
                       :description "The name of the buffer to append text to.")
               '(:name "text"
                       :type "string"
                       :description "The text to append to the buffer."))
   :category "emacs")

  ;; Message buffer logging tool
  (gptel-make-tool
   :function (lambda (text)
               (message "%s" text)
               (format "Message sent: %s" text))
   :name "echo_message"
   :description "Send a message to the *Messages* buffer"
   :args (list '(:name "text"
                       :type "string"
                       :description "The text to send to the messages buffer"))
   :category "emacs")

  ;; buffer retrieval tool
  (gptel-make-tool
   :function (lambda (buffer)
               (unless (buffer-live-p (get-buffer buffer))
                 (error "Error: buffer %s is not live." buffer))
               (with-current-buffer  buffer
                 (buffer-substring-no-properties (point-min) (point-max))))
   :name "read_buffer"
   :description "Return the contents of an Emacs buffer"
   :args (list '(:name "buffer"
                       :type "string"
                       :description "The name of the buffer whose contents are to be retrieved"))
   :category "emacs")

  (gptel-make-tool
   :function (lambda (directory)
               (mapconcat #'identity
                          (directory-files directory)
                          "\n"))
   :name "list_directory"
   :description "List the contents of a given directory"
   :args (list '(:name "directory"
                       :type "string"
                       :description "The path to the directory to list"))
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (parent name)
               (condition-case nil
                   (progn
                     (make-directory (expand-file-name name parent) t)
                     (format "Directory %s created/verified in %s" name parent))
                 (error (format "Error creating directory %s in %s" name parent))))
   :name "make_directory"
   :description "Create a new directory with the given name in the specified parent directory"
   :args (list '(:name "parent"
                       :type "string"
                       :description "The parent directory where the new directory should be created, e.g. /tmp")
               '(:name "name"
                       :type "string"
                       :description "The name of the new directory to create, e.g. testdir"))
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (path filename content)
               (let ((full-path (expand-file-name filename path)))
                 (with-temp-buffer
                   (insert content)
                   (write-file full-path))
                 (format "Created file %s in %s" filename path)))
   :name "create_file"
   :description "Create a new file with the specified content"
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
   :function (lambda (filepath)
               (with-temp-buffer
                 (insert-file-contents (expand-file-name filepath))
                 (buffer-string)))
   :name "read_file"
   :description "Read and display the contents of a file"
   :args (list '(:name "filepath"
                       :type "string"
                       :description "Path to the file to read.  Supports relative paths and ~."))
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (script_program script_file script_args)
               (let ((command (concat script_program " " (expand-file-name script_file) " " script_args)))
                 (with-temp-message (format "Running script: %s" command)
                   (shell-command-to-string command))))
   :name "run_script"
   :description "Run script"
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
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (command)
               (with-temp-message (format "Running command: %s" command)
                 (shell-command-to-string command)))
   :name "run_command"
   :description "Run command"
   :args (list
          '(:name "command"
                  :type "string"
                  :description "Command to run."))
   :category "filesystem"))


(provide 'init-gptel-tools)
;;; init-gptel-tools ends here
