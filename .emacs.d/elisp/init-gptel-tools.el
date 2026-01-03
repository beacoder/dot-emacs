;;; init-gptel-tools.el --- config gptel tools for gptel to use. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Define tool-callbacks
;; @note return either a result or a message to inform the LLM
(defun my-gptel--edit_file (file_path file_changes)
  "Apply FILE_CHANGES to FILE_PATH using pattern matching and replacement.

FILE_PATH is a string representing the full path to the file to edit.
FILE_CHANGES is a list of plists where each plist contains:
  :line_number - The line number where the edit should start (1-indexed)
  :old_string - The string to search for and replace
  :new_string - The string to replace OLD_STRING with

Returns a success or failure message string."
  (if (and file_path (not (string= file_path "")) file_changes)
      (with-temp-buffer
        (insert-file-contents (expand-file-name file_path))
        (let ((inhibit-read-only t)
              (case-fold-search nil)
              (file-name (expand-file-name file_path))
              (edit-success nil))
          ;; apply changes
          (dolist (file_change (seq-into file_changes 'list))
            (when-let ((line_number (plist-get file_change :line_number))
                       (old_string (plist-get file_change :old_string))
                       (new_string (plist-get file_change :new_string)))
              (goto-char (point-min))
              (forward-line (1- line_number))
              (let ((eol (line-end-position)))
                (when (search-forward old_string eol t)
                  (replace-match new_string t t)
                  (setq edit-success t)))))
          ;; return result to gptel
          (if edit-success
              (progn
                (write-file file-name)
                (format "Successfully edited %s" file-name))
            (format "Failed to edited %s" file-name))))
    (format "Failed to edited %s" file_path)))

(defun my-gptel--run_async_command (callback command)
  "Run COMMAND asynchronously and pass output to CALLBACK.

COMMAND is a string representing the shell command to execute.
CALLBACK is a function that will be called with the command output.
The callback receives a single argument: the output string.

This function uses async-shell-command and sets up a process sentinel
that calls the callback when the command completes.

Returns nothing (the result is passed to the callback)."
  (condition-case error
      (let ((buffer (generate-new-buffer " *async output*")))
        (with-temp-message (format "Running async command: %s" command)
          (async-shell-command command buffer nil))
        (let ((proc (get-buffer-process buffer)))
          (when proc
            (set-process-sentinel
             proc
             (lambda (process _event)
               (unless (process-live-p process)
                 (with-current-buffer (process-buffer process)
                   (let ((output (buffer-substring-no-properties (point-min) (point-max))))
                     (kill-buffer (current-buffer))
                     (funcall callback output)))))))))
    (t
     ;; Handle any kind of error
     (funcall callback (format "An error occurred: %s" error)))))

(defun my-gptel--read_file (filepath)
  "Read and return the contents of FILEPATH.

FILEPATH is a string representing the path to the file.
Supports relative paths and ~ expansion.

Returns the file contents as a string."
  (with-temp-message (format "Reading file: %s" filepath)
    (with-temp-buffer
      (insert-file-contents (expand-file-name filepath))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun my-gptel--create_file (path filename content)
  "Create a new file with FILENAME in PATH containing CONTENT.

PATH is a string representing the directory where to create the file.
FILENAME is a string representing the name of the file to create.
CONTENT is a string containing the content to write to the file.

Returns a success message string or an error description."
  (condition-case error
      (let ((full-path (expand-file-name filename path)))
        (with-temp-buffer
          (insert content)
          (write-file full-path))
        (format "Created file %s in %s" filename path))
    (t
     ;; Handle any kind of error
     (format "An error occurred: %s" error))))

(defun my-gptel--open_file (filepath)
  "Open FILEPATH in Emacs and return a success message.

FILEPATH is a string representing the path to the file.
Supports relative paths and ~ expansion.

Uses `find-file' to open the file in Emacs.
Returns a success message string."
  (find-file (expand-file-name filepath))
  (format "Opened file %s" filepath))

(defun my-gptel--append_to_buffer (buffer text)
  "Append TEXT to BUFFER.

BUFFER is a string representing the name of the buffer.
TEXT is a string containing the text to append.
If BUFFER doesn't exist, it will be created.

Uses `get-buffer-create' to get or create the buffer.
Returns a success message string."
  (with-current-buffer (get-buffer-create buffer)
    (save-excursion
      (goto-char (point-max))
      (insert text)))
  (format "Appended text to buffer %s" buffer))

(defun my-gptel--read_buffer (buffer)
  "Read and return the contents of BUFFER.

BUFFER is a string representing the buffer name.
The buffer must be live (existing and not killed).

Returns the buffer contents as a string.
Raises an error if the buffer is not live."
  (with-temp-message "Reading buffer"
    (unless (buffer-live-p (get-buffer buffer))
      (error "Error: buffer %s is not live." buffer))
    (with-current-buffer  buffer
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun my-gptel--make_directory (parent name)
  "Create a directory named NAME in PARENT.

PARENT is a string representing the parent directory path.
NAME is a string representing the name of the directory to create.

Uses `make-directory' with the `t' flag to create parent directories if needed.
Returns a success message string or an error description."
  (condition-case nil
      (progn
        (make-directory (expand-file-name name parent) t)
        (format "Directory %s created/verified in %s" name parent))
    (error (format "Error creating directory %s in %s" name parent))))


;; Holds a list of tools available for LLM to use
;; @see https://github.com/karthink/gptel/issues/514
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
                     :type string
                     :description "The URL to read"))
 :category "web")

(gptel-make-tool
 :function #'my-gptel--append_to_buffer
 :name "append_to_buffer"
 :description "Append text to the an Emacs buffer.  If the buffer does not exist, it will be created."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the buffer to append text to.")
             '(:name "text"
                     :type string
                     :description "The text to append to the buffer."))
 :category "emacs")

(gptel-make-tool
 :function #'my-gptel--read_buffer
 :name "read_buffer"
 :description "Return the contents of an Emacs buffer."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the buffer whose contents are to be retrieved"))
 :category "emacs")

(gptel-make-tool
 :function #'my-gptel--open_file
 :name "open_file"
 :description "Open and display the contents of a file."
 :args (list '(:name "filepath"
                     :type string
                     :description "Path to the file to open.  Supports relative paths and ~."))
 :category "emacs")

(gptel-make-tool
 :function #'my-gptel--make_directory
 :name "make_directory"
 :description "Create a new directory with the given name in the specified parent directory."
 :args (list '(:name "parent"
                     :type string
                     :description "The parent directory where the new directory should be created, e.g. /tmp")
             '(:name "name"
                     :type string
                     :description "The name of the new directory to create, e.g. testdir"))
 :category "filesystem")

(gptel-make-tool
 :function #'my-gptel--create_file
 :name "create_file"
 :description "Create a new file with the specified content."
 :args (list '(:name "path"
                     :type string
                     :description "The directory where to create the file")
             '(:name "filename"
                     :type string
                     :description "The name of the file to create")
             '(:name "content"
                     :type string
                     :description "The content to write to the file"))
 :category "filesystem")

(gptel-make-tool
 :function #'my-gptel--read_file
 :name "read_file"
 :description "Read and display the contents of a file."
 :args (list '(:name "filepath"
                     :type string
                     :description "Path to the file to read.  Supports relative paths and ~."))
 :category "filesystem")

(gptel-make-tool
 :function #'my-gptel--edit_file
 :name "edit_file"
 :description "Edit file with a list of changes, each change contains a line number,
an old string and a new string, new string will replace the old string at the specified line."
 :args (list '(:name "file_path"
                     :type string
                     :description "The full path of the file to edit")
             '(:name "file_changes"
                     :type array
                     :items (:type object
                                   :properties
                                   (:line_number
                                    (:type integer :description "The line number of the file where edit starts.")
                                    :old_string
                                    (:type string :description "The old string to be replaced.")
                                    :new_string
                                    (:type string :description "The new string to replace old.")))
                     :description "The list of changes to apply on the file"))
 :category "filesystem")

;; super-powerful, capable of replacing numerous existing tools.
(gptel-make-tool
 :function (lambda (command)
             (with-temp-message (format "Running command: %s" command)
               (shell-command-to-string command)))
 :name "run_command"
 :description "Run a command."
 :args (list
        '(:name "command"
                :type string
                :description "Command to run."))
 :category "command")

;; async-tools make multiple agents possible.
(gptel-make-tool
 :function #'my-gptel--run_async_command
 :name "run_async_command"
 :description "Run an async command."
 :args (list
        '(:name "command"
                :type string
                :description "Command to run."))
 :category "command"
 :async t
 :include t)


(provide 'init-gptel-tools)
;;; init-gptel-tools.el ends here
