;;; init-gptel-tools.el --- config gptel tools for gptel to use. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Define tool-callbacks
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
