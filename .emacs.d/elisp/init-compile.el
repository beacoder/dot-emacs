;;; init-compile.el --- compile command setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-elpa)
(require 'ansi-color)

(require-package 'alert)
(maybe-require-package 'cmd-to-echo)

(setq-default compilation-scroll-output t)

(defvar sanityinc/last-compilation-buffer nil
  "The last buffer in which compilation took place.")

(defvar sanityinc/last-window-configuration nil
  "The last window configuration in which compilation took place.")

(defvar sanityinc/last-selected-window nil
  "The last selected window in which compilation took place.")

;;; compilation setting
(defun mode-compile ()
  "Compile with mode specific commands."
  (interactive)
  (let ((command "make debug"))
    (case major-mode
      ('c++-mode
       (if current-prefix-arg
           ;; do full check
           (setq command (concat (getenv "WS_ROOT") "/tools/bin/build_all -v -n test -M -O"))
         ;; just build code
         (setq command (concat (getenv "WS_ROOT") "/tools/bin/build -v -c Linux_x86_64"))))
      ('ttcn-3-mode (setq command
                          (concat (getenv "TTCN3_GGSN_ROOT_PATH") "/scripts/compile_ttcn.sh build" " && "
                                  (getenv "TTCN3_GGSN_ROOT_PATH") "/scripts/copy_ttcn3.sh"))))
    (compile command)))

(defun sanityinc/alert-after-compilation-finish (buf result)
  "Use `alert' to report compilation RESULT if BUF is hidden."
  (when (buffer-live-p buf)
    (unless (catch 'is-visible
              (walk-windows (lambda (w)
                              (when (eq (window-buffer w) buf)
                                (throw 'is-visible t))))
              nil)
      (alert (concat "Compilation " result)
             :buffer buf
             :category 'compilation))))

(with-eval-after-load 'compile
  (add-hook 'compilation-finish-functions
            'sanityinc/alert-after-compilation-finish)
  (setq compilation-window-height 8)
  ;; @see http://xugx2007.blogspot.com.au/2007/06/benjamin-rutts-emacs-c-development-tips.html
  (add-hook 'compilation-finish-functions
            #'(lambda (buf str)
                (if (string-match "exited abnormally" str)
                    ;;there were errors
                    (message "compilation errors, press C-x ` to visit")
                  (when-let ((buf-name (buffer-name buf))
                             (is-exist (string-match "*compilation*" buf-name)))
                    ;; @see http://emacswiki.org/emacs/ModeCompile#toc2
                    (bury-buffer "*compilation*")
                    (set-window-configuration sanityinc/last-window-configuration)
                    (select-window sanityinc/last-selected-window)
                    (message "NO COMPILATION ERRORS!")))))

  (defun sanityinc/save-compilation-buffer-and-window (&rest _)
    "Save the compilation buffer to find it later."
    (setq sanityinc/last-compilation-buffer next-error-last-buffer
          sanityinc/last-window-configuration (current-window-configuration)
          sanityinc/last-selected-window (frame-selected-window)))
  (advice-add 'compilation-start :after 'sanityinc/save-compilation-buffer-and-window)

  (defun sanityinc/find-prev-compilation (orig &optional edit-command)
    "Find the previous compilation buffer, if present, and recompile there."
    (if (and (null edit-command)
             (not (derived-mode-p 'compilation-mode))
             sanityinc/last-compilation-buffer
             (buffer-live-p (get-buffer sanityinc/last-compilation-buffer)))
        (with-current-buffer sanityinc/last-compilation-buffer
          (funcall orig edit-command))
      (funcall orig edit-command)))
  (advice-add 'recompile :around 'sanityinc/find-prev-compilation)

  (require 'ansi-color)
  (defun sanityinc/colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'sanityinc/colourise-compilation-buffer))

(defun sanityinc/shell-command-in-view-mode (start end command &optional output-buffer replace &rest other-args)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless (or output-buffer replace)
    (with-current-buffer "*Shell Command Output*"
      (view-mode 1))))
(advice-add 'shell-command-on-region :after 'sanityinc/shell-command-in-view-mode)


(provide 'init-compile)
;;; init-compile.el ends here
