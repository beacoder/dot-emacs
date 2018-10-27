;;----------------------------------------------------------------------------
;; compile command setting
;;----------------------------------------------------------------------------

(setq-default compilation-scroll-output t)

(require-package 'alert)

;;; compilation setting
(defun mode-compile ()
  "Compile with mode specific commands."
  (interactive)
  (let ((command "make debug"))
    (case major-mode
          ('c++-mode (setq command (concat (getenv "WS_ROOT") "/tools/bin/build -v -c Linux_x86_64")))
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

(after-load 'compile
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
                    (winner-undo)
                    (message "NO COMPILATION ERRORS!"))))))

(defvar sanityinc/last-compilation-buffer nil
  "The last buffer in which compilation took place.")

(after-load 'compile
  (defun sanityinc/save-compilation-buffer (&rest _)
    "Save the compilation buffer to find it later."
    (setq sanityinc/last-compilation-buffer next-error-last-buffer))
  (advice-add 'compilation-start :after 'sanityinc/save-compilation-buffer)

  (defun sanityinc/find-prev-compilation (orig &rest args)
    "Find the previous compilation buffer, if present, and recompile there."
    (if (and (null edit-command)
             (not (derived-mode-p 'compilation-mode))
             sanityinc/last-compilation-buffer
             (buffer-live-p (get-buffer sanityinc/last-compilation-buffer)))
        (with-current-buffer sanityinc/last-compilation-buffer
          (apply orig args))
      (apply orig args)))
  (advice-add 'recompile :around 'sanityinc/find-prev-compilation))


(defun sanityinc/shell-command-in-view-mode (start end command &optional output-buffer replace &rest other-args)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless (or output-buffer replace)
    (with-current-buffer "*Shell Command Output*"
      (view-mode 1))))
(advice-add 'shell-command-on-region :after 'sanityinc/shell-command-in-view-mode)


(after-load 'compile
  (require 'ansi-color)
  (defun sanityinc/colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'sanityinc/colourise-compilation-buffer))

(maybe-require-package 'cmd-to-echo)


(provide 'init-compile)
;;; init-compile.el ends here
