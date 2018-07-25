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
            'sanityinc/alert-after-compilation-finish))

(defvar sanityinc/last-compilation-buffer nil
  "The last buffer in which compilation took place.")

(after-load 'compile
  (defadvice compilation-start (after sanityinc/save-compilation-buffer activate)
    "Save the compilation buffer to find it later."
    (setq sanityinc/last-compilation-buffer next-error-last-buffer))

  (defadvice recompile (around sanityinc/find-prev-compilation (&optional edit-command) activate)
    "Find the previous compilation buffer, if present, and recompile there."
    (if (and (null edit-command)
             (not (derived-mode-p 'compilation-mode))
             sanityinc/last-compilation-buffer
             (buffer-live-p (get-buffer sanityinc/last-compilation-buffer)))
        (with-current-buffer sanityinc/last-compilation-buffer
          ad-do-it)
      ad-do-it)))

(defadvice shell-command-on-region
    (after sanityinc/shell-command-in-view-mode
           (start end command &optional output-buffer &rest other-args)
           activate)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless output-buffer
    (with-current-buffer "*Shell Command Output*"
      (view-mode 1))))

(after-load 'compile
  (require 'ansi-color)
  (defun sanityinc/colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'sanityinc/colourise-compilation-buffer))

(maybe-require-package 'cmd-to-echo)


(provide 'init-compile)
;;; init-compile.el ends here
