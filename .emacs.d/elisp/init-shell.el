;;; init-shell.el --- shell settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'exec-path-from-shell)

  (with-eval-after-load 'exec-path-from-shell
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "GTAGSLIBPATH"))
      (add-to-list 'exec-path-from-shell-variables var)))

  (when (or (memq window-system '(mac ns x))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
    (setq-default exec-path-from-shell-arguments nil)
    (exec-path-from-shell-initialize)))


(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  "Kill buffer after `ansi-term' is exited."
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)


(setq shell-file-name "/bin/bash")
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  "Use bash as the default shell for `ansi-term'."
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)


(defun use-utf8-in-term ()
  "Use utf-8 as default coding-system for `ansi-term'."
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook #'use-utf8-in-term)


(defun enable-address-mode ()
  "Make urls clickable in `ansi-term'."
  (goto-address-mode))
(add-hook 'term-mode-hook #'enable-address-mode)


;; @see https://stackoverflow.com/questions/5819719/emacs-shell-command-output-not-showing-ansi-colors-but-the-code
;; correctly handle colors in shell output
(require 'ansi-color)
(defadvice display-message-or-buffer (before ansi-color activate)
  "Process ANSI color codes in shell output."
  (let ((buf (ad-get-arg 0)))
    (and (bufferp buf)
         (string= (buffer-name buf) "*Shell Command Output*")
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max))))))


;; On Mac, when start emacs from GUI, emacs does not inherit environment variables from your shell
(when  *is-a-mac*
  (setenv "PATH" (concat "/usr/local/bin" path-separator (getenv "PATH"))))


(provide 'init-shell)
;;; init-shell.el ends here
