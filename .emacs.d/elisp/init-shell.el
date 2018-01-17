;;----------------------------------------------------------------------------
;; shell setting
;;----------------------------------------------------------------------------

(setq shell-file-name "/bin/bash")

(require-package 'exec-path-from-shell)
(after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "GTAGSLIBPATH"))
    (add-to-list 'exec-path-from-shell-variables var)))

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; kill the buffer after the ansi-term is exited
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; use bash as the default shell for ansi-term
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; use utf-8 as default coding-system for ansi-term
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

;; make urls clickable in ansi-term
(defun my-term-hook ()
  (goto-address-mode))
(add-hook 'term-mode-hook 'my-term-hook)


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

(provide 'init-shell)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
