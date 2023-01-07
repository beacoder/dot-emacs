;;; init-eglot.el --- LSP support via eglot -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; LSP support via eglot
;;;

;;; Code:

;;; useful key-bindings
;;----------------------------------------------------------------------------
;;; eglot
;;  xref-find-definitions (M-.)
;;  xref-find-references  (M-])
;;  complete-symbol       (C-M-i)
;;  eglot-rename

;; For Emacs >= 27
(setq read-process-output-max (* 1024 1024))


(when (maybe-require-package 'eglot)
  (maybe-require-package 'consult-eglot)
  ;; default enable eglot
  (use-package eglot
    :hook ((prog-mode . (lambda () (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode)
                                (eglot-ensure))))
           ((markdown-mode yaml-mode) . eglot-ensure))))


(provide 'init-eglot)
;;; init-eglot.el ends here
