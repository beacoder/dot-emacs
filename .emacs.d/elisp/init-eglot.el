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
;;  xref-find-apropos     (M-?)
;;  xref-find-references  (M-])
;;  complete-symbol       (C-M-i)
;;  eglot-rename


(when (maybe-require-package 'eglot)
  (maybe-require-package 'consult-eglot))


(provide 'init-eglot)
;;; init-eglot.el ends here
