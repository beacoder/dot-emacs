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
;;  consult-eglot-symbols (M-?)
;;  xref-find-references  (M-])
;;  eglot-rename


;; by default, clangd will be used as c/c++ lsp server
(use-package eglot
  :hook (((c-mode c++-mode c-ts-mode c++-ts-mode) . eglot-ensure))
  :init
  (setq eglot-stay-out-of '(imenu)
        read-process-output-max (* 1024 1024) ; 1MB
        eglot-autoshutdown t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.5)
  :config
  (use-package consult-eglot
    :bind (:map eglot-mode-map
                ("M-?" . consult-eglot-symbols))))


(provide 'init-eglot)
;;; init-eglot.el ends here
