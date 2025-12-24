;;; init-eglot.el --- LSP support via eglot -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; LSP support via eglot
;;;

;;; Code:

;;; useful key-bindings and commands
;;----------------------------------------------------------------------------
;;  eglot
;;-----------------------------
;;  xref-find-definitions (M-.)
;;  xref-find-references  (M-])
;;  consult-eglot-symbols (M-?)
;;  eglot-find-declaration
;;  eglot-find-implementation
;;  eglot-find-typeDefinition
;;-----------------------------
;;  eglot-rename
;;  eglot-show-type-hierarchy
;;  eglot-show-call-hierarchy


;; ensure 1.19 installed since it added call and type hierarchies.
;; @see https://cgit.git.savannah.gnu.org/cgit/emacs.git/commit/?id=1ef9de69b3c3d8254ab58bf455137a4439dce516
(require-package 'eglot "1.19")

;; by default, clangd will be used as c/c++ lsp server
(use-package eglot
  :ensure t
  :hook (((c-mode c++-mode c-ts-mode c++-ts-mode) . eglot-ensure))
  :init
  (setq eglot-stay-out-of '(imenu)
        read-process-output-max (* 1024 1024) ; 1MB
        eglot-autoshutdown t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.5)
  :config
  (progn
    ;; update settings for clangd, run eglot-stderr-buffer to verify
    (add-to-list 'eglot-server-programs
                 '((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode)
                   . ("clangd"
                      "--compile-commands-dir=./build/Linux_x86_64"
                      "--background-index"
                      "--clang-tidy"
                      "--completion-style=detailed"
                      "--header-insertion=never"
                      "--pch-storage=memory"
                      "--malloc-trim")))
    (use-package consult-eglot
      :ensure t
      :bind (:map eglot-mode-map
                  ("M-?" . consult-eglot-symbols)))))


(provide 'init-eglot)
;;; init-eglot.el ends here
