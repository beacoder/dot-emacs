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

;; For Emacs >= 27
(setq read-process-output-max (* 1024 1024))


(when (maybe-require-package 'eglot)
  (maybe-require-package 'consult-eglot)
  ;; disable eglot for now, since clangd index is not working
  ;; ;; use eglot for all modes
  ;; (use-package eglot
  ;;   :hook ((prog-mode . (lambda () (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode)
  ;;                               (eglot-ensure))))
  ;;          ((markdown-mode yaml-mode) . eglot-ensure)))
  ;; ;; use ccls instead of clangd for c++
  ;; (with-eval-after-load 'eglot
  ;;   (add-to-list 'eglot-server-programs
  ;;                '((c-mode c++-mode)
  ;;                  . ("/proj/epg-tools/ccls/dce86b13-clang13.0.0_2/bin/ccls"))))
)


(provide 'init-eglot)
;;; init-eglot.el ends here
