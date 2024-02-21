;;; init-corfu.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;
;; Auto-completion configurations.
;;

;;; Code:

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.2 . 0.1))
  :custom-face
  (corfu-border ((t (:inherit region :background "yellow"))))
  :bind ("M-/" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))


(unless (display-graphic-p)
  (use-package corfu-terminal
    :ensure t
    :hook (global-corfu-mode . corfu-terminal-mode)))


;; Add extensions
(use-package cape
  :ensure t
  :init
  (setq cape-dict-case-fold t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; :config
  ;; (require 'cape-yasnippet)
  ;; (add-to-list 'completion-at-point-functions #'cape-yasnippet)
  )


(provide 'init-corfu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-corfu.el ends here
