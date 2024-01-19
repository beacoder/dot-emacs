;;; init-ui.el --- Better lookings and appearances. -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Visual (UI) configurations for better lookings and appearances.
;;;
;;; Code:

;;; nerd-icons
(when (display-graphic-p)
  (when (maybe-require-package 'nerd-icons)
    (use-package nerd-icons
      :diminish
      :config (nerd-icons-install-fonts t)))

  ;;; dired support
  (when (maybe-require-package 'nerd-icons-dired)
    (use-package nerd-icons-dired
      :diminish
      :custom-face
      (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
      :hook (dired-mode . nerd-icons-dired-mode)))

  ;;; region completion support
  (when (maybe-require-package 'nerd-icons-corfu)
    (use-package nerd-icons-corfu
      :after corfu
      :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

  ;;; ibuffer support
  (when (maybe-require-package 'nerd-icons-ibuffer)
    (use-package nerd-icons-ibuffer
      :hook (ibuffer-mode . nerd-icons-ibuffer-mode)))

  ;;; ivy support
  (when (maybe-require-package 'nerd-icons-ivy-rich)
    (use-package nerd-icons-ivy-rich
      :ensure t
      :init
      (nerd-icons-ivy-rich-mode 1)
      (ivy-rich-mode 1))))


(provide 'init-ui)
;;; init-ui.el ends here
