;;; init-ui.el --- Better lookings and appearances. -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Visual (UI) configurations for better lookings and appearances.
;;;
;;; Code:

;;; nerd-icons
(when (display-graphic-p)
  (use-package nerd-icons
    :ensure t
    :diminish
    :config (nerd-icons-install-fonts t))

  ;;; dired support
  (use-package nerd-icons-dired
    :ensure t
    :diminish
    :custom-face
    (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
    :hook (dired-mode . nerd-icons-dired-mode))

  ;;; region completion support
  (use-package nerd-icons-corfu
    :ensure t
    :after corfu
    :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

  ;;; ibuffer support
  (use-package nerd-icons-ibuffer
    :ensure t
    :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

  ;;; ivy support
  (use-package nerd-icons-ivy-rich
    :ensure t
    :init
    (nerd-icons-ivy-rich-mode 1)
    (ivy-rich-mode 1))

  ;;; Show icons instead of mode names
  (use-package mode-icons
    :ensure t
    :init
    (mode-icons-mode)
    (ivy-rich-mode 1)))


(provide 'init-ui)
;;; init-ui.el ends here
