;;----------------------------------------------------------------------------
;;  Helm setting
;;----------------------------------------------------------------------------

(require 'helm-config)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h e") 'helm-etags-select)
(global-set-key (kbd "C-c h g") 'helm-do-grep)
(global-set-key (kbd "C-c h s") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-c h m") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h f") 'helm-find)
(global-set-key (kbd "C-c h l") 'helm-locate)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h r") 'helm-resume)
(global-set-key (kbd "C-c h y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h i") 'helm-register)

;; avoid mistaking "C-h s" for "C-c h s"
(global-unset-key (kbd "C-h s"))

;; since "C-c C-h" cann't be unset, we set it to a dummy lambda function
(global-set-key (kbd "C-c C-h") (lambda () (interactive)))

(helm-mode 1)

(provide 'init-helm)
