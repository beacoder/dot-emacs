;;; init-register.el --- Register settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun setup-my-registers ()
  "Setup my own registers."

  ;; <C-x r j b> => open directory "~/backup"
  (set-register ?b '(file . "~/backup"))

  (set-register ?c '(file . "~/private/init.csh"))

  (set-register ?e '(file . "~/.emacs.d/elisp"))

  (set-register ?h '(file . "~/.sh_history"))

  (set-register ?o '(file . "~/.emacs.d/tutorials/org-tutorial.org"))

  (set-register ?w '(file . "~/workspace")))

(add-hook 'after-init-hook #'setup-my-registers)


(provide 'init-register)
;;; init-register.el ends here
