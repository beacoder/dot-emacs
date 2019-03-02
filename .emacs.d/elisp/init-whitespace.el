;;; init-whitespace.el --- Special handling for whitespace -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; "M-SPC" => cycle-spacing
(global-set-key [remap just-one-space] #'cycle-spacing)

(setq-default show-trailing-whitespace nil)


;;; Whitespace

(defun sanityinc/show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'sanityinc/show-trailing-whitespace))


(require-package 'whitespace-cleanup-mode)
(add-hook 'after-init-hook #'global-whitespace-cleanup-mode)


(provide 'init-whitespace)
;;; init-whitespace.el ends here
