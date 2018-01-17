;; save register definitions

;; "C-x r j d" to open directory ".emacs.d"
(set-register ?d '(file . "~/.emacs.d"))

(set-register ?h '(file . "~/.bash_history"))

(set-register ?e '(file . "~/.emacs.d/init.el"))

(set-register ?c '(file . "~/.bashrc"))

(set-register ?b '(file . "~/backup"))

(set-register ?w '(file . "~/workspace"))

(provide 'init-register)
