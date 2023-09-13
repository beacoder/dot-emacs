;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; See the following note about how I set up python + virtualenv to
;; work seamlessly with Emacs:
;; https://gist.github.com/purcell/81f76c50a42eee710dcfc9a14bfc7240


;; "C-c C-p" =>  run-python
;; "C-c C-s" =>  python-shell-send-string
;; "C-c C-r" =>  python-shell-send-region
;; "C-c C-z" =>  python-shell-switch-to-shell
;; "C-c C-j" =>  imenu

;; "C-c <"   =>  python-indent-shift-left
;; "C-c >"   =>  python-indent-shift-right

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(setq python-shell-interpreter "python3")

(require-package 'pip-requirements)

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode)))

(when (maybe-require-package 'reformatter)
  (reformatter-define black :program "black" :args '("-")))

(provide 'init-python)
;;; init-python.el ends here


(provide 'init-python)
;;; init-python.el ends here
