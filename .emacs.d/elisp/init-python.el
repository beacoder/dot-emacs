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
              auto-mode-alist)
      python-shell-interpreter "python3")

(require-package 'pip-requirements)

(when (maybe-require-package 'anaconda-mode)
  (with-eval-after-load 'python
    ;; Anaconda doesn't work on remote servers without some work, so
    ;; by default we enable it only when working locally.
    (add-hook 'python-mode-hook
              (lambda () (unless (file-remote-p default-directory)
                      (anaconda-mode 1))))
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
    (bind-keys
     :map python-mode-map
     ("C-c c" . python-skeleton-class)
     ("C-c d" . python-skeleton-def)
     ("C-c f" . python-skeleton-for)
     ("C-c i" . python-skeleton-if)
     ("C-c m" . python-skeleton-import)
     ("C-c t" . python-skeleton-try)
     ("C-c w" . python-skeleton-while)
     ("C-c C-f" . nil))) ;; allow global binding for "C-c C-f"

  (with-eval-after-load 'anaconda-mode
    (bind-keys
     :map anaconda-mode-map
     ("M-]" . anaconda-mode-find-references)
     ("M-=" . anaconda-mode-find-assignments)
     ("M-?" . anaconda-mode-show-doc)))

  (when (maybe-require-package 'company-anaconda)
    (with-eval-after-load 'company
      (with-eval-after-load 'python
        (push 'company-anaconda company-backends)))))


(provide 'init-python)
;;; init-python.el ends here
