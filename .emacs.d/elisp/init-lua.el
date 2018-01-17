;;;; This snippet enables lua-mode

;; This line is not necessary, if lua-mode.el is already on your load-path
;; (add-to-list 'load-path "/path/to/directory/where/lua-mode-el/resides")

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(add-hook 'lua-mode-hook
          (lambda ()
            (setq safe-local-variable-values
                  '((lua-indent-level . 2)
                    (lua-indent-level . 3)
                    (lua-indent-level . 4)
                    (lua-indent-level . 8)))
            ;;(flymake-lua-load)
            ))

(provide 'init-lua)
