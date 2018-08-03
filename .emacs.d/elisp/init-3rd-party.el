;;----------------------------------------------------------------------------
;; libraries with little to no configuration stay here
;;----------------------------------------------------------------------------

;;; use-package
(require-package 'use-package)


;; bind-key
;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
(require-package 'bind-key)


;;; whitespace setting
(require 'init-whitespace)


;;; async setting
(require-package 'async)
;; deal with problems when updating packages
(require 'async-bytecomp)
(async-bytecomp-package-mode 1)

;; provide dired with asynchronous abilities
(after-load "dired-aux" (require 'dired-async))

;; sending emails asynchronously
(require 'smtpmail-async)
(setq send-mail-function 'async-smtpmail-send-it
      message-send-mail-function 'async-smtpmail-send-it)


;;; multiple-cursors setting
(require-package 'multiple-cursors)


;;; undo-tree setting
;; "C-x u" => open the undo-tree-visualizer
(require-package 'undo-tree)
(add-hook 'after-init-hook 'global-undo-tree-mode)
(after-load 'undo-tree
  (diminish 'undo-tree-mode))
;; undo-buffer limit -> 100 MB                                                       |
(setq undo-outer-limit (* 100 (expt 1024 2)))


;;; browse-kill-ring
(require-package 'browse-kill-ring)
(setq browse-kill-ring-separator "\f")
(global-set-key (kbd "M-Y") #'browse-kill-ring)


;;; slime setting
;; (require 'slime-autoloads)
;; (setq inferior-lisp-program
;;       (replace-regexp-in-string "/lib/sbcl/?$" "/bin/sbcl" (getenv "SBCL_HOME")))
;; (slime-setup '(slime-fancy slime-indentation slime-asdf))
;; (setq lisp-indent-function 'common-lisp-indent-function)


;;; paredit setting
;; (require-package 'paredit)
;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; (eval-after-load 'paredit
;;   '(progn
;;      ;; Modify kill-sentence, which is easily confused with the kill-sexp
;;      ;; binding, but doesn't preserve sexp structure
;;      (define-key paredit-mode-map [remap kill-sentence] 'paredit-kill)
;;      (define-key paredit-mode-map [remap backward-kill-sentence] nil)

;;      ;; Allow my global binding of M-? to work when paredit is active
;;      (define-key paredit-mode-map (kbd "M-?") nil)
;;      ))
;; (add-hook 'prog-mode-hook 'enable-paredit-mode)

;; ;; "C-)" might not work as expected in putty, so we create a new prefix-key for paredit.
;; (define-prefix-command 'paredit-map)
;; (define-key global-map "\C-xp" paredit-map)
;; (define-key paredit-map (kbd "s") 'paredit-forward-slurp-sexp)
;; (define-key paredit-map (kbd "b") 'paredit-forward-barf-sexp)


;;; smex to handle M-x
(when (maybe-require-package 'smex)
  ;; Change path for ~/.smex-items
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] #'smex))


;;; markdown-mode
(when (maybe-require-package 'markdown-mode)
  (after-load 'whitespace-cleanup-mode
    (push 'markdown-mode whitespace-cleanup-mode-ignore-modes)))


;;; ttcn3 setting
(autoload 'ttcn-3-mode "ttcn3" "Major mode for ttcn3 files" t)
(add-to-list 'auto-mode-alist '("\\.ttcn$" . ttcn-3-mode))
(add-to-list 'auto-mode-alist '("\\.ttcnpp$" . ttcn-3-mode))


;;; keyfreq setting
;; keyfreq-show could show the key-frequency
(require-package 'keyfreq)
(setq keyfreq-excluded-commands
      '(self-insert-command
        abort-recursive-edit
        forward-char
        backward-char
        previous-line
        next-line))
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)


;;; regex-tool setting
;; "C-c C-c" => force an update
;; "C-c C-k" => quit regex-tool
(when (maybe-require-package 'regex-tool)
  (require 'regex-tool)
  (set-default regex-tool-backend 'perl)
  (global-set-key (kbd "C-c C-r") #'regex-tool))


;;; zenburn setting
;; need to setup putty color which goes well with zenburn first
(require-package 'zenburn-theme)
(load-theme 'zenburn t)


;;; diminish
(require-package 'diminish)


;;; ggtags setting
;; install "GNU Global" with "Exuberant Ctags" support
;; "cd /path/to/project && gtags"
;;
;; "M-."     => ggtags-find-tag-dwim
;; "M-]"     => ggtags-find-reference
;; "C-c M-r" => ggtags-find-tag-regexp
;; "C-c M-f" => ggtags-find-file
;; "C-c M-g" => ggtags-grep
(require-package 'ggtags)
(after-load 'ggtags
  (define-key ggtags-mode-prefix-map "\M-r" 'ggtags-find-tag-regexp))
(add-hook 'c-mode-common-hook
          (lambda () (when (and (executable-find "global")
                                ;; check existence of GTAGS
                                (not (string-match-p "GTAGS not found" (shell-command-to-string "global -p")))
                                (not (member major-mode ggtags-exclude-modes)))
                       (setq gtags-suggested-key-mapping t)
                       (ggtags-mode 1))))


;;; weather report
;; "wttrin" => Display weather
;; "g"      => Change city
;; "q"      => Quit
(require-package 'wttrin)
(setq wttrin-default-cities '("Shanghai" "Taizhou"))


;;; PlantUML
;; Download plantuml.jar first, put it in ~/plantuml.jar.
;; wget https://netcologne.dl.sourceforge.net/project/plantuml/plantuml.jar
;;
;; "C-c C-c" => plantuml-preview => Show UML Diagram
(require-package 'plantuml-mode)


;;; Pyim - "Chinese Pinyin Input Method"
;; "C-\" => toggle-input-method
(when (and (maybe-require-package 'pyim)
           (maybe-require-package 'pyim-basedict))
  (require 'pyim)
  (pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
  (defun hydra-pyim-start ()
    (interactive)
    (pyim-start "pyim")))


;;; Beacon — Never lose your cursor again
(when (maybe-require-package 'beacon)
  (setq-default beacon-lighter "")
  (setq-default beacon-size 50)
  (setq-default beacon-color "#666600")
  (setq-default beacon-blink-duration 0.5)
  (add-hook 'after-init-hook 'beacon-mode))


;;; Expand region
(require-package 'expand-region)
(global-set-key (kbd "M-8") #'er/expand-region)


;;; Yaml mode
(when (maybe-require-package 'yaml-mode)
  (add-auto-mode 'yaml-mode "\\.yml\\.erb\\'")
  (add-hook 'yaml-mode-hook 'goto-address-prog-mode))


;;; Call-graph
(when (maybe-require-package 'call-graph)
  (require 'call-graph)
  (global-set-key (kbd "C-c g") #'call-graph)
  ;; (setq cg-path-to-global "~/private/gtags-6.5.7/bin/")
  (setq imenu-max-item-length "Unlimited")
  (dolist (filter '("grep -v \"Test/\""
                    "grep -v \"_SUITE/\""
                    "grep -v \"/test-src/\""
                    "grep -v \"/TestPkg/\""))
    (add-to-list 'cg-search-filters filter)))


;;; package-lint
(maybe-require-package 'package-lint)


;;; diff-hl
(when (maybe-require-package 'diff-hl)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'after-init-hook 'global-diff-hl-mode)

  (after-load 'diff-hl
    (define-key diff-hl-mode-map
      (kbd "<left-fringe> <mouse-1>")
      #'diff-hl-diff-goto-hunk)))
(maybe-require-package 'browse-at-remote)


;;; csv-mode
(require-package 'csv-mode)
(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")
(setq csv-separators '("," ";" "|" " "))


;;; Multiple major modes
(require-package 'mmm-mode)
(require 'mmm-auto)
(setq mmm-global-mode 'buffers-with-submode-classes
      mmm-submode-decoration-level 2)


;;; nyan-mode
(when (maybe-require-package 'nyan-mode)
  (setq-default nyan-animate-nyancat nil
                nyan-wavy-trail t)
  (add-hook 'after-init-hook 'nyan-mode))


;;; symbol-overlay
(when (maybe-require-package 'symbol-overlay)
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook yaml-mode-hook conf-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  (after-load 'symbol-overlay
    (diminish 'symbol-overlay-mode)
    (define-key symbol-overlay-mode-map (kbd "M-i") #'symbol-overlay-put)
    (define-key symbol-overlay-mode-map (kbd "M-n") #'symbol-overlay-switch-forward)
    (define-key symbol-overlay-mode-map (kbd "M-p") #'symbol-overlay-switch-backward)))


;;; emojify
(when (maybe-require-package 'emojify)
  (use-package emojify
    :disabled t
    :defer 15
    :config
    (global-emojify-mode)
    ;; (global-emojify-mode-line-mode -1)
    ))


;;; which-key
(when (maybe-require-package 'which-key)
  (require 'which-key)
  (which-key-mode +1))


;;; move-text
(require-package 'move-text)


;;; smart-mode-line
(when (maybe-require-package 'smart-mode-line)
  (sml/setup))


;;; gist
;;
;; git config --global github.user <your-github-user-name>
;; git config --global github.oauth-token <your-personal-access-token-with-gist-scope>
;;
;; "gist-region-or-buffer"         => Post region/buffer to gist.github.com and copy URL to kill-ring
;; "gist-region-or-buffer-private" => Same as gist-region-or-buffer but create a private gist
;;
;; "gist-list"                     => Lists your gists in a new buffer
;; "+"                             => add a file to the current gist
;; "-"                             => remove a file from the current gist
;; "e"                             => edit current gist description
;; "k"                             => delete current gist
(when (maybe-require-package 'gist)
  (setq gist-view-gist nil))


;;; other setting
(require 'init-hydra)
(require 'init-git)
;; (require 'init-helm)
(require 'init-company)
;; (require 'init-rtags)
(require 'init-dictionary)
(require 'init-dired)
(require 'init-isearch)
(require 'init-ibuffer)
(require 'init-org)
(require 'init-ivy)
(require 'init-eww)
(require 'init-flycheck)
(require-package 'buffer-move)
(require 'init-http)


(provide 'init-3rd-party)
;;; init-3rd-party.el ends here
