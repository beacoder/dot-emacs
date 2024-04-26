;;; init-3rd-party.el --- 3rd party config -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Libraries with little to no configuration stay here
;;;
;;; Code:

;; bind-key
;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
(require-package 'bind-key)


;;; whitespace setting
(require 'init-whitespace)


;;; async handling with subprocess
(when (maybe-require-package 'async)
  ;; deal with problems when updating packages
  (require 'async-bytecomp)
  (async-bytecomp-package-mode 1)

  ;; provide dired with asynchronous abilities
  (require 'dired-async)
  (dired-async-mode 1)

  ;; sending emails asynchronously
  (require 'smtpmail-async)
  (setq send-mail-function 'async-smtpmail-send-it
        message-send-mail-function 'async-smtpmail-send-it))


;;; multiple-cursors setting
(require-package 'multiple-cursors)


;;; undo setting
;; "C-x u" => open the undo-tree-visualizer
(if (>= emacs-major-version 28)
    (when (maybe-require-package 'vundo)
     (use-package vundo
       :bind ("C-x u" . vundo)
       :config (setq vundo-glyph-alist vundo-unicode-symbols)))
  (when (maybe-require-package 'undo-tree)
    (add-hook 'after-init-hook 'global-undo-tree-mode)
    (with-eval-after-load 'undo-tree
      ;; undo-buffer limit -> 100 MB
      (setq undo-outer-limit (* 100 (expt 1024 2))))
    ;; prevent undo-tree files from polluting your git repo
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))))


;;; browse-kill-ring
(when (require-package 'browse-kill-ring)
  (setq browse-kill-ring-separator "\f")
  (global-set-key (kbd "C-M-y") #'browse-kill-ring))


;;; slime setting
;; (require 'slime-autoloads)
;; (setq inferior-lisp-program
;;       (replace-regexp-in-string "/lib/sbcl/?$" "/bin/sbcl" (getenv "SBCL_HOME")))
;; (slime-setup '(slime-fancy slime-indentation slime-asdf))
;; (setq lisp-indent-function 'common-lisp-indent-function)


;;; markdown-mode
(when (maybe-require-package 'markdown-mode)
  (with-eval-after-load 'whitespace-cleanup-mode
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


;;; zenburn setting
;; need to setup putty color which goes well with zenburn first
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))


;;; ggtags setting
;; install "GNU Global" with universal-ctags support
;; "cd /path/to/project && gtags"
;;
;; "M-."     => ggtags-find-tag-dwim
;; "M-]"     => ggtags-find-reference
;; "C-c M-r" => ggtags-find-tag-regexp
;; "C-c M-f" => ggtags-find-file
;; "C-c M-g" => ggtags-grep
(when (maybe-require-package 'ggtags)
  (with-eval-after-load 'ggtags
    (define-key ggtags-mode-prefix-map "\M-r" #'ggtags-find-tag-regexp))
  (defun enable-ggtags-mode ()
    "Enable ggtags mode."
    (when (and (executable-find "global")
               (not (string-match-p "GTAGS not found" (shell-command-to-string "global -p")))
               (not (member major-mode ggtags-exclude-modes)))
      (setq gtags-suggested-key-mapping t)
      (ggtags-mode 1)))
  (dolist (c-mode-hook '(c-mode-common-hook c-ts-mode-hook c++-ts-mode-hook))
    (add-hook c-mode-hook #'enable-ggtags-mode)))


;;; PlantUML
;; Download plantuml.jar first, put it in ~/plantuml.jar.
;; wget https://netcologne.dl.sourceforge.net/project/plantuml/plantuml.jar
;;
;; "C-c C-c" => plantuml-preview => Show UML Diagram
(when (maybe-require-package 'plantuml-mode)
  (setq plantuml-default-exec-mode 'jar))  ;; default server mode is not working


;;; Pyim - "Chinese Pinyin Input Method"
;; "C-\" => toggle-input-method
(when (and (maybe-require-package 'pyim)
           (maybe-require-package 'pyim-basedict))
  (require 'pyim)
  (pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
  (defun hydra-pyim-start ()
    (interactive)
    (pyim-activate)))


;;; Beacon — Never lose your cursor again
(when (maybe-require-package 'beacon)
  (setq-default beacon-lighter "")
  (setq-default beacon-size 50)
  (setq-default beacon-color "#00cd00") ;; highlight with green color
  (setq-default beacon-blink-duration 0.5)
  (add-hook 'after-init-hook 'beacon-mode))


;;; Expand region
(use-package expand-region
  :ensure t
  :bind ("M-8" . er/expand-region)
  :config
  (when (treesit-available-p)
    (defun treesit-mark-bigger-node ()
      "Use tree-sitter to mark regions."
      (let* ((root (treesit-buffer-root-node))
             (node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
             (node-start (treesit-node-start node))
             (node-end (treesit-node-end node)))
        ;; Node fits the region exactly. Try its parent node instead.
        (when (and (= (region-beginning) node-start) (= (region-end) node-end))
          (when-let ((node (treesit-node-parent node)))
            (setq node-start (treesit-node-start node)
                  node-end (treesit-node-end node))))
        (set-mark node-end)
        (goto-char node-start)))
    (add-to-list 'er/try-expand-list 'treesit-mark-bigger-node)))


;;; Yaml mode
(when (maybe-require-package 'yaml-mode)
  (add-auto-mode 'yaml-mode "\\.yml\\.erb\\'")
  (add-hook 'yaml-mode-hook 'goto-address-prog-mode))


;;; Call-graph
(use-package call-graph
  :ensure t
  :config
  (global-set-key (kbd "C-c g") #'call-graph)
  (customize-set-variable 'cg-path-to-global "/home/ehumche/private/gtags-6.6.6/bin/")
  (customize-set-variable 'imenu-max-item-length "Unlimited")
  (customize-set-variable 'cg-display-func-args t)
  (dolist (filter '("grep -v \"Test/\""
                    "grep -v \"Stub/\""
                    "grep -v \"_SUITE/\""
                    "grep -v \"/test-src/\""
                    "grep -v \"/TestPkg/\""
                    "grep -v \"/unittest/\""
                    "grep -v \"/test_src/\""
                    "grep -v \"/signalflowtest/\""
                    "grep -v \"/ct/\""))
    (add-to-list 'cg-search-filters filter))
  (setq cg-search-backend "Global"
        cg-path-to-git-repo "/workspace/git/ehumche/epg__3/"))


;;; org-ivy-search
(maybe-require-package 'org-ivy-search)


;;; package-lint
(maybe-require-package 'package-lint)


;;; diff-hl
(when (maybe-require-package 'diff-hl)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'after-init-hook 'global-diff-hl-mode)

  (with-eval-after-load 'diff-hl
    (define-key diff-hl-mode-map
      (kbd "<left-fringe> <mouse-1>")
      #'diff-hl-diff-goto-hunk)))


;;; csv-mode
(require-package 'csv-mode)
(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")
(setq csv-separators '("," ";" "|" " "))


;;; nyan-mode
(when (maybe-require-package 'nyan-mode)
  (setq-default nyan-animate-nyancat (display-graphic-p)
                nyan-wavy-trail t)
  (add-hook 'after-init-hook 'nyan-mode))


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
  (add-hook 'after-init-hook 'which-key-mode)
  (setq-default which-key-idle-delay 1.5))


;;; move-text
(require-package 'move-text)


;;; smart-mode-line
(when (maybe-require-package 'smart-mode-line)
  (sml/setup))


;;; gist - share code, notes, and snippets
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


;;; super-save-mode
(when (maybe-require-package 'super-save)
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t
        auto-save-default nil
        super-save-remote-files nil))


;;; avy-mode - Efficient cursor movement
(when (maybe-require-package 'avy)
  (global-set-key (kbd "M-:") 'avy-goto-char-timer)
  (setq avy-background t
        avy-style 'at-full))


;;; rainbow-delimiters
(when (require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


;;; scratch - Create buffers in same mode
(maybe-require-package 'scratch)


;; Docker
(when (maybe-require-package 'docker)
  (use-package docker
    :bind ("C-c d" . docker)
    :init (setq docker-image-run-arguments '("-i" "-t" "--rm")
                docker-container-shell-file-name "/bin/bash")))


;; PDF reader
(when (display-graphic-p)
  (when (maybe-require-package 'pdf-tools)
    (use-package pdf-tools
      :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
      :defines pdf-annot-activate-created-annotations
      :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
      :magic ("%PDF" . pdf-view-mode)
      :bind (:map pdf-view-mode-map
                  ("C-s" . isearch-forward))
      :init
      (setq pdf-view-midnight-colors '("#ededed" . "#21242b")
            pdf-annot-activate-created-annotations t)
      :config
      ;; WORKAROUND: Fix compilation errors on macOS.
      ;; @see https://github.com/politza/pdf-tools/issues/480
      (when *is-a-mac*
        (setenv "PKG_CONFIG_PATH"
                "/usr/local/lib/pkgconfig:/usr/local/opt/libffi/lib/pkgconfig"))
      (pdf-tools-install t nil t t)

      ;; Recover last viewed position
      (when (>= emacs-major-version 26)
        (use-package pdf-view-restore
          :hook (pdf-view-mode . pdf-view-restore-mode)
          :init (setq pdf-view-restore-filename
                      (locate-user-emacs-file ".pdf-view-restore")))))))


;; Epub reader
(when (maybe-require-package 'nov)
  (use-package nov
    :mode ("\\.epub\\'" . nov-mode)
    :preface
    (defun my-nov-setup ()
      (visual-line-mode 1)
      (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5)
      (if (fboundp 'olivetti-mode) (olivetti-mode 1)))
    :hook (nov-mode . my-nov-setup)))


;; Nice writing
(when (maybe-require-package 'olivetti)
  (use-package olivetti
    :diminish
    :init (setq olivetti-body-width 0.618)))


;; Music player
(when (maybe-require-package 'bongo)
  (use-package bongo
    :functions (bongo-add-dired-files
                dired-get-filename
                dired-marker-regexp
                dired-move-to-filename)
    :commands (bongo-buffer
               bongo-library-buffer-p
               bongo-library-buffer)
    :init
    (with-eval-after-load 'dired
      (defun bongo-add-dired-files ()
        "Add marked files to Bongo library"
        (interactive)
        (bongo-buffer)
        (let (file (files nil))
          (dired-map-over-marks
           (setq file (dired-get-filename)
                 files (append files (list file)))
           nil t)
          (with-bongo-library-buffer
           (mapc 'bongo-insert-file files)))
        (bongo-switch-buffers))
      (bind-key "b" #'bongo-add-dired-files dired-mode-map))))


;; stock-tracker
(when (maybe-require-package 'stock-tracker)
  (customize-set-variable 'stock-tracker-list-of-stocks
                          '("0688516"
                            "0601012"
                            "0601058"
                            "0603707"
                            "0603131"
                            "1002585"
                            "0600905"
                            "1002594"
                            "1300014"
                            "1300750"
                            "0600563")))


;; lua-mode
(when (maybe-require-package 'lua-mode)
  (use-package lua-mode))


;; Preview files in dired
(when (maybe-require-package 'peep-dired)
  (use-package peep-dired
    :ensure t
    :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
    :bind (:map dired-mode-map
                ("P" . peep-dired))))


;; ipretty
(when (maybe-require-package 'ipretty)
  (add-hook 'after-init-hook 'ipretty-mode))


;; Automatic byte compilation
(when (maybe-require-package 'auto-compile)
  (setq auto-compile-delete-stray-dest nil)
  (add-hook 'after-init-hook 'auto-compile-on-save-mode)
  (add-hook 'after-init-hook 'auto-compile-on-load-mode)
  ;; Load .el if newer than corresponding .elc
  (setq load-prefer-newer t))


;; everlasting-scratch
(when (maybe-require-package 'everlasting-scratch)
  (add-hook 'after-init-hook 'everlasting-scratch-mode))


;; Extras for theme editing
(when (maybe-require-package 'rainbow-mode)
  (defun sanityinc/enable-rainbow-mode-if-theme ()
    (when (and (buffer-file-name) (string-match-p "\\(color-theme-\\|-theme\\.el\\)" (buffer-file-name)))
      (rainbow-mode)))
  (add-hook 'emacs-lisp-mode-hook 'sanityinc/enable-rainbow-mode-if-theme)
  (add-hook 'help-mode-hook 'rainbow-mode))


;;; diminish modes
(when (maybe-require-package 'diminish)
  (require 'diminish)
  (defun enable-diminish ()
    "Diminish various modes"
    (diminish 'eldoc-mode)
    (diminish 'undo-tree-mode)
    (diminish 'ggtags-mode)
    (diminish 'symbol-overlay-mode)
    (diminish 'super-save-mode)
    (diminish 'rainbow-delimiters-mode)
    (diminish 'which-key-mode)
    (diminish 'abbrev-mode)
    (diminish 'flymake-mode)
    (diminish 'ivy-mode)
    (diminish 'counsel-mode)
    (diminish 'yard-mode)
    (diminish 'whitespace-cleanup-mode)
    (diminish 'rainbow-mode)
    (diminish 'view-mode)
    (diminish 'hs-minor-mode))
  (add-hook 'prog-mode-hook 'enable-diminish))


;;; improve long lines performance
(when (maybe-require-package 'so-long)
  (use-package so-long
    :hook (after-init . global-so-long-mode)))


;;; elisp-demos
(when (maybe-require-package 'elisp-demos)
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))


;;; show indentation
(when (maybe-require-package 'indent-guide)
  (when (display-graphic-p)
    (require 'indent-guide)
    (indent-guide-global-mode)))


;;; download manager within emacs
(when (maybe-require-package 'download-region)
  (require 'download-region)
  (setq download-region-max-downloads 5))


;;; highlight TODO and similar keywords in comments and strings
(when (maybe-require-package 'hl-todo)
  (use-package hl-todo
    :custom-face
    (hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :underline nil :inverse-video t))))
    :bind (:map hl-todo-mode-map
                ([C-f3] . hl-todo-occur)
                ("C-c t p" . hl-todo-previous)
                ("C-c t n" . hl-todo-next)
                ("C-c t o" . hl-todo-occur)
                ("C-c t i" . hl-todo-insert))
    :hook (after-init . global-hl-todo-mode)
    :init (setq hl-todo-require-punctuation t
                hl-todo-highlight-punctuation ":")
    :config
    (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
      (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
    (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
      (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces))))


;;; dims surrounding text
(use-package focus
  :ensure t
  :hook (prog-mode . focus-mode))


;;; process
(use-package proced
    :ensure t
    :init
    (setq-default proced-format 'verbose)
    (setq proced-auto-update-flag t
          proced-auto-update-interval 3))


;;; list-environment
(use-package list-environment :ensure t)


;;; system services/daemons
(use-package daemons :ensure t)


;;; cheatsheets for console commands
(use-package tldr :ensure t)


;;; symbol-overlay
;; "p" => jump-prev
;; "n" => jump-next
;; "<" => jump-first
;; ">" => jump-last
;; "t" => toggle
;; "r" => rename
;; "w" => save
(when (maybe-require-package 'symbol-overlay)
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook yaml-mode-hook conf-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  (with-eval-after-load 'symbol-overlay
    ;; (define-key symbol-overlay-mode-map (kbd "p") #'symbol-overlay-jump-prev)
    ;; (define-key symbol-overlay-mode-map (kbd "n") #'symbol-overlay-jump-next)
    (define-key symbol-overlay-mode-map (kbd "M-i") #'symbol-overlay-put)
    (define-key symbol-overlay-mode-map (kbd "M-I") #'symbol-overlay-remove-all)
    (define-key symbol-overlay-mode-map (kbd "M-n") #'symbol-overlay-switch-forward)
    (define-key symbol-overlay-mode-map (kbd "M-p") #'symbol-overlay-switch-backward)))


;; emacs-29 new features
(when (>= emacs-major-version 29)
  ;; tree-sitter: language parser
  ;; @see https://github.com/casouri/tree-sitter-module
  ;;      https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter
  ;;      https://blog.markhepburn.com/posts/experimenting-with-the-built-in-treesitter-support-in-emacs
  (when (treesit-available-p)
    (use-package treesit
      :ensure nil
      :init (setq treesit-extra-load-path
                  (list (expand-file-name "treesit-grammars" user-emacs-directory))
                  major-mode-remap-alist
                  ;; modes powered by treesit
                  '((c-mode          . c-ts-mode)
                    (c++-mode        . c++-ts-mode)
                    (cmake-mode      . cmake-ts-mode)
                    (conf-toml-mode  . toml-ts-mode)
                    (csharp-mode     . csharp-ts-mode)
                    (css-mode        . css-ts-mode)
                    (dockerfile-mode . dockerfile-ts-mode)
                    (go-mode         . go-ts-mode)
                    (java-mode       . java-ts-mode)
                    (json-mode       . json-ts-mode)
                    (js-json-mode    . json-ts-mode)
                    (js-mode         . js-ts-mode)
                    (python-mode     . python-ts-mode)
                    (rust-mode       . rust-ts-mode)
                    (sh-mode         . bash-ts-mode)
                    (typescript-mode . typescript-ts-mode))
                  treesit-font-lock-level 4))))


;;; chatgpt client
;;  "gptel"      => start a chatgpt session
;;  "C-c RET"    => send to chatgpt
;;  "gptel-send" => send to chatgpt
(use-package gptel
    :ensure t
    :config
    ;; @see https://www.xnbeast.com/create-openai-chatgpt-account/
    ;; @see https://platform.openai.com/account/api-keys
    (setq gptel-api-key (gptel-api-key-from-auth-source)
          gptel-default-mode #'markdown-mode))


;;; Emacs X Window Manager
(when (and (display-graphic-p) *is-linux*)
  (use-package exwm
    :ensure t
    :config
    ;; @see https://github.com/ch11ng/exwm/wiki#bootstrap
    (require 'exwm)
    (require 'exwm-config)
    ;; replace existing window manager by default
    (customize-set-variable 'exwm-replace t)
    (exwm-enable)
    ;;; enable editing for app inside EXWM
    ;;  "C-c C-RET" => start editing
    ;;  "C-c C-c"   => finish editing
    ;;  "C-c C-k"   => cancel editing
    (use-package exwm-edit :ensure t
      :config
      (exwm-input-set-key (kbd "C-c C-<return>") #'exwm-edit--compose))
    ;; fix ediff conflict with EXWM issue
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)
    ;; leave only one workspace
    (while (> (exwm-workspace--count) 1) (exwm-workspace-delete))
    ;; @see https://github.com/ch11ng/exwm/issues/198
    ;; show window title in EXWM buffer name
    (defun exwm-rename-buffer ()
      (interactive)
      (exwm-workspace-rename-buffer
       (concat exwm-class-name ":"
               (if (<= (length exwm-title) 50) exwm-title
                 (concat (substring exwm-title 0 49) "...")))))
    (add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
    (add-hook 'exwm-update-title-hook 'exwm-rename-buffer)
    ;; @see https://stackoverflow.com/questions/3679930/how-to-automatically-remove-or-prevent-popping-up-async-shell-command-in-ema
    (defun async-shell-hide-popping-window (orig &rest args) (save-window-excursion (apply orig args)))
    (advice-add 'async-shell-command :around 'async-shell-hide-popping-window)
    ;; kill process-buffer after process exited
    (defun kill-async-buffer-when-done (process signal)
      (when (memq (process-status process) '(exit signal))
        (kill-buffer (process-buffer process))))
    (advice-add 'shell-command-sentinel :after 'kill-async-buffer-when-done)
    ;; @see https://www.reddit.com/r/emacs/comments/z75ric/how_to_spawn_external_terminal_in_exwm/
    (defun my-exwm-exterm () (interactive) (async-shell-command "mate-terminal"))))


;;; replacement for replace-regexp
(use-package visual-regexp
  :ensure t
  :config
  (define-key global-map (kbd "C-c r") 'vr/replace)
  (define-key global-map (kbd "C-c q") 'vr/query-replace)
  ;; if you use multiple-cursors, this is for you:
  (define-key global-map (kbd "C-c m") 'vr/mc-mark))


;;; move to the beginning/end of code
(use-package mwim
  :ensure t
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))


;;; other setting
(require 'init-hydra)
(require 'init-git)
(require 'init-dired)
(require 'init-isearch)
(require 'init-ibuffer)
(require 'init-ivy)
(require 'init-minibuffer)
(require 'init-org)
(require 'init-eww)
(require 'init-flymake)
(require 'init-http)
(require 'init-calendar)
(require 'ipc-udp)
(require 'init-eglot)
(require 'init-corfu)
(require 'init-terminals)
(require 'init-ui)


(provide 'init-3rd-party)
;;; init-3rd-party.el ends here
