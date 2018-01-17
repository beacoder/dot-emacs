;;----------------------------------------------------------------------------
;; global key-bindings
;;----------------------------------------------------------------------------

;; @see http://steve.yegge.googlepages.com/effective-emacs
;; M-x may not be avalible everywhere, e.g: In term-char-mode, or when there is no Meta key.
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(defun backward-kill-word-or-region ()
  "do backward-kill-word or kill-region.
Enhancement to 'Prefer backward-kill-word over Backspace'.
URL `https://sites.google.com/site/steveyegge2/effective-emacs'"
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
(global-set-key (kbd "C-w") 'backward-kill-word-or-region)

;; bind pop-tag-mark
(global-set-key (kbd "M-?") 'pop-tag-mark)

(after-load "xref"
  (progn
    (define-key esc-map "." #'xref-find-definitions)
    (define-key esc-map "?" #'xref-pop-marker-stack)
    (define-key esc-map "]" #'xref-find-apropos)
    
    ;; don't prompt if we have candiddates at point
    (defun xref-find-apropos (pattern)
      "Find all meaningful symbols that match PATTERN.
The argument has the same meaning as in `apropos'."
      (interactive (list (smart/read-from-minibuffer "Search string")))
      (require 'apropos)
      (xref--find-xrefs pattern 'apropos
                        (apropos-parse-pattern
                         (if (string-equal (regexp-quote pattern) pattern)
                             ;; Split into words
                             (or (split-string pattern "[ \t]+" t)
                                 (user-error "No word list given"))
                           pattern))
                        nil))))

;; Handy way of navigating forward and backward.
;; @see http://stackoverflow.com/questions/3393834/how-to-move-forward-and-backward-in-emacs-mark-ring
(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (let ((pos (marker-position (car (last mark-ring)))))
      (if (not (= (point) pos))
          (goto-char pos)
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) pos)
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))))
(global-set-key (kbd "C-c p") 'pop-to-mark-command)        
(global-set-key (kbd "C-c n") 'unpop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;; bind goto-line command
(global-set-key (kbd "M-g M-g") 'goto-line)

;; enable recentf-mode
(require 'recentf)
(add-hook 'after-init-hook 'recentf-mode)
(setq recentf-max-menu-items  100
      recentf-max-saved-items 1000
      recentf-exclude '(
                     ;; "/tmp/"
                        "/ssh:"
                        "/sudo:"
                     ;; "/home/[a-z]\+/\\."
                        ))
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; @see http://stackoverflow.com/questions/2068697/emacs-is-slow-opening-recent-files
(setq recentf-keep '(file-remote-p file-readable-p))

(global-set-key (kbd "\C-cl") 'list-matching-lines)

;;----------------------------------------------------------------------------
;; mode setting
;;----------------------------------------------------------------------------

;; incremental picking of buffers
;; (when (fboundp 'iswitchb-mode) (iswitchb-mode t))

;; auto-refresh all buffers when files have changed on disk.
(add-hook 'after-init-hook 'global-auto-revert-mode)
;; I use this mode for log files (Emacs’s version of tail -f) to easily search through the logs.
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))

;; enable ibuffer-mode
(when (fboundp 'ibuffer-mode)
  (add-hook 'after-init-hook 'ibuffer-mode))
(setq ibuffer-filter-group-name-face 'font-lock-doc-face)

;; enable electric-pair-mode
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode)
  (add-hook 'after-init-hook 'show-paren-mode)
  (setq show-paren-delay 0))

;; highlight the active region
(add-hook 'after-init-hook 'transient-mark-mode)

;; hide menu bar
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; hide toolbar
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; hide scroll bar
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; enable the delete-selection-mode
(add-hook 'after-init-hook 'delete-selection-mode)

;; enable global high light
(add-hook 'after-init-hook 'global-font-lock-mode)

;; set text-mode as default major mode
(setq default-major-mode 'text-mode)

;; enable emacs to open image file
(add-hook 'after-init-hook 'auto-image-file-mode)

;; enable line-number-mode
(add-hook 'after-init-hook 'line-number-mode)

;; enable column-number-mode
(add-hook 'after-init-hook 'column-number-mode)

;; display Lambda as λ
(when (fboundp 'prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode))

;; don't show line-number in left margin
(when (fboundp 'global-linum-mode) (global-linum-mode -1))

;; replace tab with spaces, use "C-q [tab]" to get a real tab
(setq-default indent-tabs-mode nil)

;; highlight tabulations
(setq-default highlight-tabs t)

;; show trailing white spaces
(setq-default show-trailing-whitespace t)

;; toggle whether to show trailing whitespace
(defun tf-toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace between t and nil"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (redraw-display))
(global-set-key (kbd "C-c C-SPC") 'tf-toggle-show-trailing-whitespace)

;; remove useless whitespaces before saving a file
;; (add-hook 'before-save-hook
;;           (lambda()
;;             (when (member major-mode '(c-mode c++-mode))
;;               (whitespace-cleanup)
;;               (delete-trailing-whitespace))))

;; disable formatting in text-mode
(add-hook 'text-mode-hook (lambda() (electric-indent-mode 0)))

;; display-occurence after occur-preve/next;
;; M-p => occur-preve, M-n => occur-next
(defadvice occur-prev (after display-prev activate)
  (save-excursion (occur-mode-display-occurrence)))
(defadvice occur-next (after display-next activate)
  (save-excursion (occur-mode-display-occurrence)))

;; automaticly enable view-mode after entering read-only-mode
;; view-mode: q => kill-buffer-if-not-modified
(setq view-read-only t)
(add-hook 'read-only-mode-hook
          (lambda ()
            (require 'view)
            (and buffer-read-only (setq view-exit-action 'kill-buffer-if-not-modified))))
(after-load 'view
  (define-key view-mode-map (kbd "p") 'View-scroll-line-backward)
  (define-key view-mode-map (kbd "n") 'View-scroll-line-forward))

;;----------------------------------------------------------------------------
;; setting locales
;;----------------------------------------------------------------------------

;; set locale to utf-8
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;----------------------------------------------------------------------------
;; some other settings
;;----------------------------------------------------------------------------

;; enable narrow
(put 'narrow-to-region 'disabled nil)

;; disable emacs system beep
(setq visible-bell nil)
(setq ring-bell-function (lambda () (message "*beep*")))

;; enlarge kill-ring-max value
(setq kill-ring-max 200)

;; show file's full path in title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; "DejaVu Sans Mono" is a nice open source font, good for programming
;;(when (display-graphic-p)
;;    (set-face-attribute 'default nil
;;                        :font "DejaVu Sans Mono-10:weight=normal"
;;                        :height 100))

;;(when (display-graphic-p)
;;   ;; "DejaVu Sans Mono" is a nice open source font family
;;   (set-face-attribute 'default nil
;;                       :font "-misc-dejavu lgc sans mono-medium-r-normal--0-0-0-0-m-0-iso8859-1"
;;                       :height 100))

;; set minibuffer-prompt color
(set-face-foreground 'minibuffer-prompt "pink")

;;----------------------------------------------------------------------------
;; isearch setting
;;----------------------------------------------------------------------------

;; @see https://github.com/Hawstein/my-emacs/blob/master/_emacs/isearch-face-settings.el
(defun isearch-face-settings ()
  "Face settings for `isearch'."

  ;; (set-face-foreground 'isearch "red")
  ;; (set-face-background 'isearch "blue")

  (set-face-foreground 'isearch "white")
  (set-face-background 'isearch "red")
  (when (is-modern-emacs)
    ;; (set-face-foreground 'lazy-highlight "black")
    ;; (set-face-background 'lazy-highlight "white")

    (set-face-foreground 'lazy-highlight "white")
    (set-face-background 'lazy-highlight "pink"))
  (custom-set-faces '(isearch-fail ((((class color)) (:background "red"))))))
(after-load "isearch" (isearch-face-settings))

;; Isearch convenience, space matches anything (non-greedy)
;; @see https://www.reddit.com/r/emacs/comments/3yxk2x/flexible_isearch_without_a_package/
(setq search-whitespace-regexp ".*?")

;; @see http://endlessparentheses.com/leave-the-cursor-at-start-of-match-after-isearch.html
(add-hook 'isearch-mode-end-hook #'endless/goto-match-beginning)
(defun endless/goto-match-beginning ()
    "Go to the start of current isearch match.
Use in `isearch-mode-end-hook'."
    (when (and isearch-forward
               (number-or-marker-p isearch-other-end)
               (not mark-active)
               (not isearch-mode-end-hook-quit))
          (goto-char isearch-other-end)))

;; don't interfere with "mark-search-mark" operation
;; @see http://stackoverflow.com/questions/32002122
;; (add-hook 'isearch-mode-hook #'jrh-isearch-with-region)
;; (defun jrh-isearch-with-region ()
;;   "Use region as the isearch text."
;;   (when mark-active
;;     (let ((region (funcall region-extract-function nil)))
;;       (deactivate-mark)
;;       (isearch-push-state)
;;       (isearch-yank-string region))))

;; set cursor color
(add-hook 'window-setup-hook '(lambda () (set-cursor-color "white")))
(add-hook 'after-make-frame-functions '(lambda (f) (with-selected-frame f (set-cursor-color "white"))))
(setq cursor-type 'box)

;; save place in files between sessions
(require 'saveplace)
(setq-default save-place t)

;; ediff splits window horizontally
(setq ediff-split-window-function 'split-window-horizontally
      ;; ediff-diff-options (concat " -w " ediff-diff-options)
      )
;; skip regions that differ only in the white space and line breaks.
(setq-default ediff-ignore-similar-regions t)

(defun ediff-face-settings ()
  "Face settings for `ediff'."
  (progn
    (custom-set-faces '(ediff-current-diff-A
                        ((((type tty)) :background "yellow" :foreground "blue")
                         (t :background "DarkSeaGreen3" :foreground "blue violet"))))
    (custom-set-faces '(ediff-fine-diff-A
                        ((((type tty)) :background "blue" :foreground "white")
                         (t :background "gold1" :foreground "red"))))
    (custom-set-faces '(ediff-current-diff-B
                        ((((type tty)) :background "yellow" :foreground "black")
                         (t :background "DodgerBlue1" :foreground "gray11"))))
    (custom-set-faces '(ediff-fine-diff-B
                        ((((type tty)) :background "cyan" :foreground "red")
                         (t :background "chocolate2" :foreground "dark slate blue"))))
    (custom-set-faces '(ediff-current-diff-C
                        ((((type tty)) :background "yellow" :foreground "blue")
                         (t :background "DarkSeaGreen3" :foreground "blue violet"))))
    (custom-set-faces '(ediff-fine-diff-C
                        ((((type tty)) :background "blue" :foreground "white")
                         (t :background "gold1" :foreground "red"))))))
(after-load "ediff" (ediff-face-settings))


;; show elisp error backtrace
;; (setq debug-on-error t)

;; enable flyspell in text-mode
;;(dolist (hook '(text-mode-hook))
;;  (add-hook hook (lambda () (flyspell-mode 1))))
;;(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
;;  (add-hook hook (lambda () (flyspell-mode -1))))
;; improve performance
;;(setq flyspell-issue-message-flag nil)

;; fix Error: No word lists can be found for the language "zh_CN"
;; use apsell as ispell backend
(setq-default ispell-program-name "aspell")  
;; use American English as ispell default dictionary  
(ispell-change-dictionary "american" t)

;;----------------------------------------------------------------------------
;; Switch ibuffer format
;;----------------------------------------------------------------------------
(define-ibuffer-column filename-only
  (:name "Filename" :inline nil)
  (if (buffer-file-name buffer)
      (file-name-nondirectory (buffer-file-name buffer))
    (or dired-directory "")))

(setq old-ibuffer-format '((mark modified read-only " "
                                 (name 18 18 :left :elide)
                                 " "
                                 (size 9 -1 :right)
                                 " "
                                 (mode 16 16 :left :elide)
                                 " " filename-and-process)
                           (mark " "
                                 (name 16 -1)
                                 " " filename)))

(setq new-ibuffer-format '((mark modified read-only " "
                                 (name 18 18 :left :elide)
                                 " "
                                 (size 9 -1 :right)
                                 " "
                                 (mode 16 16 :left :elide)
                                 " " filename-only)
                           (mark " "
                                 (name 16 -1)
                                 " " filename)))

;; Modify the default ibuffer-formats
(defvar new-format-on nil)
(setq ibuffer-formats new-ibuffer-format
      new-format-on t)

(defun switch-ibuffer-format()
  (interactive)
  (if new-format-on
      (setq ibuffer-formats old-ibuffer-format
            new-format-on nil)
    (setq ibuffer-formats new-ibuffer-format
          new-format-on t))
  (list-buffers))

(after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "(") 'switch-ibuffer-format)
  (define-key ibuffer-mode-map (kbd ")") 'switch-ibuffer-format))

;;----------------------------------------------------------------------------
;; Nicer naming of buffers for files with identical names
;;----------------------------------------------------------------------------
(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;----------------------------------------------------------------------------
;; Measure startup time
;;----------------------------------------------------------------------------
(require 'init-benchmarking)

(provide 'init-basics)
