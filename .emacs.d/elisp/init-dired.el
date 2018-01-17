;;----------------------------------------------------------------------------
;; dired setting
;;----------------------------------------------------------------------------

;; "A"   => dired-do-find-regexp
;; "Q"   => dired-do-find-regexp-and-replace
;; "M-q" => dired-do-query-replace-regexp

(require-package 'dired-details)
(require-package 'dired-details+)
(require-package 'dired+)
(require-package 'dired-sort)
(require-package 'dired-subtree)

;; In a file, how to go to its directory and place cursor on the file name
(global-set-key (kbd "C-x C-j") 'dired-jump)

;; allow dired to be able to delete or copy a whole dir.
;; "always" means no asking. "top" means ask once.
;; Any other symbol means ask each and every time for a dir and subdir.
(setq dired-recursive-copies (quote always)
      dired-recursive-deletes (quote top))

(setq-default diredp-hide-details-initially-flag nil
              ;; copy from one dired dir to the next dired dir shown in a split window
              dired-dwim-target t)

;; restore positions and markers after dired-view-file exits
(defadvice dired-view-file (around advice-dired-view-file activate)
  (interactive)
  (save-excursion ad-do-it))

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

;; (when (maybe-require-package 'diredfl)
;;   (after-load 'dired
;;     (diredfl-global-mode)))

(after-load 'dired
  (require 'dired+)
  (require 'dired-sort)
  (require 'dired-subtree)
  ;; the background color is awful, disable it.
  (setq dired-subtree-use-backgrounds nil)
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (add-hook 'dired-mode-hook
            (lambda () (guide-key/add-local-guide-key-sequence "%")))
  (define-key dired-mode-map "l" 'dired-subtree-insert)
  (define-key dired-mode-map "L" 'dired-subtree-remove)
  (define-key dired-mode-map (kbd "M-b") nil)
  (define-key dired-mode-map (kbd "M-r") nil)
  (define-key dired-mode-map (kbd "M-s") nil))

(when (maybe-require-package 'diff-hl)
  (after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(provide 'init-dired)
