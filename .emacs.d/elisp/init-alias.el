;;; init-alias.el --- alias config -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Setup all kinds of alias here
;;;
;;; Code:

;; y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;; always use ibuffer
(when (> emacs-major-version 21)
  (defalias 'list-buffers 'ibuffer))

;; sh stands for shell
(defalias 'sh 'shell)

;; act stands for ansi-term
;; "C-x C-j" => activate term-line-mode (use emacs feature)
;; "C-c C-k" => activate character-mode (could use 'less')
(defalias 'act 'ansi-term)

;; esh stands for eshell
(defalias 'esh 'eshell)

;; eb stands for eval-buffer
(defalias 'eb 'eval-buffer)

;; ed stands for ediff
(defalias 'ed 'ediff)
(defalias 'edb 'ediff-buffers)
(defalias 'edd 'edirs)

(defalias 'ed3 'ediff3)
(defalias 'edb3 'ediff-buffers3)
(defalias 'edd3 'edirs3)

(defalias 'em 'ediff-merge)
(defalias 'emb 'ediff-merge-buffers)
(defalias 'emd 'edirs-merge)

;; shortcut for kill-some-buffers
(defalias 'ka 'kill-some-buffers)

;; goto-line
(defalias 'gl 'goto-line)

;; mainly used in terminal
(defalias 'sm 'set-mark-command)

;; cc-mode
(defalias 'ts 'tags-search)
(defalias 'tq 'tags-query-replace)

;; prettify the xml in the active region
(defalias 'pt 'sanityinc/pp-xml-region)
(defalias 'td 'sanityinc/tidy-buffer-xml)

;; misc
(defalias 'cl 'clone-indirect-buffer)
(defalias 'sc 'scratch)
(defalias 'wg 'wgrep-change-to-wgrep-mode)
(defalias 'dc 'desktop-change-dir)


(provide 'init-alias)
;;; init-alias.el ends here
