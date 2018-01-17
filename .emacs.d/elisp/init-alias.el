;;----------------------------------------------------------------------------
;; use alias to shorten commands
;;----------------------------------------------------------------------------

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

;; misc
(defalias 'afa 'apply-function-to-region-lines-with-args)
(defalias 'af 'apply-function-to-region-lines-without-args)

(defalias 'cc 'clone-indirect-buffer)

(provide 'init-alias)
