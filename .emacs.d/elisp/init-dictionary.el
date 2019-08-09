;;; init-dictionary.el --- dictionary config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'dictionary)

(autoload 'dictionary-search "dictionary"
  "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary"
  "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary-lookup-definition "dictionary"
  "Unconditionally lookup the word at point." t)
(autoload 'dictionary "dictionary"
  "Create a new dictionary buffer" t)
(autoload 'dictionary-mouse-popup-matching-words "dictionary"
  "Display entries matching the word at the cursor" t)
(autoload 'dictionary-popup-matching-words "dictionary"
  "Display entries matching the word at the point" t)
(autoload 'dictionary-tooltip-mode "dictionary"
  "Display tooltips for the current word" t)
(unless (boundp 'running-xemacs)
  (autoload 'global-dictionary-tooltip-mode "dictionary"
    "Enable/disable dictionary-tooltip-mode for all buffers" t))

;; Bypass custom-add-load to speed startup.
(put 'dictionary       'custom-loads '(dictionary))
(put 'dictionary-group 'custom-loads '(dictionary))

(global-set-key (kbd "\C-cs") #'dictionary-search)
(global-set-key (kbd "\C-cm") #'dictionary-match-words)


;; @see https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-utils.el
;; Youdao Dictionary
(when (maybe-require-package 'youdao-dictionary)
  (use-package youdao-dictionary
    :functions (posframe-show
                posframe-hide)
    :commands (youdao-dictionary-mode
               youdao-dictionary--region-or-word
               youdao-dictionary--format-result)
    :bind (("C-c y" . youdao-dictionary-search-at-point)
           ("C-c Y" . youdao-dictionary-search))
    :config
    ;; Cache documents
    (setq url-automatic-caching t)

    ;; Enable Chinese word segmentation support (支持中文分词)
    (setq youdao-dictionary-use-chinese-word-segmentation t)))


(provide 'init-dictionary)
;;; init-dictionary.el ends here
