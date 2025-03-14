;; init.el --- Entry file for emacs configuration.      -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Shadowsocks
;;                                      GFW
;;                                       |
;; Emacs <-> SS-local (localhost:1080) <-|-> SS-server <-> Internet
;;                                       |
;;
;; Buy VPS and setup SS on bandwagonhost
;; Install SS-GUI and config SS address, pwd..
;;
;;;
;;; Code:

;;----------------------------------------------------------------------------
;; proxy setting for url-retrieve which is emacs's default http client.
;;----------------------------------------------------------------------------
;; (customize-set-variable 'url-proxy-services
;;                         '(("no_proxy" . "^.*example.com")
;;                           ("https" . "localhost:1080")
;;                           ("http" . "localhost:1080")
;;                           ;; socks not working, @see url-default-find-proxy-for-url
;;                           ;; ("socks5" . "localhost:1080")
;;                           ))


;;; Directory structure
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
(when (<= emacs-major-version 21)
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-21")))
(require 'init-utils)
;; Machinery for installing required packages
(when (is-modern-emacs) (require 'init-elpa))

;;----------------------------------------------------------------------------
;; create tags
;;----------------------------------------------------------------------------

;;; create tags with 'universal-ctags'
(defun create-tags (dir-name1 tag-file-name dir-name2)
  "Create tags file TAG-FILE-NAME for directory DIR-NAME2 in directory DIR-NAME1."
  (interactive
   "DDirectory to save tag-file: \nsName of tag-file (TAGS): \nDDirectory to be taged: ")
  (if (string= "" tag-file-name) (setq tag-file-name "TAGS"))
  (shell-command
   ;; use universal-ctags instead of emacs ctags
   (format "~/private/universal-ctags/bin/ctags -f %s/%s -e -R %s" dir-name1 tag-file-name (directory-file-name dir-name2)))
  (message "create-tags succeed !"))
(defalias 'ct 'create-tags)

;; set tags file list and cpp file search path
(setq tags-file-name nil
      tags-table-list
      '(
        ;; "~/my_tag_files/TAGS"         ;; not used for now
        ;; "~/my_tag_files/STL_TAGS"     ;; stl(gcc) headers
        ;; "~/my_tag_files/BOOST_TAGS"   ;; boost headers
        )
      ;; add stl/boost/project path into ff-find-other-file's search dir
      cc-search-directories '("."
                              "../include/*"
                              "../src/*"
                              "/usr/include"
                              "/usr/local/include/*"
                              "/usr/include/c++/4.4.7/*"
                              "/usr/include/boost/*")
      ;; Don't ask before reverting the TAGS files
      tags-revert-without-query t
      ;; Do case-sensitive tag searches
      tags-case-fold-search nil)

(if *is-windows*
    ;; on win-32, set threshhold to 511MB
    (setq large-file-warning-threshold (* 511 (expt 1024 2)))
  ;; Don't warn unless TAGS files are bigger than 1GB
  (setq large-file-warning-threshold (expt 1024 3)))


;; steps to create gtags for STL and Boost
;; cd ~/my_tag_files
;; ln -s /usr/include/c++/4.4.7 .
;; ln -s /usr/include/boost .
;; gtags
;; add ~/my_tag_files into environment variable "GTAGSLIBPATH"
(setq ggtags-enable-navigation-keys nil
      ggtags-oversize-limit 1048576
      ggtags-exclude-modes '(ttcn-3-mode))


;;; Allow access from emacsclient
(when (is-modern-emacs)
  (require 'server)
  (unless (server-running-p)
    (server-start)))


;;; Kill all processes when closing emacs
;;  @see https://stackoverflow.com/questions/2706527/make-emacs-stop-asking-active-processes-exist-kill-them-and-exit-anyway
(require 'cl-lib)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))


;;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024)
      process-adaptive-read-buffering nil)


;;; Load configs for specific features and modes
(require 'init-cc)
(require 'tempo-c-cpp)
(require 'init-alias)
(when (= emacs-major-version 21)
  (require 'missing)
  (require 'syntax))
(when (is-modern-emacs)
  (require 'init-3rd-party)
  (require 'init-windows)
  (require 'init-nxml)
  (require 'init-css)
  (require 'init-javascript)
  (require 'init-ruby)
  (require 'init-python)
  (require 'init-shell)
  (require 'init-grep)
  (require 'init-compile))
(require 'init-basics)
(require 'init-imenu)
(require 'init-session)
(require 'init-productivity)
;; measure startup time at the end
(require 'init-benchmarking)

(provide 'init)
;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
