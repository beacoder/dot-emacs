;;----------------------------------------------------------------------------
;; Entry file for emacs configuration 
;;----------------------------------------------------------------------------

;; Use socks proxy for downloading packages from melpa
;; (setq url-gateway-local-host-regexp
;;       (concat "\\`" (regexp-opt '("localhost" "127.0.0.1")) "\\'")
;;       url-gateway-method 'socks
;;       socks-server '("Default server" "xx.xx.xx.xx" port 5))


(setq emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
(when (<= emacs-major-version 21)
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-21")))
(require 'init-utils)
;; Machinery for installing required packages
(when (is-modern-emacs) (require 'init-elpa))


;;; Multiple major modes
(when (is-modern-emacs)
  (require-package 'mmm-mode)
  (require 'mmm-auto)
  (setq mmm-global-mode 'buffers-with-submode-classes
        mmm-submode-decoration-level 2))


;;----------------------------------------------------------------------------
;; key-bindings for specific mode in emacs
;;----------------------------------------------------------------------------
;; the key definition only happen once
;; (eval-after-load "coffee-mode"
;;     '(define-key coffee-mode-map (kbd "C-c c" 'coffee-compile-file)))
;;
;; the key definition happens every time coffee-mode is enabled
;; (add-hook 'coffee-mode-hook
;;     (lambda ()
;;         (define-key coffee-mode-map (kbd "C-c c" 'coffee-compile-file))))

;;----------------------------------------------------------------------------
;; compilation setting
;;----------------------------------------------------------------------------
;;  (setq compilation-read-command nil
;;    compile-command "make clobber")
(defun mode-compile ()
  "compile with mode specific commands."
  (interactive)
  (setq command "make debug")
  (when (eq major-mode 'c++-mode)
    (setq command (concat (getenv "WS_ROOT") "/tools/bin/build -v -c Linux_x86_64")))
  (when (eq major-mode 'ttcn-3-mode)
    (setq command
          (concat (getenv "TTCN3_GGSN_ROOT_PATH") "/scripts/compile_ttcn.sh build" " && "
                  (getenv "TTCN3_GGSN_ROOT_PATH") "/scripts/copy_ttcn3.sh")))
  (compile command))

;;; create tags
;; requires 'Exuberant Ctags' installed
;; (defun create-tags (dir-name1 tag-file-name dir-name2)
;;   "Create tags file."
;;   (interactive
;;    "DDirectory where tag-file will be saved: \
;;     \nsName of the tag-file (TAGS): \
;;     \nDDirectory to be taged: ")
;;   (if (string= "" tag-file-name) (setq tag-file-name "TAGS"))
;;   (shell-command
;;    ;; use Exuberant Ctags instead of emacs ctags
;;    (format "/usr/bin/ctags -f %s/%s -e -R %s" dir-name1 tag-file-name (directory-file-name dir-name2)))
;;   (message "create-tags succeed !"))
;; (defalias 'ct 'create-tags)

;; set tags file lists
(setq tags-file-name nil
      tags-table-list
      '(
        "~/my_tag_files/TAGS"
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
(require 'cl)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (flet ((process-list ())) ad-do-it))


;;; config modeline format
(setq display-time-format "[%A %Y/%m/%d %H:%M Time-Zone:'%Z']"
      display-time-interval 60
      display-time-default-load-average nil
      display-time-mail-face 'custom-themed)

;; display time
(display-time-mode t);; config time format
(setq display-time-format "[%A %Y/%m/%d %H:%M Time-Zone:'%Z']"
      display-time-interval 60
      display-time-default-load-average nil
      display-time-mail-face 'custom-themed)

;; display time
(display-time-mode t)


;;; auto-save settings
(setq auto-save-default t
      auto-save-mode t
      auto-save-interval 300
      auto-save-timeout 60)


;;; Load configs for specific features and modes
(require 'init-register)
(require 'init-hippie-expand)
(require 'init-cc)
(require 'tempo-c-cpp)
(require 'init-alias)
(when (= emacs-major-version 21)
  (require 'missing)
  (require 'syntax))
(when (is-modern-emacs)
  (require 'init-3rd-party)
  (require 'init-lua)
  (require 'init-ido)
  (require 'init-windows)
  (require 'init-nxml)
  (require 'init-html)
  (require 'init-css)
  (require 'init-javascript)
  (require 'init-ruby)
  (require 'init-python)
  (require 'init-ttcn3)
  (require 'init-shell)
  (require 'init-grep)
  (require 'init-compile))
(require 'init-basics)
(require 'init-sessions)
(require 'init-cperl)
(require 'init-productivity)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
