;;; init-http.el --- Web/Http config -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Initialize web configurations
;;;
;;; Code:

(maybe-require-package 'httprepl)
(when (maybe-require-package 'restclient)
  (add-auto-mode 'restclient-mode "\\.rest\\'")

  (defun sanityinc/restclient ()
    (interactive)
    (with-current-buffer (get-buffer-create "*restclient*")
      (restclient-mode)
      (pop-to-buffer (current-buffer)))))


;; Webkit browser
(use-package xwidget
  :ensure nil
  :if (featurep 'xwidget-internal)
  :bind (("C-c w" . xwidget-webkit-browse-url)
         :map xwidget-webkit-mode-map
         ("h" . xwidget-hydra/body))
  :pretty-hydra
  ((:title (pretty-hydra-title "Webkit" 'faicon "chrome" :face 'all-the-icons-blue)
           :color amaranth :quit-key "q")
   ("Navigate"
    (("b" xwidget-webkit-back "back")
     ("f" xwidget-webkit-forward "forward")
     ("r" xwidget-webkit-reload "refresh")
     ("SPC" xwidget-webkit-scroll-up "scroll up")
     ("DEL" xwidget-webkit-scroll-down "scroll down")
     ("S-SPC" xwidget-webkit-scroll-down "scroll down"))
    "Zoom"
    (("+" xwidget-webkit-zoom-in "zoom in")
     ("=" xwidget-webkit-zoom-in "zoom in")
     ("-" xwidget-webkit-zoom-out "zoom out"))
    "Misc"
    (("g" xwidget-webkit-browse-url "browse url" :exit t)
     ("u" xwidget-webkit-current-url "show url" :exit t)
     ("v" xwwp-follow-link "follow link" :exit t)
     ("w" xwidget-webkit-current-url-message-kill "copy url" :exit t)
     ("?" describe-mode "help" :exit t)
     ("Q" quit-window "quit" :exit t))))
  :init
  ;; Link navigation
  (when (maybe-require-package 'xwwp-follow-link-ivy)
    (use-package xwwp-follow-link-ivy
      :after ivy
      :bind (("C-c x" . xwwp)
             :map xwidget-webkit-mode-map
             ("v" . xwwp-follow-link))
      :init (setq xwwp-follow-link-completion-system 'ivy))))


(provide 'init-http)
;;; init-http.el ends here
