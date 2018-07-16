;;----------------------------------------------------------------------------
;; elfeed setting
;;----------------------------------------------------------------------------

;;; elfeed => web feeds client
(when (maybe-require-package 'elfeed)
  (use-package elfeed
    :bind ("M-F" . elfeed)
    :config
    ;; (add-hook 'elfeed-search-update-hook
    ;;           #'(lambda () (selected-minor-mode -1)))
    (setq      elfeed-db-directory "~/.emacs.d/data/elfeed"
               elfeed-enclosure-default-dir "~/Downloads/elfeed"
               elfeed-feeds
               (quote
                ("http://feeds.feedburner.com/schneier/excerpts"
                 "http://feeds.feedburner.com/codinghorror/"
                 "http://www.tuaw.com/category/mac/rss.xml"
                 "http://www.apple.com/main/rss/hotnews/hotnews.rss"
                 "https://feeds.feedburner.com/RomanCheplyaka")))))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
