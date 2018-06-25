;;----------------------------------------------------------------------------
;; elfeed setting
;;----------------------------------------------------------------------------

;;; elfeed => web feeds client
(when (maybe-require-package 'elfeed)
  (use-package elfeed
    :bind ("M-F" . elfeed)
    :config
    (add-hook 'elfeed-search-update-hook
              #'(lambda () (selected-minor-mode -1)))
    (setq      elfeed-db-directory "~/.emacs.d/data/elfeed"
               elfeed-enclosure-default-dir "~/Downloads/elfeed"
               elfeed-feeds
               (quote
                ("http://feeds.feedburner.com/schneier/excerpts"
                 "https://www.reddit.com/new/.rss"
                 "http://feeds.feedburner.com/codinghorror/"
                 "http://blog.metatheorem.org/feed.xml"
                 "http://www.tuaw.com/category/mac/rss.xml"
                 "http://www.apple.com/main/rss/hotnews/hotnews.rss"
                 "http://daringfireball.net/index.xml"
                 "http://blog.higher-order.com/atom.xml"
                 "http://comonad.com/reader/feed/"
                 "http://byorgey.wordpress.com/feed/"
                 "http://jaspervdj.be/rss.xml"
                 "http://jeremykun.com/feed/"
                 "https://feeds.feedburner.com/RomanCheplyaka"
                 "http://tcsavage.org/atom.xml"
                 "http://sequence.complete.org/node/feed"
                 "http://www.serpentine.com/blog/feed/"
                 "http://chris-taylor.github.io/atom.xml"
                 "http://sigfpe.blogspot.com/feeds/posts/default"
                 "http://blog.typlab.com/feed/"
                 "http://intoverflow.wordpress.com/feed/"
                 "https://elvishjerricco.github.io/feed.xml"
                 "http://bartoszmilewski.wordpress.com/feed/"
                 "http://lambda.jstolarek.com/feed/"
                 "http://javran.github.io/atom.xml"
                 "http://donsbot.wordpress.com/feed/"
                 "http://dev.stephendiehl.com/fun/rss/atom.xml"
                 "http://jelv.is/blog/atom.xml"
                 "https://ocharles.org.uk/blog/posts.rss"
                 "http://duplode.github.io/rss.xml"
                 "http://themonadreader.wordpress.com/feed/"
                 "http://learningagdaandats.wordpress.com/feed/"
                 "http://paolocapriotti.com/atom.xml"
                 "http://www.stephendiehl.com/feed.rss"
                 "http://lambda-the-ultimate.org/rss.xml"
                 "http://www.joachim-breitner.de/blog_feed.rss"
                 "http://alpmestan.com/rss.xml"
                 "http://blog.ezyang.com/feed/atom/"
                 "http://lpuppet.banquise.net/atom.xml"
                 "http://patternsinfp.wordpress.com/feed/"
                 "http://blog.well-typed.com/feed/"
                 "http://feeds.bahai.org/bwns/rss"
                 "http://www.bahairights.org/feed/"
                 "https://stackoverflow.com/feeds/tag?tagnames=c%2b%2b&sort=newest"
                 "http://mukeshiiitm.wordpress.com/feed/"
                 "http://poleiro.info/atom.xml"
                 "http://pigworker.wordpress.com/feed/"
                 "http://osa1.net/rss.xml"
                 "http://metatheorem.wordpress.com/feed/"
                 "http://existentialtype.wordpress.com/feed/"
                 "https://golem.ph.utexas.edu/category/atom10.xml"
                 "http://www.carloangiuli.com/blog/feed/"
                 "http://syntaxexclamation.wordpress.com/feed/"
                 "http://feeds.feedburner.com/CoolTools"
                 "http://motherboard.vice.com/rss?trk_source=motherboard"
                 "http://feeds.arstechnica.com/arstechnica/gadgets")))))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
