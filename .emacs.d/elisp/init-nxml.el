;;----------------------------------------------------------------------------
;; nxml mode configuration
;;----------------------------------------------------------------------------

(add-auto-mode
 'nxml-mode
 (concat "\\."
         (regexp-opt
          '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss"
            "gpx" "tcx" "plist"))
         "\\'"))

(setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
(setq nxml-slash-auto-complete-flag t)


;; See: http://sinewalker.wordpress.com/2008/06/26/pretty-printing-xml-with-emacs-nxml-mode/
(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

;; prettify the xml in the active region
(defalias 'prt 'bf-pretty-print-xml-region)

;;----------------------------------------------------------------------------
;; Integration with tidy for html + xml
;;----------------------------------------------------------------------------

(defun sanityinc/tidy-buffer-xml (beg end)
  "Run \"tidy -xml\" on the region from BEG to END, or whole buffer."
  (interactive "r")
  (unless (use-region-p)
    (setq beg (point-min)
          end (point-max)))
  (shell-command-on-region beg end "tidy -xml -q -i" (current-buffer) t "*tidy-errors*" t))


;; company-nxml
(after-load 'company
  (add-hook 'nxml-mode-hook (lambda () (sanityinc/local-push-company-backend 'company-nxml))))

(provide 'init-nxml)
