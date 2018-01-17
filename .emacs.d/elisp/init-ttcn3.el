;;----------------------------------------------------------------------------
;; ttcn3 mode settings
;;----------------------------------------------------------------------------

(require 'async)

(defun async-update-ttcn3-tags ()
  "Async udpate TTCN3 tags"
  (interactive)
  (let ()
    (message "Updating TTCN3 tags...")
    (async-start

     ;; START-FUNC -> Building TTCN3 tags
     `(lambda ()
        (message "Updating TTCN3 tags...")
        (shell-command
         (concat (getenv "TTCN3_GGSN_ROOT_PATH") "/scripts/compile_ttcn.sh tags")))

     ;; FINISH-FUNC -> Change relative path to absolute path
     `(lambda (&optional ignore)
        (with-temp-file
            "~/my_tag_files/TAGS"
          (insert-file-contents (concat (getenv "TTCN3_BUILD_PATH") "/TAGS"))
          (goto-char (point-min))
          ;; replace relative path with abosolute path
          (while (re-search-forward "^\.\./" nil t)
            (replace-match (concat (getenv "TTCN3_GGSN_ROOT_PATH") "/") t nil))
          (message "TTCN3 tags has been updated..."))))))

(after-load 'ttcn3
  ;; Allow my global binding of M-? to work when paredit is active
  (define-key ttcn3-mode-map (kbd "M-?") nil)
  (define-key ttcn3-mode-map (kbd ",") nil)
  (define-key ttcn3-mode-map (kbd "C-c C-s") nil)
  (define-key ttcn3-mode-map (kbd "C-c C-e") nil)

  ;; key-bindings used in ttcn3-mode
  (define-prefix-command 'ttcn3-map)
  (define-key ttcn3-mode-map "\C-xt" ttcn3-map)
  (define-key ttcn3-map (kbd "u") 'async-update-ttcn3-tags))


(provide 'init-ttcn3)
