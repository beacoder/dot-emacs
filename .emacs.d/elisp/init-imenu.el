;;----------------------------------------------------------------------------
;; imenu setting
;;----------------------------------------------------------------------------

;;; imenu
(global-set-key (kbd "C-c C-j") #'imenu)
(setq imenu-max-item-length "Unlimited")


(after-load 'cc-menus
  ;; enable imenu to display both function name and its arg-list
  (setf (nth 2 cc-imenu-c++-generic-expression)
        ;; General function name regexp
        `(nil
          ,(concat
            "^\\<"                                 ; line MUST start with word char
            ;; \n added to prevent overflow in regexp matcher.
            ;; https://lists.gnu.org/r/emacs-pretest-bug/2007-02/msg00021.html
            "[^()\n]*"                             ; no parentheses before
            "[^" c-alnum "_:<>~]"                  ; match any non-identifier char
            "\\(?2:\\(?1:[" c-alpha "_][" c-alnum "_:<>~]*\\)" ; 2ND-GROUP MATCH FUNCTION AND ITS ARGS WHILE 1ST-GROUP MATCH FUNCTION NAME
            "\\([ \t\n]\\|\\\\\n\\)*("            ; see above, BUT the arg list
            "\\([ \t\n]\\|\\\\\n\\)*"             ; must not start
            "\\([^ \t\n(*]"                       ; with an asterisk or parentheses
            "[^()]*\\(([^()]*)[^()]*\\)*"         ; Maybe function pointer arguments
            "\\)?)\\)"                            ; END OF 2ND-GROUP
            "\\([ \t\n]\\|\\\\\n\\)*[^ \t\n;(]"
            ) 2)                                  ; USE 2ND-GROUP AS IMENU ITEM
        cc-imenu-c-generic-expression cc-imenu-c++-generic-expression)

  ;; @see https://weilin2015.wordpress.com/2016/05/30/create-imenu-index-for-gtest/
  ;; imenu for gtest: TEST_F(Foo, TestToJumpTo)
  (push (list nil "TEST\\(_F\\)?.*, \\(.*\\))" 0) cc-imenu-c++-generic-expression)

  (after-load 'imenu
    ;; skip default generated ("TEST" "").
    (defun xwl-skip-gtest/imenu--make-index-alist (orig-fun &rest args)
      (let ((orig-ret (apply orig-fun args)))
        (when (derived-mode-p 'c++-mode)
          (setq orig-ret (remove-if (lambda (el) (member (car el) '("TEST" "TEST_F"))) orig-ret)))
        orig-ret))
    (advice-add 'imenu--make-index-alist :around 'xwl-skip-gtest/imenu--make-index-alist)))


(after-load 'which-func
  ;; remove arg-list for call-graph to work.
  ;; TODO: add more code in call-graph to use arg-list to make better judgement.
  (setq which-func-cleanup-function
        #'(lambda (imenu-item)
            (trim-string (nth 0 (split-string imenu-item "("))))))


(provide 'init-imenu)
;;; init-imenu.el ends here
