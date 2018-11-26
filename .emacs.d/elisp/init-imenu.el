;;----------------------------------------------------------------------------
;; imenu setting
;;----------------------------------------------------------------------------

;;; imenu
(global-set-key (kbd "C-c C-j") #'imenu)
(setq imenu-max-item-length "Unlimited")


;; reset cc-imenu-c++-generic-expression to return function together with its arg-list
;; (after-load 'cc-menus
;;   (setq cc-imenu-c++-generic-expression
;;         `(
;;           ;; Try to match ::operator definitions first. Otherwise `X::operator new ()'
;;           ;; will be incorrectly recognized as function `new ()' because the regexps
;;           ;; work by backtracking from the end of the definition.
;;           (nil
;;            ,(concat
;;              "^\\<.*"
;;              "[^" c-alnum "_:<>~]"                  ; match any non-identifier char
;;                                         ; (note: this can be `\n')
;;              "\\("
;;              "\\([" c-alnum "_:<>~]*::\\)?"      ; match an operator
;;              "operator\\>[ \t]*"
;;              "\\(()\\|[^(]*\\)"                  ; special case for `()' operator
;;              "\\)"

;;              "[ \t]*([^)]*)[ \t]*[^ \t;]"           ; followed by ws, arg list,
;;                                         ; require something other than
;;                                         ; a `;' after the (...) to
;;                                         ; avoid prototypes.  Can't
;;                                         ; catch cases with () inside
;;                                         ; the parentheses surrounding
;;                                         ; the parameters.  e.g.:
;;                                         ; `int foo(int a=bar()) {...}'
;;              ) 1)
;;           ;; Special case to match a line like `main() {}'
;;           ;; e.g. no return type, not even on the previous line.
;;           (nil
;;            ,(concat
;;              "^"
;;              "\\([" c-alpha "_][" c-alnum "_:<>~]*\\)" ; match function name
;;              "[ \t]*("                             ; see above, BUT
;;              "[ \t]*\\([^ \t(*][^)]*\\)?)"          ; the arg list must not start
;;              "[ \t]*[^ \t;(]"                       ; with an asterisk or parentheses
;;              ) 1)
;;           ;; General function name regexp
;;           (nil
;;            ,(concat
;;              "^\\<"                                 ; line MUST start with word char
;;              ;; \n added to prevent overflow in regexp matcher.
;;              ;; https://lists.gnu.org/r/emacs-pretest-bug/2007-02/msg00021.html
;;              "[^()\n]*"                             ; no parentheses before
;;              "[^" c-alnum "_:<>~]"                  ; match any non-identifier char
;;              "\\([" c-alpha "_][" c-alnum "_:<>~]*\\)" ; match function name
;;              "\\([ \t\n]\\|\\\\\n\\)*("            ; see above, BUT the arg list
;;              "\\([ \t\n]\\|\\\\\n\\)*"             ; must not start
;;              "\\([^ \t\n(*]"                       ; with an asterisk or parentheses
;;              "[^()]*\\(([^()]*)[^()]*\\)*"         ; Maybe function pointer arguments
;;              "\\)?)"
;;              "\\([ \t\n]\\|\\\\\n\\)*[^ \t\n;(]"
;;              ) 0)                                   ; CHANGED FROM 1 -> 0 TO MATCH 0-TH GROUP WHICH MEANS THE WHOLE MATCH
;;           ;; Special case for definitions using phony prototype macros like:
;;           ;; `int main _PROTO( (int argc,char *argv[]) )'.
;;           ;; This case is only included if cc-imenu-c-prototype-macro-regexp is set.
;;           ;; Only supported in c-code, so no `:<>~' chars in function name!
;;           ,@(if cc-imenu-c-prototype-macro-regexp
;;                 `((nil
;;                    ,(concat
;;                      "^\\<.*"                   ; line MUST start with word char
;;                      "[^" c-alnum "_]"          ; match any non-identifier char
;;                      "\\([" c-alpha "_][" c-alnum "_]*\\)" ; match function name
;;                      "[ \t]*"                   ; whitespace before macro name
;;                      cc-imenu-c-prototype-macro-regexp
;;                      "[ \t]*("                  ; ws followed by first paren.
;;                      "[ \t]*([^)]*)[ \t]*)[ \t]*[^ \t;]" ; see above
;;                      ) 1)))
;;           ;; Class definitions
;;           ("Class"
;;            ,(concat
;;              "^"                                  ; beginning of line is required
;;              "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a `template <...>'
;;              "\\(class\\|struct\\)[ \t]+"
;;              "\\("                                ; the string we want to get
;;              "[" c-alnum "_]+"                    ; class name
;;              "\\(<[^>]+>\\)?"                     ; possibly explicitly specialized
;;              "\\)"
;;              "\\([ \t\n]\\|\\\\\n\\)*[:{]"
;;              ) 3))
;;         cc-imenu-c-generic-expression cc-imenu-c++-generic-expression))


;; @see https://weilin2015.wordpress.com/2016/05/30/create-imenu-index-for-gtest/
(after-load 'cc-menus
  ;; imenu for gtest: TEST_F(Foo, TestToJumpTo)
  (push (list nil "TEST\\(_F\\)?.*, \\(.*\\))" 0) cc-imenu-c++-generic-expression))

(after-load 'imenu
  ;; skip default generated ("TEST" "").
  (defun xwl-skip-gtest/imenu--make-index-alist (orig-fun &rest args)
    (let ((orig-ret (apply orig-fun args)))
      (when (derived-mode-p 'c++-mode)
        (setq orig-ret (remove-if (lambda (el) (member (car el) '("TEST" "TEST_F"))) orig-ret)))
      orig-ret))
  (advice-add 'imenu--make-index-alist :around 'xwl-skip-gtest/imenu--make-index-alist))


;; (after-load 'which-func
     ;; remove arg-list for call-graph to work.
     ;; TODO: add more code in call-graph to use arg-list to make better judgement.
;;     which-func-cleanup-function)


(provide 'init-imenu)
;;; init-imenu.el ends here
