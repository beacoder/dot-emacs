;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

;;; tempo-c-cpp.el --- abbrevs for c/c++ programming
;;
;; Copyright (C) 2008  Sebastien Varrette
;;
;; Author: Sebastien Varrette <Sebastien.Varrette@uni.lu>
;; Maintainer: Sebastien Varrette <Sebastien.Varrette@uni.lu>
;; Created: 18 Jan 2008
;; Version: 0.1
;; Keywords: template, C, C++

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary
;;
;; This is a way to hook tempo into cc-mode. In fact, I merge here many ressources, including:
;; - http://www.lysator.liu.se/~davidk/elisp/tempo-examples.html
;; - http://svn.marc.abramowitz.info/homedir/dotfiles/emacs
;; - http://www.emacswiki.org/cgi-bin/wiki/TempoMode
;; etc...
;;
;; To use this file, just put a (require 'tempo-c-cpp) in your .emacs file
;;
;; Note on tempo (from EmacsWiki):
;; templates are defined through tempo-define-template. they uses (p ...) to prompt for variables
;; and (s ...) to insert them again. > indents, n inserts a newline, and r inserts the region, if active.
;;
;; To use the templates defined here:
;; - either run M-x tempo-template-c-<xx> where <xx> is the name of the template (use TAB to have the list)
;; - or start to type the corresponding abbreviation (list follows) and hit C-RET or F5
;;
;; Feel free to adapt the templates to your own programming style.
;;
;; List of abbreviations:
;;            <abbrev>                <correspondant sequence>
;; ---- Preprocessor statements ---
;;            include                 #include
;;            define                  #define
;;            ifdef                   #ifdef
;;            ifndef                  #ifndef
;; --- C statements
;;            if                      if (...) { }
;;            else                    else { ... }
;;            ifelse                  if (...) { } else { }
;;            while                   while (...) { }
;;            for                     for (...) { }
;;            ifor                    for (i = 0; i < limit; ++i) { }
;;            switch                  switch() {...}
;;            case                    case: ... break;
;;            main                    int main() { ... }
;;            malloc                  type * var = (type *) malloc(...)
;; --- C++ statements
;;            class                   class xxx { ... };
;;            getset                  accessor/mutator
;;            cfor                    for (auto it = container.begin(); it != container.end(); ++it) { }
;;            rfor                    for (auto var : range) { }

(require 'tempo)
(setq tempo-interactive t)

(defvar c-tempo-tags nil
  "Tempo tags for C mode")

(defvar c++-tempo-tags nil
  "Tempo tags for C++ mode")

(defvar c-tempo-keys-alist nil
  "")

(defun my-tempo-c-cpp-bindings ()
  ;;(local-set-key (read-kbd-macro "<f8>") 'tempo-forward-mark)
  (local-set-key (read-kbd-macro "M-<return>") 'tempo-complete-tag)
  (local-set-key (read-kbd-macro "M-RET") 'tempo-complete-tag)
  (tempo-use-tag-list 'c-tempo-tags)
  (tempo-use-tag-list 'c++-tempo-tags))

(add-hook 'c-mode-hook   '(lambda () (my-tempo-c-cpp-bindings)))
(add-hook 'c++-mode-hook '(lambda () (my-tempo-c-cpp-bindings)))

;; the following macros allow to set point using the ~ character in tempo templates

(defvar tempo-initial-pos nil
  "Initial position in template after expansion")
(defadvice tempo-insert( around tempo-insert-pos act )
  "Define initial position."
  (if (eq element '~)
      (setq tempo-initial-pos (point-marker))
    ad-do-it))
(defadvice tempo-insert-template( around tempo-insert-template-pos act )
  "Set initial position when defined. ChristophConrad"
  (setq tempo-initial-pos nil)
  ad-do-it
  (if tempo-initial-pos
      (progn
        (put template 'no-self-insert t)
        (goto-char tempo-initial-pos))
    (put template 'no-self-insert nil)))

;;; Preprocessor Templates (appended to c-tempo-tags)
(tempo-define-template "c-include"
                       '("#include \"" r ".h\"" > n
                         )
                       "include"
                       "Insert a #include statement"
                       'c-tempo-tags)

(tempo-define-template "c-define"
                       '("#define " r " " > n
                         )
                       "define"
                       "Insert a #define statement"
                       'c-tempo-tags)

(tempo-define-template "c-ifdef"
                       '("#ifdef " (p "ifdef-condition: " clause) > n> ~ n
                         "#else /* !(" (s clause) ") */" n> ~ n
                         "#endif // " (s clause) n>
                         )
                       "ifdef"
                       "Insert a #ifdef #else #endif statement"
                       'c-tempo-tags)

(tempo-define-template "c-ifndef"
                       '("#ifndef " (p "ifndef-clause: " clause) > n
                         "#define " (s clause) n> ~ n
                         "#endif // " (s clause) n>
                         )
                       "ifndef"
                       "Insert a #ifndef #define #endif statement"
                       'c-tempo-tags)

;;; C-Mode Templates
(tempo-define-template "c-if"
                       '(> "if (" ~ ")" > n
                           > "{" > n> n> "}" > n>
                           )
                       "if"
                       "Insert a C if statement"
                       'c-tempo-tags)

(tempo-define-template "c-else"
                       '(> "else"> n
                           > "{" > n> ~ n "}" > n>
                         )
                       "else"
                       "Insert a C else statement"
                       'c-tempo-tags)

(tempo-define-template "c-if-else"
                       '(> "if (" ~ ")" > n
                           > "{" > n> n> "}" > n
                           > "else" > n
                           > "{" > n> n> "}" > n>
                           )
                       "ifelse"
                       "Insert a C if else statement"
                       'c-tempo-tags)

(tempo-define-template "c-while"
                       '(> "while (" ~ ")" > n
                           > "{" > n> n> "}" > n>
                         )
                       "while"
                       "Insert a C while statement"
                       'c-tempo-tags)

(tempo-define-template "c-do-while"
                       '(> "do" > n
                           > "{" > n> ~ n> "} while ();" > n>
                           )
                       "do"
                       "Insert a C do-while statement"
                       'c-tempo-tags)

(tempo-define-template "c-for"
                       '(> "for (" ~ ")" > n
                           > "{" > n> > n> "}" > n>
                           )
                       "for"
                       "Insert a C for statement"
                       'c-tempo-tags)

(tempo-define-template "c-for-i"
                       '(> "for (size_t " (p "variable: " var) " = 0; " (s var)
                           " < "(p "upper bound: " ub)"; ++" (s var) ")" > n
                           > "{" >  n> > r n "}" > n>
                           )
                       "ifor"
                       "Insert a C for loop: for(x = 0; x < ..; ++x)"
                       'c-tempo-tags)

(tempo-define-template "c-switch"
                       '(> "switch (" (p "variable to check: " clause) ")" > n
                           > "{" >  n
                           > "case " > (p "first value: ") ": " ~ > n
                           > " break;" > n> n
                           > "default:" > n
                           > " break;" > n
                           > "}" > n>
                           )
                       "switch"
                       "Insert a C switch statement"
                       'c-tempo-tags)

(tempo-define-template "c-case"
                       '(> "case " (p "value: ") ": " ~ > n
                           > "break;" > n>
                           )
                       "case"
                       "Insert a C case statement"
                       'c-tempo-tags)

(tempo-define-template "c-main"
                       '(> "int main(int argc, char *argv[])" > n
                           > "{" > n> ~ n> n> "return 0;" > n "}" > n>
                           )
                       "main"
                       "Insert a C main statement"
                       'c-tempo-tags)

(tempo-define-template "c-function"
                       '(> (p "return type: " type) " " (p "name: " name) "(" ~ ")" >  n>
                           "{" > n> n> "}" > n>
                           )
                       "fn"
                       "Insert a function"
                       'c-tempo-tags)

(tempo-define-template "c-malloc"
                       '(>(p "type: " type) " * " (p "variable name: " var) " = (" (s type) " *) malloc(sizeof(" (s type) "));" n>
                          "if (NULL != " (s var) ")" n>
                          "{" > n> ~ n> "}" > n>
                          )
                       "malloc"
                       "Insert a C malloc statement to define and allocate a pointer"
                       'c-tempo-tags)

;;;C++-Mode Templates
;;(setq max-lisp-eval-depth 500)

(tempo-define-template "c++-class-noncopyable"
                       '(> "class " (p "class " var) n
                           > "{" > n
                           > "public:" > n
                           > (s var) "();" n
                           > "virtual ~" (s var) "();" n> n
                           > "private:" > n>
                           > (s var) "(const " (s var) " &);" n
                           > (s var) "& operator=(const " (s var) " &);" n
                           > "};" > n> n>
                           > "inline " (s var) "::" (s var) "()" n
                           > "{" > n> n> "};" > n> n
                           > "inline " (s var) "::~" (s var) "()" n
                           > "{" > n> n> "};" > n> ~
                           )
                       "nocopy"
                       "New C++ class with private copy and assign"
                       'c++-tempo-tags)

(tempo-define-template "c++-class-singleton"
                       '(> "class " (p "class " var) n
                           > "{" > n
                           > "public:" > n
                           > "static " (s var) "& instance();" > n
                           > n> "private:" > n
                           > (s var) "();" n
                           > "~" (s var) "();" n
                           > "};" > n
                           > n> "inline " (s var) "::" (s var) "()" n
                           > "{" > n> n> "};" > n> n
                           > "inline " (s var) "::~" (s var) "()" n
                           > "{" > n> n> "};" > n> n>
                           > "/*static*/ " (s var) "& " (s var) "::instance();" n
                           > "{" > n>
                           > "static " (s var) " unique;" n
                           > "return (unique);" n
                           > "};" > n> ~
                           )
                       "singleton"
                       "New singleton C++ class"
                       'c++-tempo-tags)

(tempo-define-template "c++-getset"
                       '((p "type: "     type 'noinsert)
                         (p "variable: " var  'noinsert)
                         (tempo-save-named 'virtual (if (y-or-n-p  "virtual?") "virtual " ""))
                         (tempo-save-named 'm_var (concat "_" (tempo-lookup-named 'var)))
                         (tempo-save-named 'fnBase (upcase-initials (tempo-lookup-named 'var)))
                         (s type) " " (s m_var) ";" > n>
                         (s virtual) (s type) " get" (s fnBase) "() const { return "(s m_var) "; }" > n>
                         (s virtual) "void set" (s fnBase) "(" (s type) " " (s var) ") { " (s m_var) " = " (s var) "; }" > n>
                         )
                       "getset"
                       "Insert get set methods"
                       'c++-tempo-tags)

(tempo-define-template "c++-for-container"
                       '(> "for (" (if (y-or-n-p "const_iterator? ") "const auto " "auto ")
                           (p "iterator: " iter) " = " (p "container: " container) ".begin(); " (s iter) " != " (s container) ".end(); ++" (s iter) ")" n
                           > "{" > n> r n "}" > n>
                           )
                       "cfor"
                       "Insert a C++ for loop iterating over an STL container"
                       'c++-tempo-tags)

(tempo-define-template "c++-for-range"
                       '(> "for (" (if (y-or-n-p "const? ") "const " "")
                           (if (y-or-n-p "use reference? ") "auto&" "auto")
                           " " (p "item: " it) " : " (p "range: " range) ")" n>
                           "{" > n> r n "}" > n>
                           )
                       "rfor"
                       "Insert a C++ range based for loop"
                       'c++-tempo-tags)

(tempo-define-template "c++-cout"
                       '(> "std::cout << \"" ~ "\" << std::endl;" > n>
                           )
                       "cout"
                       "cout with endl"
                       'c++-tempo-tags)

(tempo-define-template "c++-doxygen-class"
                       '(> "/** @brief " ~ n> n
                           > "@author " (getenv "USERNAME") n
                           > "*/" > n>
                           )
                       "doc"
                       "New doxygen C++ class header"
                       'c++-tempo-tags)

(tempo-define-template "c++-using"
                       '(> "using namespace " (p "name: " name) ";" > n>
                           )
                       "using"
                       "using namespace"
                       'c++-tempo-tags)

(tempo-define-template "c++-functor-class"
                       '(> "struct " (p "classname: " type) n
                           > "{" n
                           > (p "return type: " return) " operator (" ~ ")" n
                           > "{" > n> n> "}" > n> "};" > n>
                           )
                       "ft"
                       "functor class"
                       'c++-tempo-tags)

(tempo-define-template "c++-exception"
                       '(> "try" n>
                           "{" > n> ~ > n> "}" > n>
                            "catch (std::exception& e)" n>
                            "{" > n> n> "}" > n>
                           )
                       "exception"
                       "Insert C++ exception handling statement"
                       'c++-tempo-tags)

(tempo-define-template "c++-for_each"
                       '(> "std::for_each("
                           (p "container: " cont) ".begin(), "
                           (s cont) ".end(), &" ~
                           (p "method: " method) ");" >
                           )
                       "each"
                       "C++ STL for_each with method call no args)"
                       'c++-tempo-tags)

(tempo-define-template "c++-copy-iter"
                       '(> "std::copy(" (p "iterator: " iter) ".begin(), "
                           (s iter) ".end(), " ~ ");" >
                           )
                       "copy"
                       "C++ STL copy"
                       'c++-tempo-tags)

(provide 'tempo-c-cpp)
;;; tempo-c-cpp.el ends here
