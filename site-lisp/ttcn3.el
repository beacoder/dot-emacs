;;; ttcn3.el --- a major mode for editing TTCN-3 core language files

;; Copyright (C) 2000-2005 W. Martin Borgert <debacle@debian.org>

;; Author:     2000 W. Martin Borgert <debacle@debian.org>
;; Maintainer: W. Martin Borgert <debacle@debian.org>
;; Created:    2000-03-26
;; Version:    ttcn3.el 2012-12-10
;; Keywords:   TTCN, languages, ASN.1

;; Author:     1997, 2000 W. Martin Borgert <debacle@debian.org>
;; Maintainer: W. Martin Borgert <debacle@debian.org>
;; Version:    $Id: tm.el,v 1.11 2000/07/30 12:20:59 debacle Exp $
;; Keywords:   Test Manager, protocol test

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'cc-mode)			; ttcn-3-mode inherits from cc-mode

(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  (c-add-language 'ttcn-3-mode 'c-mode))

(defconst c-TTCN3-conditional-key "do\\|else\\|for\\|if\\|while")
(defconst c-TTCN3-comment-start-regexp "/\\([*][*]?\\)")
(defconst c-TTCN3-defun-prompt-regexp "\\<function\\>")
(defvar c-ttcn3-menu nil)

(defvar ttcn3-mode-abbrev-table nil
  "Abbreviation table used in TTCN-3 buffers.")
(define-abbrev-table 'ttcn3-mode-abbrev-table ())

(defvar ttcn3-mode-map ()
  "Keymap used in TTCN-3 buffers.")
(if ttcn3-mode-map
    nil
  (setq ttcn3-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for TTCN-3
  )

(defvar ttcn3-mode-syntax-table nil
  "Syntax table used in TTCN-3 buffers.")
(if ttcn3-mode-syntax-table
    ()
  (setq ttcn3-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table ttcn3-mode-syntax-table))
(modify-syntax-entry ?_ "w" ttcn3-mode-syntax-table)

(easy-menu-define c-ttcn3-menu ttcn3-mode-map "TTCN-3 Mode Commands"
		  (c-mode-menu "TTCN-3"))

(defvar ttcn3-imenu-generic-expression nil
  "Imenu generic expression for TTCN-3 mode.  See `imenu-generic-expression'.")
(setq ttcn3-imenu-generic-expression
      '(("Constants"
         "^[ \t]*\\(external[ \t]+\\)?\\<const\\>[ \t]+\\sw+[ \t]+\\(\\sw+\\)"
         2)
        ("Module Parameters"
         "^[ \t]*\\<modulepar\\>[ \t]+\\sw+[ \t]+\\(\\sw+\\)"
         1)
        ("Variables"
	 "^[ \t]*\\<var\\>[ \t]+\\(\\(record\\|set\\)[ \t]+of[ \t]+\\)?\\sw+\\sw+[ \t]+\\(\\sw+\\)"
	 3)
	("Timers"
	 "^[ \t]*\\<timer\\>[ \t]+\\(\\sw+\\)" 1)
	("Templates"
	 "^[ \t]*\\<template\\>[ \t]+\\sw+\[ \t]+\\(\\sw+\\)" 1)
	("Types"
	 "^[ \t]*\\<type\\>[ \t]+\\(\\(record\\|set\\)[ \t]+of[ \t]+\\)?\\sw+\\sw+[ \t]+\\(\\sw+\\)"
	 3)
	("Named Alts"
         "^[ \t]*\\<named alt\\>[ \t]+\\(\\sw+\\)" 1)
        ("Altsteps"
         "^[ \t]*\\<altstep\\>[ \t]+\\(\\sw+\\)" 1)
        ("Functions"
	 "^[ \t]*\\(external[ \t]+\\)?\\<function\\>[ \t]+\\(\\sw+\\)"
	 2)
	("Test Cases"
	 "^[ \t]*\\<testcase\\>[ \t]+\\(\\sw+\\)" 1)
	("Groups"
	 "^[ \t]*\\<group\\>[ \t]+\\(\\sw+\\)" 1)
	("Modules"
	 "^[ \t]*\\<module\\>[ \t]+\\(\\sw+\\)" 1)))

(defvar ttcn3-font-lock-keywords nil
  "Expressions to highlight in TTCN-3 mode.")

; Different Emacsen - different font-lock-faces!

; GNU Emacs 20.7 has: builtin comment constant function-name keyword
;   reference string type variable-name warning

; GNU Emacs 21.0 has: builtin comment constant doc function-name
;   keyword reference string type variable-name warning

; XEmacs 21.1 has: comment doc-string function-name keyword
;   preprocessor reference string type variable-name

; Therefore, some aliases:
(if (and (not (boundp 'font-lock-builtin-face))
	 (boundp 'font-lock-doc-string-face))
    (defun ttcn3-builtin-face ()
      "builtin face for XEmacs"
      font-lock-doc-string-face)
  (defun ttcn3-builtin-face ()
    "builtin face for GNU Emacs"
    font-lock-builtin-face))
(if (and (not (boundp 'font-lock-constant-face))
	 (boundp 'font-lock-preprocessor-face))
    (defun ttcn3-constant-face ()
      "constant face for XEmacs"
      font-lock-preprocessor-face)
  (defun ttcn3-constant-face ()
    "constant face for GNU Emacs"
    font-lock-constant-face))

(setq ttcn3-font-lock-keywords
      (eval-when-compile
	(list
	 ;; TTCN-3 functions, modules, and testcases
	 (list (concat
                "\\<\\(" "function" "\\|" "group" "\\|" "language"
                "\\|" "module" "\\|" "named alt" "\\|" "altstep"
                "\\|" "testcase"
                "\\)\\>" "[ \t]+\\(\\sw+\\)?")
               '(1 font-lock-keyword-face)
	       '(2 font-lock-function-name-face nil t))
	 ;; TTCN-3 keywords
	 (list
	  (concat
	   "\\<"
	   (regexp-opt
	    '("action" "activate" "all" "alt" "and" "and4b" "any"
              "call" "catch" "check" "clear" "connect" "const"
              "control" "create" "deactivate" "disconnect" "display"
              "do" "done" "else" "encode" "error" "except" "exception"
              "execute" "expand" "extension" "external" "fail" "false"
              "for" "from" "get" "getcall" "getreply" "getverdict" "goto" "if"
              "ifpresent" "import" "in" "inconc" "infinity" "inout"
              "interleave" "label" "length" "log" "map" "match"
              "message" "mixed" "mod" "modifies" "modulepar" "mtc" "none"
              "nonrecursive" "not" "not4b" "nowait" "null" "omit"
              "optional" "or" "or4b" "out" "param" "pass" "pattern"
              "procedure" "raise" "read" "receive" "rem" "repeat"
              "reply" "return" "running" "runs on" "self" "send"
              "sender" "setverdict" "signature" "start" "stop" "sut.action"
              "system" "template" "timeout" "timer" "to" "trigger"
              "true" "type" "unmap" "value" "valueof" "var"
              "verdict.get" "verdict.set" "while" "with" "xor"
              "xor4b") t) "\\>")
	  '(1 font-lock-keyword-face))
	 ;; TTCN-3 predefined (built-in) functions
	 (list
	  (concat
	   "\\<"
	   (regexp-opt
	    '("bit2hex" "bit2int" "bit2oct" "bit2str" "char2int" "float2int"
	      "hex2bit" "hex2int" "hex2oct" "hex2str" "int2bit" "int2char"
	      "int2float" "int2hex" "int2oct" "int2str" "int2unichar" "ischosen"
	      "ispresent" "lengthof" "oct2bit" "oct2hex" "oct2int"
	      "oct2str" "regexp" "rnd" "sizeof" "str2int" "str2oct"
	      "substr" "unichar2int") t) "\\>")
	  '(1 (ttcn3-builtin-face)))
	 ;; TTCN-3 types
	 (list
	  (concat
	   "\\<"
	   (regexp-opt
	    '("address" "anytype" "bitstring" "boolean" "char" "charstring"
	      "component" "enumerated" "float" "hexstring" "integer"
	      "objid" "octetstring" "port" "record" "record of" "set"
	      "set of" "union" "universal char" "universal charstring"
	      "verdicttype") t)
	      "\\>")
	  '(1 font-lock-type-face))
	 ;; user-defined types
	 (list (concat "\\<\\(type\\)\\>[ \t]+\\(\\(record\\|set\\)[ \t]+"
		       "of[ \t]+\\)?\\(\\sw+\\)[ \t]+\\(\\sw+\\)")
	       '(1 font-lock-keyword-face)
	       '(5 font-lock-type-face nil t))
	 ;; TTCN-3 constants
	 (list (concat "\\<\\(const\\)\\>"
		       "[ \t]+\\(\\sw+\\)?[ \t]+\\(\\sw+\\)?")
	       '(1 font-lock-keyword-face)
	       '(2 font-lock-type-face)
	       '(3 (ttcn3-constant-face) nil t))
	 ;; TTCN-3 templates, and variables
	 (list (concat "\\<\\(template\\|var\\)\\>[ \t]+"
		       "\\(\\(record\\|set\\)[ \t]+of[ \t]+\\)?"
		       "\\(\\sw+\\)[ \t]+\\(\\sw+\\)")
	       '(1 font-lock-keyword-face)
	       '(4 font-lock-type-face)
	       '(5 font-lock-variable-name-face nil t))
	 ;; ASN.1 keywords, not to be used as identifiers in TTCN-3
	 (list
	  (concat
	   "\\<"
	   (regexp-opt
	    '("ABSENT" "ABSTRACT-SYNTAX" "ALL" "APPLICATION"
	      "AUTOMATIC" "BEGIN" "BIT" "BMPSTRING" "BOOLEAN" "BY"
	      "CHARACTER" "CHOICE" "CLASS" "COMPONENT" "COMPONENTS"
	      "CONSTRAINED" "DEFAULT" "DEFINITIONS" "EMBEDDED" "END"
	      "ENUMERATED" "EXCEPT" "EXPLICIT" "EXPORTS" "EXTERNAL"
	      "FALSE" "FROM" "GeneralizedTime" "GeneralString"
	      "IA5String" "IDENTIFIER" "IMPLICIT" "IMPORTS" "INCLUDES"
	      "INSTANCE" "INTEGER" "INTERSECTION" "ISO646String" "MAX"
	      "MIN" "MINUS-INFINITY" "NULL" "NumericString" "OBJECT"
	      "ObjectDescriptor" "OCTET" "OF" "OPTIONAL" "PDV"
	      "PLUS-INFINITY" "PRESENT" "PrintableString" "PRIVATE"
	      "REAL" "SEQUENCE" "SET" "SIZE" "STRING" "SYNTAX"
	      "T61String" "TAGS" "TeletexString" "TRUE"
	      "TYPE-IDENTIFIER" "UNION" "UNIQUE" "UNIVERSAL"
	      "UniversalString" "UTCTime" "VideotexString"
	      "VisibleString" "WITH") t) "\\>")
	  '(1 font-lock-reference-face)))))

;; Handle TTCN-3 alternatives simlilar to switch/case in C
(make-variable-buffer-local 'c-switch-label-key)
(defconst c-TTCN3-alternative-key "\\(\\[.*\\]\\)")

;; Support for the TTCN3Parser and ttthreeparser
(defvar ttcn3-parse-command "TTCN3Parser"
  "The default command for \\[ttcn3-parse], e.g. TTCN3Parser or
ttthreeparser.")

(defvar ttcn3-parse-history '("TTCN3Parser" "ttthreeparser"))

(defun ttcn3-parse (command-args)
  "Run a TTCN-3 parser, with user-specified args, and collect output
in a buffer.  While the parser runs asynchronously, you can use
\\[next-error] (M-x next-error), or
\\<compilation-minor-mode-map>\\[compile-goto-error] in the parser
output buffer, to go to the lines where the parser found problems.
This command uses a special history list for its arguments, so you can
easily repeat a parse."
  (interactive
   (let ((ttcn3-parse-default
	  (or (car ttcn3-parse-history) ttcn3-parse-command)))
     (list (read-from-minibuffer
	    "Run the TTCN-3 parser (like this): "
	    (or ttcn3-parse-default ttcn3-parse-command)
	    nil nil 'ttcn3-parse-history))))
  (let* ((buf (compile-internal
	       (concat command-args " " (buffer-file-name))
	       "No more errors"
	       "TTCN-3 parse")))))

;;; Online help (taken from IDLWAVE)
(defun ttcn3-mouse-context-help (ev &optional arg)
  "Call `ttcn3-context-help' on the clicked location."
  (interactive "eP")
  (mouse-set-point ev)
  (ttcn3-context-help arg))

(define-key ttcn3-mode-map "\M-?" 'ttcn3-context-help)
(define-key ttcn3-mode-map
  (if (featurep 'xemacs) [(shift button3)] [(shift mouse-3)])
  'ttcn3-mouse-context-help)

(defvar ttcn3-help-bnf-file "file:///usr/share/ttcn-el/ttcn-bnf.html"
  "URL of TTCN-3 BNF help in HTML format.")

(defun ttcn3-context-help (&optional arg)
  "Display TTCN-3 Online Help on context.
If point is on a keyword, help for that keyword will be shown."
  (interactive "P")
  (save-excursion
    (backward-word 1)
    (let
	((link
	  (cond 
	   ((looking-at "action") "ActionKeyword")
	   ((looking-at "activate") "ActivateKeyword")
	   ((looking-at "address") "AddressKeyword")
	   ((looking-at "all") "AllKeyword")
	   ((looking-at "alt") "AltKeyword")
	   ((looking-at "altstep") "AltstepKeyword")
	   ((looking-at "any") "AnyKeyword")
	   ((looking-at "anytype") "AnyTypeKeyword")
	   ((looking-at "attrib") "AttribKeyword")
	   ((looking-at "bit2hex") "tf_bit2hex")
	   ((looking-at "bit2int") "tf_bit2int")
	   ((looking-at "bit2oct") "tf_bit2oct")
	   ((looking-at "bit2str") "tf_bit2str")
	   ((looking-at "bitstring") "BitStringKeyword")
	   ((looking-at "boolean") "BooleanKeyword")
	   ((looking-at "callop") "CallOpKeyword")
	   ((looking-at "catchop") "CatchOpKeyword")
	   ((looking-at "char") "CharKeyword")
	   ((looking-at "char2int") "tf_char2int")
	   ((looking-at "charstring") "CharStringKeyword")
	   ((looking-at "checkop") "CheckOpKeyword")
	   ((looking-at "clearop") "ClearOpKeyword")
	   ((looking-at "complement") "ComplementKeyword")
	   ((looking-at "component") "ComponentKeyword")
	   ((looking-at "connect") "ConnectKeyword")
	   ((looking-at "const") "ConstKeyword")
	   ((looking-at "control") "ControlKeyword")
	   ((looking-at "create") "CreateKeyword")
	   ((looking-at "deactivate") "DeactivateKeyword")
	   ((looking-at "default") "DefaultKeyword")
	   ((looking-at "disconnect") "DisconnectKeyword")
	   ((looking-at "display") "DisplayKeyword")
	   ((looking-at "do") "DoKeyword")
	   ((looking-at "done") "DoneKeyword")
	   ((looking-at "else") "ElseKeyword")
	   ((looking-at "encode") "EncodeKeyword")
	   ((looking-at "enum") "EnumKeyword")
	   ((looking-at "except") "ExceptKeyword")
	   ((looking-at "exception") "ExceptionKeyword")
	   ((looking-at "execute") "ExecuteKeyword")
	   ((looking-at "ext") "ExtKeyword")
	   ((looking-at "extension") "ExtensionKeyword")
	   ((looking-at "float") "FloatKeyword")
	   ((looking-at "float2int") "tf_float2int")
	   ((looking-at "for") "ForKeyword")
	   ((looking-at "from") "FromKeyword")
	   ((looking-at "function") "FunctionKeyword")
	   ((looking-at "getcallop") "GetCallOpKeyword")
	   ((looking-at "getreplyop") "GetReplyOpKeyword")
	   ((looking-at "goto") "GotoKeyword")
	   ((looking-at "group") "GroupKeyword")
	   ((looking-at "hex2bit") "tf_hex2bit")
	   ((looking-at "hex2int") "tf_hex2int")
	   ((looking-at "hex2oct") "tf_hex2oct")
	   ((looking-at "hex2str") "tf_hex2str")
	   ((looking-at "hexstring") "HexStringKeyword")
	   ((looking-at "if") "IfKeyword")
	   ((looking-at "ifpresent") "IfPresentKeyword")
	   ((looking-at "import") "ImportKeyword")
	   ((looking-at "infinity") "InfinityKeyword")
	   ((looking-at "inoutpar") "InOutParKeyword")
	   ((looking-at "inpar") "InParKeyword")
	   ((looking-at "int2bit") "tf_int2bit")
	   ((looking-at "int2char") "tf_int2char")
	   ((looking-at "int2float") "tf_int2float")
	   ((looking-at "int2hex") "tf_int2hex")
	   ((looking-at "int2oct") "tf_int2oct")
	   ((looking-at "int2str") "tf_int2str")
	   ((looking-at "int2unichar") "tf_int2unichar")
	   ((looking-at "integer") "IntegerKeyword")
	   ((looking-at "interleaved") "InterleavedKeyword")
	   ((looking-at "ischosen") "tf_ischosen")
	   ((looking-at "ispresent") "tf_ispresent")
	   ((looking-at "label") "LabelKeyword")
	   ((looking-at "language") "LanguageKeyword")
	   ((looking-at "length") "LengthKeyword")
	   ((looking-at "lengthof") "tf_lengthof")
	   ((looking-at "log") "LogKeyword")
	   ((looking-at "map") "MapKeyword")
	   ((looking-at "match") "MatchKeyword")
	   ((looking-at "message") "MessageKeyword")
	   ((looking-at "mixed") "MixedKeyword")
	   ((looking-at "modifies") "ModifiesKeyword")
	   ((looking-at "modulepar") "ModuleParKeyword")
	   ((looking-at "mtc") "MTCKeyword")
	   ((looking-at "noblock") "NoBlockKeyword")
	   ((looking-at "nowait") "NowaitKeyword")
	   ((looking-at "objectidentifier") "ObjectIdentifierKeyword")
	   ((looking-at "oct2bit") "tf_oct2bit")
	   ((looking-at "oct2hex") "tf_oct2hex")
	   ((looking-at "oct2int") "tf_oct2int")
	   ((looking-at "oct2str") "tf_oct2str")
	   ((looking-at "octetstring") "OctetStringKeyword")
	   ((looking-at "of") "OfKeyword")
	   ((looking-at "omit") "OmitKeyword")
	   ((looking-at "on") "OnKeyword")
	   ((looking-at "optional") "OptionalKeyword")
	   ((looking-at "outpar") "OutParKeyword")
	   ((looking-at "override") "OverrideKeyword")
	   ((looking-at "para") "ParaKeyword")
	   ((looking-at "pattern") "PatternKeyword")
	   ((looking-at "port") "PortKeyword")
	   ((looking-at "procedure") "ProcedureKeyword")
	   ((looking-at "raise") "RaiseKeyword")
	   ((looking-at "read") "ReadKeyword")
	   ((looking-at "receiveop") "ReceiveOpKeyword")
	   ((looking-at "record") "RecordKeyword")
	   ((looking-at "recursive") "RecursiveKeyword")
	   ((looking-at "regexp") "tf_regexp")
	   ((looking-at "reply") "ReplyKeyword")
	   ((looking-at "return") "ReturnKeyword")
	   ((looking-at "rnd") "tf_rnd")
	   ((looking-at "running") "RunningKeyword")
	   ((looking-at "runs") "RunsKeyword")
	   ((looking-at "sender") "SenderKeyword")
	   ((looking-at "sendop") "SendOpKeyword")
	   ((looking-at "set") "SetKeyword")
	   ((looking-at "setverdict") "SetVerdictKeyword")
	   ((looking-at "signature") "SignatureKeyword")
	   ((looking-at "sizeof") "tf_sizeof")
	   ((looking-at "start") "StartKeyword")
	   ((looking-at "stop") "StopKeyword")
	   ((looking-at "str2int") "tf_str2int")
	   ((looking-at "str2oct") "tf_str2oct")
	   ((looking-at "subset") "SubsetKeyword")
	   ((looking-at "substr") "tf_substr")
	   ((looking-at "superset") "SupersetKeyword")
	   ((looking-at "system") "SystemKeyword")
	   ((looking-at "template") "TemplateKeyword")
	   ((looking-at "testcase") "TestcaseKeyword")
	   ((looking-at "timeout") "TimeoutKeyword")
	   ((looking-at "timer") "TimerKeyword")
	   ((looking-at "to") "ToKeyword")
	   ((looking-at "triggerop") "TriggerOpKeyword")
	   ((looking-at "ttcn3module") "TTCN3ModuleKeyword")
	   ((looking-at "typedef") "TypeDefKeyword")
	   ((looking-at "unichar2int") "tf_unichar2int")
	   ((looking-at "union") "UnionKeyword")
	   ((looking-at "universal") "UniversalKeyword")
	   ((looking-at "unmap") "UnmapKeyword")
	   ((looking-at "value") "ValueKeyword")
	   ((looking-at "valueof") "ValueofKeyword")
	   ((looking-at "var") "VarKeyword")
	   ((looking-at "variation") "VariationKeyword")
	   ((looking-at "verdicttype") "VerdictTypeKeyword")
	   ((looking-at "while") "WhileKeyword")
	   ((looking-at "with") "WithKeyword"))))
      (message link)
      (w3m (concat ttcn3-help-bnf-file "#" link)))))

;;;###autoload
(defun ttcn3-add-extensions (ext)
  ""
  ;; support for speedbar (we want to see these files in speedbar)
  (condition-case nil
      (progn
        (require 'speedbar)
        (funcall (symbol-function 'speedbar-add-supported-extension) ext))))

(ttcn3-add-extensions ".ttcn(3)?")

;;;###autoload
(define-derived-mode ttcn-3-mode c-mode "TTCN-3"
  "Major mode for editing TTCN-3 core language.  Reference: rev. 5 of
the BNF with changes until 2001-10.
This mode is based on `CC Mode'.  Please look for further information
in the info documenation for that mode."
  (c-initialize-cc-mode)
  (set-syntax-table ttcn3-mode-syntax-table)
  (setq local-abbrev-table ttcn3-mode-abbrev-table
	abbrev-mode t)
  (use-local-map ttcn3-mode-map)
  (c-init-language-vars ttcn-3-mode)
  (c-common-init 'ttcn-3-mode)
  (setq comment-start "/* "
	comment-end   " */"
 	c-conditional-key c-TTCN3-conditional-key
 	c-comment-start-regexp c-TTCN3-comment-start-regexp
	c-method-key nil
	c-switch-label-key c-TTCN3-alternative-key
 	c-baseclass-key nil
	c-recognize-knr-p nil
	defun-prompt-regexp c-TTCN3-defun-prompt-regexp
	imenu-generic-expression ttcn3-imenu-generic-expression
	imenu-case-fold-search nil)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'statement-cont 0)
  (easy-menu-add c-ttcn3-menu)
  (set (make-local-variable 'font-lock-defaults)
       '(ttcn3-font-lock-keywords nil nil ((?_ . "w"))))
  (c-run-mode-hooks 'c-mode-common-hook 'ttcn3-mode-hook)
  (c-update-modeline))

(provide 'ttcn3)

;;; ttcn3.el ends here
