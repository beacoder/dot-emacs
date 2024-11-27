;;; call-graph.el --- Generate call graph for c/c++ functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 by Huming Chen

;; Author: Huming Chen <chenhuming@gmail.com>
;; URL: https://github.com/beacoder/call-graph
;; Version: 1.0.3
;; Created: 2018-01-07
;; Keywords: programming, convenience
;; Package-Requires: ((emacs "26.1") (tree-mode "1.0.0") (ivy "0.10.0") (beacon "1.3.4"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate call graph for c/c++ functions.

;;; Install:

;; Put this file into load-path directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'call-graph)
;;     (global-set-key (kbd "C-c g") 'call-graph)
;;
;;; Usage:

;; "C-c g" => (call-graph) => buffer <*call-graph*> will be generated

;;; Change Log:
;;
;; 0.1.1 Remove unusable python support
;;       Remove unusable emacs lisp support
;;       Refactor code
;; 0.1.2 Automatically save call-graph--caller-cache into desktop file.
;;       Add `call-graph-add-caller' to manually add callee <- caller.
;;       Refactor code
;; 0.1.3 Set buffer unmodified.
;;       Recover position after collapsing and expanding.
;;       Highlight hotkeys in help message.
;; 1.0.0 Add Git (git grep) as search backend.
;; 1.0.1 Support only c++-mode for now.
;;       Don't kill buffers which has been visited before when closing call-graph buffer.
;; 1.0.2 Replace mapc/mapcar with cl-loop to improve performance.
;; 1.0.3 Flash visited file location with beacon.

;;; Code:

(require 'beacon)
(require 'cc-mode)
(require 'cl-lib)
(require 'desktop)
(require 'hierarchy)
(require 'ivy)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'tree-mode)
(require 'which-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup call-graph nil
  "Customization support for the `call-graph'."
  :version "0.1.0"
  :group 'applications)

(defcustom call-graph-initial-max-depth 2
  "The maximum initial depth of call graph."
  :type 'integer
  :group 'call-graph)

(defcustom call-graph-display-file-other-window t
  "Display file in another window."
  :type 'boolean
  :group 'call-graph)

(defcustom call-graph-ignore-invalid-reference nil
  "Non-nil means reference with function name but no `(...)' will be ignored."
  :type 'boolean
  :group 'call-graph)

(defcustom call-graph-display-func-args nil
  "Display function together with its args in `call-graph'."
  :type 'boolean
  :group 'call-graph)

(defcustom call-graph-search-filters '("grep -E \"\\.(cpp|cc|c):\"")
  "The filters used by `call-graph' when searching caller."
  :type 'list
  :group 'call-graph)

(defcustom call-graph-path-to-global nil
  "Directory to search Gnu global executables."
  :type '(choice (const :tag "Unset" nil) directory)
  :risky t
  :group 'call-graph)

(defcustom call-graph-search-backend nil
  "Backend for `call-graph' to search with."
  :type '(choice (const :tag "Global")
                 (const :tag "Git"))
  :risky t
  :group 'call-graph)

(defcustom call-graph-path-to-git-repo nil
  "Directory to git repo."
  :type 'string
  :group 'call-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst call-graph--pattern-replace-alist
  '(("\"[^\"]*\""   " quoted-string ") ;; get rid of quoted-string first
    ("([^()]*)"     " parens ")
    ("<[^<>]*>"     " angle-bracket ")
    ("{[^{}]*}"     " curly-bracket ")
    ("\\[[^][]*\\]" " square-bracket ")
    ("void"         ""))
  "Replace PATTERN with REPLACE for better C++ function argument parsing.")

(defconst call-graph--pattern-to-func-left-parens
  (concat
   "\\(?1:[" c-alpha "_][" c-alnum "_:<>~]*\\)" ;; match function name
   "\\([ \t\n]\\|\\\\\n\\)*(") ;; match left-parens
  "Regexp to match function til its left parens.")

(defconst call-graph--help-string
  "(+)xpand     (_)ollapse     (p)revious (n)ext          (q)uit         (?)help
(e)xpand-all (c)ollapse-all t(o)-file  (d)elete-caller (l)ocation-set (r)eset-cache"
  "Help string for `Call-Graph'.")

(defvar call-graph--caller-cache-alist nil
  "The alist form of `call-graph--caller-cache'.")

(defvar call-graph--caller-cache nil
  "The cached caller-map.")

(defvar call-graph--default-instance nil
  "Default CALL-GRAPH instance.")

(defvar call-graph--default-hierarchy nil
  "Hierarchy to display `call-graph'.")

(defvar call-graph--window-configuration nil
  "The window configuration to be restored upon closing the buffer.")

(defvar call-graph--selected-window nil
  "The currently selected window.")

(defvar call-graph--created-buffers ()
  "List of buffers created by `call-graph'.")

(defvar call-graph--previous-buffers ()
  "List of buffers before opening `call-graph'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-graph--extract-method-name (full-func)
  "Given FULL-FUNC, return a SHORT-FUNC.
e.g: class::method(arg1, arg2) => method."
  (when-let ((full-func-str (symbol-name full-func))
             (temp-split (split-string full-func-str "("))
             (short-func-with-namespace (car temp-split))
             (short-func (intern (car (last (split-string short-func-with-namespace "::"))))))
    short-func))

(defun call-graph--which-function ()
  "Return current function name and args based on point."
  (when-let ((func (which-function)))
    (catch 'found
      (dolist (alist imenu--index-alist)
        (when-let ((full-func (car alist))
                   (match? (string-match func full-func))
                   (found? (zerop match?)))
          (throw 'found full-func))))))

(defun call-graph--find-caller (reference func data-mode)
  "Given a REFERENCE of FUNC for mode DATA-MODE.
Return the caller as (caller . location).
When FUNC with args, match number of args as well."
  (when-let ((tmp-split (split-string reference ":"))
             (file-name (car tmp-split))
             (line-nb-str (cadr tmp-split))
             (line-nb (string-to-number line-nb-str))
             (is-valid-file (file-exists-p file-name))
             (is-valid-nb (integerp line-nb))
             func
             (short-func (call-graph--extract-method-name func)))
    (let ((location (concat file-name ":" line-nb-str))
          (caller nil)
          (nb-of-func-args (call-graph--number-of-args (symbol-name func)))
          (nb-of-reference-args nil)
          (short-fun-str (symbol-name short-func))
          (is-valid-reference t))
      (with-temp-buffer
        (call-graph--show-function-args)
        (insert-file-contents-literally file-name)
        (goto-char (point-min))
        (while (re-search-forward "__attribute__[ \t\n]*(([[:alpha:]]+))" nil t)
          ; imenu failed to parse function with __attribute__ ((...)) as args
          (replace-match "__attribute__" t nil))
        (goto-char (point-min))
        (forward-line (1- line-nb))
        (call-graph--setq-local-mode-hook-nil data-mode)
        (setq imenu--index-alist nil)
        (funcall data-mode)
        (setq-local which-func-cleanup-function nil)
        (which-function-mode t)
        ;; make sure reference contains a function call
        (when call-graph-ignore-invalid-reference
          (save-excursion
            (end-of-line)
            (let ((end-of-line-pos (point)))
              (beginning-of-line)
              (if (not (re-search-forward (concat short-fun-str "\\([ \t\n]\\|\\\\\n\\)*(") nil t))
                  (setq is-valid-reference nil)
                (when (> (match-beginning 0) end-of-line-pos)
                  (setq is-valid-reference nil))))))
        (when is-valid-reference
          (setq nb-of-reference-args (call-graph--scan-func-args short-fun-str))
          (if (and nb-of-func-args nb-of-reference-args)
              ;; TODO: check if func has args with default value
              ;; if not, we should use exact match here.
              (when (= nb-of-reference-args nb-of-func-args) ; check func-args matches references-args
                (setq caller (call-graph--which-function)))
            (setq caller (call-graph--which-function)))
          (unless call-graph-display-func-args
            (setq caller (call-graph--extract-namespace-and-method caller)))))
      (when caller
        (cons (intern caller) location)))))

(defun call-graph--git-find-references (func root-location)
  "Given a FUNC and ROOT-LOCATION, return all references as a list."
  (let* ((git-repo-path (string-trim-right call-graph-path-to-git-repo "[ \t\n\r/]+"))
         (command
          (format "git -C %s --no-pager grep --full-name --no-color -ne %s"
                  (shell-quote-argument git-repo-path)
                  (shell-quote-argument (symbol-name func))))
         (filter-separator " | ")
         command-filter command-output command-result)
    (when (and (> (length call-graph-search-filters) 0)
               (setq command-filter
                     (mapconcat #'identity (delq nil call-graph-search-filters) filter-separator))
               (not (string= command-filter filter-separator)))
      (setq command (concat command filter-separator command-filter)))
    (when (setq command-output
                (with-temp-message "Searching ..."
                  (shell-command-to-string command)))
      (seq-doseq (reference (split-string command-output "\n" t))
        (when-let* ((full-location (concat git-repo-path "/" reference))
                    ;; TODO: enable this when call-graph--root-location works well
                    ;; (ignore (not (string-match root-location full-location)))
                    )
          (cl-pushnew full-location command-result))))
    command-result))

(defun call-graph--global-find-references (func)
  "Given a FUNC, return all references as a list."
  (let ((command
         (format "%s -a --result=grep -r %s"
                 (call-graph--get-path-to-global)
                 (shell-quote-argument (symbol-name func))))
        (filter-separator " | ")
        command-filter command-output)
    (when (and (> (length call-graph-search-filters) 0)
               (setq command-filter
                     (mapconcat #'identity (delq nil call-graph-search-filters) filter-separator))
               (not (string= command-filter filter-separator)))
      (setq command (concat command filter-separator command-filter)))
    (when (setq command-output (shell-command-to-string command))
      (split-string command-output "\n" t))))

(defun call-graph--handle-root-function (call-graph)
  "Save location of root function in CALL-GRAPH."
  (when-let ((file-name (buffer-file-name))
             (line-nb (line-number-at-pos))
             (location (concat file-name ":" (number-to-string line-nb))))
    ;; save root function location
    (setf (map-elt (call-graph--locations call-graph) 'root-function) (list location))
    ;; TODO: this line causes void-function error, fix it later
    ;; (setf (call-graph--root-location call-graph) location)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (call-graph
               (:constructor nil)
               (:constructor call-graph--make)
               (:conc-name call-graph--))
  (callers (make-hash-table :test #'equal)) ; map func to its callers
  (locations (make-hash-table :test #'equal)) ; map func <- caller to its locations
  (data-mode 'c++-mode) ; support c++-mode for now, consider c++-ts-mode in the future
  (root-location nil)) ; root-function location

(defun call-graph-new ()
  "Create a `call-graph' and return it."
  (call-graph--make))

(defun call-graph--add-callers (call-graph func callers)
  "In CALL-GRAPH, given FUNC, add CALLERS."
  (when (and call-graph func callers)
    (let ((short-func (call-graph--extract-method-name func))) ; method only
      (unless (map-elt (call-graph--callers call-graph) short-func)
        (seq-doseq (caller callers)
          (let* ((full-caller (car caller)) ; class::method
                 (location (cdr caller)) ; location
                 (func-caller-key ; "callee <- class::caller" as key
                  (intern (concat (symbol-name short-func) " <- " (symbol-name full-caller)))))

            ;; populate caller data
            (cl-pushnew full-caller (map-elt (call-graph--callers call-graph) short-func (list)))

            ;; populate location data
            (cl-pushnew location (map-elt (call-graph--locations call-graph) func-caller-key (list))
                        :test #'equal)))))))

(defun call-graph--is-valid (call-graph)
  "Check if CALL-GRAPH is valid."
  (and call-graph
       (not (zerop (map-length (call-graph--callers call-graph))))
       (not (zerop (map-length (call-graph--locations call-graph))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; steal from ag/dwim-at-point
(defun call-graph--dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

;;; improved version, based on ag/read-from-minibuffer
(defun call-graph--read-from-minibuffer (prompt)
  "Read a value from the minibuffer with PROMPT.
If there's a string at point, use it instead of prompt."
  (let* ((suggested (call-graph--dwim-at-point))
         (final-prompt
          (if suggested (format "%s (default %s): " prompt suggested)
            (format "%s: " prompt))))
    (if (or current-prefix-arg (string= "" suggested) (not suggested))
        (read-from-minibuffer final-prompt nil nil nil nil suggested)
      suggested)))

(defun call-graph--trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, Emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string
   "\\`[ \t\n]*" ""
   (replace-regexp-in-string
    "[ \t\n]*\\'" "" string)))

(defun call-graph--extract-namespace-and-method (full-func)
  "Given FULL-FUNC, return a namespace and method.
e.g: class::method(arg1, arg2) => class::method."
  (when-let ((full-func-str full-func)
             (temp-split (split-string full-func-str "("))
             (short-func-with-namespace (car temp-split)))
    short-func-with-namespace))

(defun call-graph--get-path-to-global ()
  "Return path to program GNU GLOBAL."
  (let ((absolute-path
         (or (executable-find "global")
             (expand-file-name "global" call-graph-path-to-global))))
    (unless (file-exists-p absolute-path)
      (error "Failed to find \"GNU GLOBAL\" in path: %s" absolute-path))
    absolute-path))

(defun call-graph--show-function-args ()
  "Customize c++-generic-expression to support function args."
  (make-local-variable 'cc-imenu-c++-generic-expression)
  (make-local-variable 'cc-imenu-c-generic-expression)
  ;; enable imenu to display both function name and its arg-list
  (setf (nth 2 cc-imenu-c++-generic-expression)
        ;; General function name regexp
        `(nil
          ,(concat
            "^\\<"                                  ; line MUST start with word char
            ;; \n added to prevent overflow in regexp matcher.
            ;; https://lists.gnu.org/r/emacs-pretest-bug/2007-02/msg00021.html
            "[^()\n]*"                              ; no parentheses before
            "[^" c-alnum "_:<>~]"                   ; match any non-identifier char
            ; 2ND-GROUP MATCH FUNCTION AND ITS ARGS WHILE 1ST-GROUP MATCH FUNCTION NAME
            "\\(?2:\\(?1:[" c-alpha "_][" c-alnum "_:<>~]*\\)"
            "\\([ \t\n]\\|\\\\\n\\)*("              ; see above, BUT the arg list
            "\\([ \t\n]\\|\\\\\n\\)*"               ; must not start
            "\\([^ \t\n(*]"                         ; with an asterisk or parentheses
            "[^()]*\\(([^()]*)[^()]*\\)*"           ; Maybe function pointer arguments
            "\\)?)\\)"                              ; END OF 2ND-GROUP
            "\\([ \t\n]\\|\\\\\n\\)*[^ \t\n;(]") 2) ; USE 2ND-GROUP AS IMENU ITEM
        cc-imenu-c-generic-expression cc-imenu-c++-generic-expression))

(defun call-graph--number-of-args (func-with-args)
  "Count number of C++ function arguments of FUNC-WITH-ARGS."
  (ignore-errors
    (with-temp-buffer
      (insert func-with-args)
      (check-parens) ;; check parentheses balance
      (goto-char (point-min))
      (unless (re-search-forward call-graph--pattern-to-func-left-parens nil t)
        (error "Failed to find left-parens"))
      (delete-region (point-min) (point))
      (goto-char (point-max))
      (delete-region (search-backward ")" nil t) (point-max))
      ;; (message (buffer-string))
      (save-match-data ;; save previous match-data and restore later
        ;; Map over the elements of call-graph--pattern-replace-alist
        ;; (pattern, replace)
        (dolist (pair call-graph--pattern-replace-alist)
          (let ((pattern (car pair))
                (replace (cadr pair)))
            (goto-char (point-min))
            (while (re-search-forward pattern nil t) ;; patttern exists
              (goto-char (point-min)) ;; start from begining
              (while (re-search-forward pattern nil t) ;; start replacing
                (replace-match replace t nil))
              (goto-char (point-min))))) ;; go over and do match-replace again
        ;; all noise cleared, count number of args
        (let ((args-string (call-graph--trim-string (buffer-string))))
          (cond ((string= "" args-string) 0)
                ((not (string= "" args-string))
                 (length (split-string args-string ",")))))))))

(defun call-graph--scan-func-args (func)
  "Scan FUNC and its args from current position, and return number of args."
  (save-excursion
    (save-match-data
      (ignore-errors
        (let (func-beginning
              func-with-args-str)
          (search-forward func)
          (setq func-beginning (match-beginning 0))
          (forward-sexp)
          (setq func-with-args-str
                (buffer-substring-no-properties func-beginning (point)))
          (when func-with-args-str
            (call-graph--number-of-args func-with-args-str)))))))

(defun call-graph-get-number-of-args (&optional func-with-args)
  "Interactively get number of arguments of FUNC-WITH-ARGS."
  (interactive (list (call-graph--read-from-minibuffer "Input C++ function with args")))
  (deactivate-mark)
  (let ((nb-args (call-graph--number-of-args func-with-args)))
    (if nb-args
        (message "Number of args is: %d" nb-args)
      (message "Failed to get argument."))))

(defun call-graph--setq-local-mode-hook-nil (mode)
  "Clear mode hooks for MODE."
  (cond ((eql mode 'c++-mode)
         (setq-local c++-mode-hook nil))
        ((eql mode 'c-mode)
         (setq-local c-mode-hook nil))))

(defun call-graph--get-func-caller-location (call-graph func caller)
  "In CALL-GRAPH, given FUNC and CALLER, return the caller postion."
  (when (and call-graph func caller)
    (let ((locations (call-graph--locations call-graph))
          (func-caller-key
           (if (eq 'root-function func)
               'root-function ; special treatment for root-function
             (intern
              (concat
               (symbol-name (call-graph--extract-method-name func)) " <- " (symbol-name caller))))))
      (map-elt locations func-caller-key))))

(defun call-graph--get-buffer ()
  "Generate buffer <*call-graph*>."
  (let ((buffer-name "*call-graph*"))
    (get-buffer-create buffer-name)))

(defun call-graph--visit-function (func-location)
  "Visit function location FUNC-LOCATION."
  (when-let ((temp-split (split-string func-location ":"))
             (file-name (car temp-split))
             (line-nb-str (cadr temp-split))
             (line-nb (string-to-number line-nb-str))
             (is-valid-file (file-exists-p file-name))
             (is-valid-nb (integerp line-nb)))
    (find-file-read-only-other-window file-name)
    (with-no-warnings (goto-char (point-min))
                      (forward-line (1- line-nb))
                      (beacon-blink))
    (unless (member
             (buffer-name (window-buffer))
             (cl-loop for buffer in call-graph--previous-buffers
                      collect (buffer-name buffer)))
      (add-to-list 'call-graph--created-buffers (window-buffer)))))

(defun call-graph--widget-root ()
  "Return current tree root."
  (save-excursion
    (goto-char (point-min))
    (let ((me (tree-mode-icon-current-line)))
      (when (and (not (tree-widget-leaf-node-icon-p me))
                 (tree-widget-p (widget-get me :parent)))
        (setq me (widget-get me :parent))
        (intern (widget-get (tree-widget-node me) :tag))))))

(defun call-graph--widget-depth ()
  "Return current tree depth."
  (save-excursion
    (goto-char (point-min))
    (let ((me (tree-mode-icon-current-line))
          (depth 0))
      (if (or (tree-widget-leaf-node-icon-p me)
              (not (tree-widget-p (widget-get me :parent))))
          (message "Not a tree under point!")
        (prog1 (setq me (widget-get me :parent)
                     depth (call-graph--widget-depth-imp me))
          (message "Depth of tree is %d" depth))))))

(defun call-graph--widget-depth-imp (tree &optional depth)
  "Return `DEPTH' of `TREE'."
  (if-let ((depth (or depth 0))
           (is-valid-tree (tree-widget-p tree))
           (is-tree-open (widget-get tree :open)))
      (progn
        ;; (message "Depth of %s is %d" (widget-get (tree-widget-node tree) :tag) depth)
        (seq-max
         (seq-map (lambda (child) (call-graph--widget-depth-imp child (1+ depth)))
                  (widget-get tree :children))))
    (if is-valid-tree depth (1- depth))))

(defun call-graph--save-caller-cache ()
  "Save caller cache by saving `call-graph--caller-cache-alist' in .emacs.desktop file."
  (when call-graph--caller-cache
    (setq call-graph--caller-cache-alist
          (map-into call-graph--caller-cache 'list))))

(defun call-graph-help ()
  "Provide help for the `Call-Graph'."
  (interactive)
  (if (eq last-command 'call-graph-help)
      (describe-mode)
    (message (call-graph--colorize-message call-graph--help-string))))

(defun call-graph--colorize-message (message)
  "Colorize `MESSAGE'."
  (with-temp-buffer
    (insert message)
    (let ((end (point-max)) key)
      (goto-char (point-min))
      (while (and end (<= (point) end) (re-search-forward ")" nil 'move))
        (when (setq key (char-after (1- (1- (point)))))
          (delete-char -2)
          (insert (propertize (string key) 'face '((:foreground "#8ac6f2") bold))))
        (insert ")")))
    (buffer-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-graph--search-callers (call-graph func depth)
  "In CALL-GRAPH, given FUNC, search callers deep to level DEPTH."
  (when-let ((next-depth (and (> depth 0) (1- depth)))
             (short-func (call-graph--extract-method-name func))
             (data-mode (call-graph--data-mode call-graph)))
    (let ((caller-list (list))
          (callers (map-elt (call-graph--callers call-graph) short-func (list))))

      ;; callers not found.
      (unless callers
        (seq-doseq (reference (if (and call-graph-search-backend (equal call-graph-search-backend "Git"))
                                  (call-graph--git-find-references short-func (call-graph--root-location call-graph))
                                (call-graph--global-find-references short-func)))
          (when-let ((caller-info
                      (and reference (call-graph--find-caller reference func data-mode))))
            (message (format "Search returns: %s" (symbol-name (car caller-info))))
            (push caller-info caller-list)))
        (call-graph--add-callers call-graph func caller-list)
        (setq callers (map-elt (call-graph--callers call-graph) short-func (list))))

      ;; recursively search callers.
      (seq-doseq (caller callers)
        (call-graph--search-callers call-graph caller next-depth)))))

(defun call-graph--build-hierarchy (call-graph func depth)
  "In CALL-GRAPH, given FUNC, build hierarchy deep to level DEPTH.
CALCULATE-DEPTH is used to calculate actual depth."
  (when-let ((next-depth (and (> depth 0) (1- depth)))
             (hierarchy call-graph--default-hierarchy)
             (short-func (call-graph--extract-method-name func))
             (callers
              (or (map-elt call-graph--caller-cache func (list)) ; load callers from cache
                  (map-elt (call-graph--callers call-graph) short-func (list)))))

    ;; populate hierarchy data.
    (seq-doseq (caller callers)
      ;; avoid recursive caller
      (unless (eq caller func)
        (hierarchy-add-tree hierarchy caller (lambda (item) (when (eq item caller) func)))
        (message "Insert child %s under parent %s" (symbol-name caller) (symbol-name func))))

    ;; recursively populate callers.
    (seq-doseq (caller callers)
      (call-graph--build-hierarchy call-graph caller next-depth))))

(defun call-graph--display-hierarchy ()
  "Display `call-graph' in hierarchy."
  (let ((switch-buffer (not (eq major-mode 'call-graph-mode)))
        hierarchy-buffer)
    (setq hierarchy-buffer
          (hierarchy-tree-display
           call-graph--default-hierarchy
           (lambda (tree-item _)
             (let ((caller (symbol-name tree-item))
                   (parent (or (hierarchy-parent call-graph--default-hierarchy tree-item) 'root-function)))
               (insert (propertize caller 'caller-name tree-item 'callee-name parent 'intangible t))))
           (call-graph--get-buffer)))
    (when switch-buffer
      (switch-to-buffer-other-window hierarchy-buffer))
    (call-graph-mode)
    (call-graph-widget-expand-all)
    (set-buffer-modified-p nil)))

(defun call-graph--create (call-graph func depth)
  "Generate CALL-GRAPH for FUNC, DEPTH is the depth of caller-map."
  (when (and call-graph func depth)
    (setq call-graph--default-hierarchy (hierarchy-new))
    (call-graph--search-callers call-graph func depth)
    (call-graph--build-hierarchy call-graph func depth)
    (call-graph--display-hierarchy)
    (message "")))

(defun call-graph--initialize ()
  "Initialize data for `call-graph'."
  (when (or current-prefix-arg
            (not call-graph--default-instance)
            (not (call-graph--is-valid call-graph--default-instance)))
    (setq call-graph--default-instance (call-graph-new))) ; clear cached reference

  (when (not call-graph--caller-cache)
    (if call-graph--caller-cache-alist ; load cache from saved session
        (setq call-graph--caller-cache (map-into call-graph--caller-cache-alist 'hash-table)
              call-graph--caller-cache-alist nil)
      (setq call-graph--caller-cache (make-hash-table :test #'equal))))

  ;; (unless (eq major-mode 'call-graph-mode) ; set mode of data
  ;;   (setf (call-graph--data-mode call-graph--default-instance) major-mode))
  )

;;;###autoload
(defun call-graph (&optional func)
  "Generate `call-graph' for FUNC / func-at-point / func-in-active-rigion.
With prefix argument, discard cached data and re-generate reference data."
  (interactive (list (and (call-graph--dwim-at-point) (intern (call-graph--dwim-at-point)))))
  (deactivate-mark)
  (when func
    (call-graph--initialize)
    (let ((call-graph call-graph--default-instance))
      (setq call-graph--previous-buffers (buffer-list)
            call-graph--window-configuration (current-window-configuration)
            call-graph--selected-window (frame-selected-window))
      (call-graph--handle-root-function call-graph)
      (save-mark-and-excursion
        (call-graph--create call-graph func call-graph-initial-max-depth)))))

;;;###autoload
(defun call-graph-add-caller (&optional func)
  "Manually add FUNC at point into `Call-Graph' internal data structure.
This works as a supplement, as `Global' sometimes fail to find caller."
  (interactive (list (call-graph--dwim-at-point)))
  (deactivate-mark)
  (when-let ((short-func (intern func))
             (full-caller (intern (which-function)))
             (file-name (buffer-file-name))
             (line-nb-str (number-to-string (line-number-at-pos)))
             (location (concat file-name ":" line-nb-str))
             (func-caller-key ; "callee <- class::caller" as key
              (intern (concat (symbol-name short-func) " <- " (symbol-name full-caller)))))
    (call-graph--initialize)
    (let ((call-graph call-graph--default-instance))
      ;; populate full-caller data
      (cl-pushnew full-caller (map-elt (call-graph--callers call-graph) short-func (list)))
      ;; populate location data
      (cl-pushnew location (map-elt (call-graph--locations call-graph) func-caller-key (list))
                  :test #'equal)
      ;; todo: save newly added mapping into cache
      (call-graph--save-caller-cache)
      (message (format "Successfully added %s." (symbol-name func-caller-key))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call-Graph Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-graph-visit-file-at-point ()
  "Visit occurrence on the current line."
  (when-let ((call-graph call-graph--default-instance)
             (callee (get-text-property (point) 'callee-name))
             (caller (get-text-property (point) 'caller-name))
             (locations (call-graph--get-func-caller-location call-graph callee caller))
             (location (car locations)))
    (call-graph--visit-function location)
    (when (> (seq-length locations) 1)
      (message "Multiple locations for this function, select with `call-graph-select-caller-location'"))))

(defun call-graph--forward-to-text ()
  "Forward to text with callee-name."
  (let ((is-end-of-line (= (point) (line-end-position))))
    (while (not (get-text-property (point) 'callee-name))
      (if is-end-of-line (backward-char 1)
        (forward-char)))))

(defun call-graph--forward-to-button ()
  "Forward to button."
  (beginning-of-line)
  (while (not (get-char-property (point) 'button))
    (forward-char)))

(defun call-graph-goto-file-at-point ()
  "Go to the occurrence on the current line."
  (interactive)
  (call-graph--forward-to-text)
  (call-graph-visit-file-at-point)
  ;; update window and buffers
  (setq call-graph--window-configuration (current-window-configuration)
        call-graph--selected-window (frame-selected-window)
        call-graph--created-buffers (delete (window-buffer) call-graph--created-buffers))
  (add-to-list 'call-graph--previous-buffers (window-buffer)))

(defun call-graph-display-file-at-point ()
  "Display in another window the occurrence the current line describes."
  (interactive)
  (save-selected-window
    (save-excursion
      (call-graph--forward-to-text)
      (call-graph-visit-file-at-point))))

(defun call-graph-select-caller-location ()
  "Select caller location as default location for function at point."
  (interactive)
  (save-excursion
    (call-graph--forward-to-text)
    (when-let ((call-graph call-graph--default-instance)
               (callee (get-text-property (point) 'callee-name))
               (caller (get-text-property (point) 'caller-name))
               (func-caller-key
                (intern
                 (concat (symbol-name (call-graph--extract-method-name callee)) " <- " (symbol-name caller))))
               (locations (call-graph--get-func-caller-location call-graph callee caller))
               (has-many (> (seq-length locations) 1)))
      (ivy-read "Caller Locations:" locations
                :action (lambda (func-location)
                          (while (not (equal func-location (car locations)))
                            (setq locations ; put selected location upfront
                                  (nconc (cdr locations) (cons (car locations) ()))))
                          (setf (map-elt (call-graph--locations call-graph) func-caller-key) locations)
                          (call-graph--visit-function func-location))))))

(defun call-graph-remove-single-caller ()
  "Within buffer <*call-graph*>, remove single caller at point."
  (call-graph--forward-to-text)
  (when-let ((call-graph call-graph--default-instance)
             (callee (get-text-property (point) 'callee-name))
             (caller (get-text-property (point) 'caller-name))
             (short-func (call-graph--extract-method-name callee))
             (callers (map-elt (call-graph--callers call-graph) short-func (list)))
             (deep-copy-of-callers (seq-map #'identity callers))
             (filters
              (or (map-elt call-graph--caller-cache callee deep-copy-of-callers)
                  (setf (map-elt call-graph--caller-cache callee) deep-copy-of-callers))))
    (unwind-protect
        (progn
          (when call-graph-display-file-other-window
            (remove-hook 'widget-move-hook 'call-graph-display-file-at-point)) ; disable display-file temporarly
          (tree-mode-delete-match (symbol-name caller))
          (call-graph-display-file-at-point))
      (when call-graph-display-file-other-window
        (add-hook 'widget-move-hook 'call-graph-display-file-at-point))) ; restore display-file
    (setf (map-elt call-graph--caller-cache callee)
          (remove caller filters))))

(defun call-graph-remove-region-callers ()
  "Within buffer <*call-graph*>, remove callers within active region."
  (when (region-active-p)
    (deactivate-mark)
    (let* ((rbeg (region-beginning))
           (rend (region-end))
           rbeg-line rend-line line-iterator)
      (goto-char rbeg) (setq rbeg-line (line-number-at-pos))
      (goto-char rend) (setq rend-line (line-number-at-pos))
      (setq line-iterator (min rbeg-line rend-line))
      (while (<= line-iterator (max rbeg-line rend-line))
        (goto-char (point-min)) (forward-line (1- (min rbeg-line rend-line)))
        (call-graph--forward-to-button)
        (call-graph-remove-single-caller)
        (setq line-iterator (1+ line-iterator))))))

(defun call-graph-remove-caller ()
  "Within buffer <*call-graph*>, remove caller."
  (interactive)
  (if (region-active-p)
      (call-graph-remove-region-callers)
    (call-graph-remove-single-caller))
  (call-graph--save-caller-cache))

(defun call-graph-reset-caller-cache ()
  "Within buffer <*call-graph*>, reset caller cache for symbol at point.
With prefix argument, discard whole caller cache."
  (interactive)
  (when (yes-or-no-p "Reset whole caller cache ?")
    (setf call-graph--caller-cache nil
          call-graph--caller-cache-alist nil)
    (message "Reset whole caller cache done")))

(defun call-graph-quit ()
  "Quit `call-graph'."
  (interactive)
  (when (eq major-mode 'call-graph-mode)
    (setq major-mode nil)
    (kill-this-buffer)
    (set-window-configuration call-graph--window-configuration)
    (select-window call-graph--selected-window)
    (cl-loop for buffer in call-graph--created-buffers
             do (kill-buffer-if-not-modified buffer))
    (setq call-graph--created-buffers ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Widget Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-graph-widget-expand-all ()
  "Iterate all widgets in buffer and expand em."
  (interactive)
  (tree-mode-expand-level 0))

(defun call-graph-widget-collapse-all ()
  "Iterate all widgets in buffer and close em."
  (interactive)
  (goto-char (point-min))
  (tree-mode-expand-level 1))

(defun call-graph-expand (&optional level)
  "Expand `call-graph' by LEVEL."
  (interactive "p")
  (when-let ((call-graph call-graph--default-instance)
             (depth (+ (call-graph--widget-depth) level))
             (func (call-graph--widget-root)))
    (let ((origin-pos (point))
          (origin-caller-name
           (get-text-property (point) 'caller-name)))
      (unless origin-caller-name
        (beginning-of-line)
        (while (null (setq origin-caller-name
                           (get-text-property (point) 'caller-name)))
          (forward-char)))
      (call-graph--create call-graph func depth)
      (goto-char origin-pos)
      (while (null (equal (get-text-property (point) 'caller-name)
                          origin-caller-name))
        (forward-char))
      (call-graph--forward-to-button)
      (call-graph-display-file-at-point))))

(defun call-graph-collapse (&optional level)
  "Collapse `call-graph' by LEVEL."
  (interactive "p")
  (let ((level (- (call-graph--widget-depth) level))
        (origin-pos (point))
        (origin-caller-name
           (get-text-property (point) 'caller-name))
        list-of-parents parent-caller-name)
    (unless origin-caller-name
      (beginning-of-line)
      (while (null (setq origin-caller-name
                         (get-text-property (point) 'caller-name)))
        (forward-char)))
    (cl-pushnew origin-caller-name list-of-parents)
    (while (null (tree-mode-root-linep))
      (tree-mode-goto-parent 1)
      (while (null (setq parent-caller-name
                         (get-text-property (point) 'caller-name)))
        (forward-char))
      (cl-pushnew parent-caller-name list-of-parents))
    (goto-char (point-min))
    (cond
     ((> level 0)
      (tree-mode-expand-level level))
     ((<= level 0)
      (tree-mode-expand-level 1)))
    (goto-char origin-pos)
    (end-of-line)
    (while (null (member (get-text-property (point) 'caller-name)
                         list-of-parents))
      (forward-char -1))
    (call-graph--forward-to-button)
    (call-graph-display-file-at-point)
    (set-buffer-modified-p nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar call-graph-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "e") 'call-graph-widget-expand-all)
    (define-key map (kbd "c") 'call-graph-widget-collapse-all)
    (define-key map (kbd "p") 'widget-backward)
    (define-key map (kbd "n") 'widget-forward)
    (define-key map (kbd "q") 'call-graph-quit)
    (define-key map (kbd "+") 'call-graph-expand)
    (define-key map (kbd "_") 'call-graph-collapse)
    (define-key map (kbd "o") 'call-graph-goto-file-at-point)
    (define-key map (kbd "d") 'call-graph-remove-caller)
    (define-key map (kbd "l") 'call-graph-select-caller-location)
    (define-key map (kbd "r") 'call-graph-reset-caller-cache)
    (define-key map (kbd "?") 'call-graph-help)
    (define-key map (kbd "<RET>") 'call-graph-goto-file-at-point)
    map)
  "Keymap for `call-graph' major mode.")

;;;###autoload
(define-derived-mode call-graph-mode special-mode "call-graph"
  "Major mode for viewing function's `call graph'.
\\{call-graph-mode-map}"
  :group 'call-graph
  (buffer-disable-undo)
  (setq truncate-lines t
        buffer-read-only t
        show-trailing-whitespace nil)
  (setq-local line-move-visual t)
  (set (make-local-variable 'inhibit-point-motion-hooks) nil)
  (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (when call-graph-display-file-other-window
    (add-hook 'widget-move-hook 'call-graph-display-file-at-point))
  (setq desktop-globals-to-save
        (add-to-list 'desktop-globals-to-save 'call-graph--caller-cache-alist))
  (run-mode-hooks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-assert (not (call-graph--number-of-args "func")))
(cl-assert (not (call-graph--number-of-args "class::func")))
(cl-assert (= (call-graph--number-of-args "func(template<p1,p2>(a),[a,b](a,b){a,b,c;},(a,b))") 3))
(cl-assert (eq (intern "method") (call-graph--extract-method-name (intern "method"))))
(cl-assert (eq (intern "method") (call-graph--extract-method-name (intern "class::method"))))
(cl-assert (eq (intern "method") (call-graph--extract-method-name (intern "class::method(arg1,arg2)"))))
(cl-assert (eq (intern "method") (call-graph--extract-method-name (intern "class::method(class::variable1,class::variable2)"))))
(cl-assert (equal "class::method" (call-graph--extract-namespace-and-method "class::method(class::variable1,class::variable2)")))


(provide 'call-graph)
;;; call-graph.el ends here
