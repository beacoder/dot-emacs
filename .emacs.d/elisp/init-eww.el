;;; init-eww.el --- EWW config -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Initialize EWW configurations
;;;
;;; Code:

;; @see https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-eww.el

;; eww-lnum
;; https://github.com/m00natic/eww-lnum
(require-package 'eww-lnum)


;; org-eww
;; Copy text from html page for pasting in org mode file/buffer
;; e.g. Copied HTML hyperlinks get converted to [[link][desc]] for org mode.
;; http://emacs.stackexchange.com/a/8191/115
;; (require-package 'org-eww)


;; EWW
(use-package eww
  :ensure nil
  :bind (:map eww-mode-map
         ("?" . eww-hydra/body)
         :map eww-link-keymap
         ("c" . modi/eww-copy-url-dwim)
         ("w" . modi/eww-search-words)
         :map shr-map
         ("c" . modi/eww-copy-url-dwim)
         ("w" . modi/eww-search-words)
         :map eww-checkbox-map
         ("<down-mouse-1>" . eww-toggle-checkbox)
         ;; For multi-line text boxes
         ;; S-TAB Jump to previous link on the page
         ;; C-RET Submit form data
         :map eww-textarea-map
         ("<backtab>"  . shr-previous-link)
         ("<C-return>" . eww-submit)
         ;; For single line text fields
         ;; S-TAB Jump to previous link on the page
         ;; C-RET Submit form data
         :map eww-text-map
         ("<backtab>"  . shr-previous-link)
         ("<C-return>" . eww-submit))
  :pretty-hydra
  ((:title (pretty-hydra-title "Eww" 'faicon "Eww" :face 'all-the-icons-blue)
           :color amaranth :quit-key "q")
   ("Navigate"
    ((":" eww "eww")
     ("f" eww-lnum-follow "follow")
     ("F" eww-lnum-universal "universal-follow")
     ("h" eww-list-histories "history")
     ("w" modi/eww-search-words "search-word")
     ("c" modi/eww-copy-url-dwim "copy-url")
     ("/" highlight-regexp "highlight-regexp")
     ("q" kill-this-buffer "kill-this-buffer"))))
  :init
  ;; Configuration
  (require 'eww)
  (setq ;; eww-search-prefix "https://duckduckgo.com/html/?q="
   eww-search-prefix "https://www.google.com.hk/search?q="
   eww-download-directory "~/Downloads"
   eww-form-checkbox-symbol "[ ]"
   ;; (setq eww-form-checkbox-symbol "☐") ; Unicode hex 2610
   eww-form-checkbox-selected-symbol "[X]"
   ;; (setq eww-form-checkbox-selected-symbol "☑") ; Unicode hex 2611
   ;; Improve the contract of pages like Google results
   ;; http://emacs.stackexchange.com/q/2955/115
   ;; default = 40
   shr-color-visible-luminance-min 80
   browse-url-browser-function 'eww-browse-url)
  ;; Make the binding for `revert-buffer' do `eww-reload' in eww-mode
  (define-key eww-mode-map [remap revert-buffer] #'eww-reload))


;; Auto-rename new eww buffers
;; http://ergoemacs.org/emacs/emacs_eww_web_browser.html
(defun xah-rename-eww-hook ()
  "Rename eww browser's buffer so sites open in new page."
  (rename-buffer "eww" t))
(add-hook 'eww-mode-hook #'xah-rename-eww-hook)


;; Re-write of the `eww-search-words' definition.
(defun modi/eww-search-words (search-word)
  "Search the web for the text between BEG and END.
If region is active (and not whitespace), search the web for
the text in that region.
Else if the region is not active, and the point is on a symbol,
search the web for that symbol.
Else prompt the user for a search string.
See the `eww-search-prefix' variable for the search engine used."
  (interactive (list (smart/read-from-minibuffer "Google Search Word")))
  (eww search-word))


(defun modi/eww-copy-url-dwim (&optional option)
  "Copy the URL or image under point to the kill ring.
If OPTION is \\[universal-argument], or if there is no link under
point, but there is an image under point then copy the URL of the
image under point instead.
If OPTION is \\[universal-argument] \\[universal-argument], or if
there is neither a link nor an image, the page URL will be
copied.
\(For emacs 25.x and older) If this function is called twice, try
to fetch the URL and see whether it redirects somewhere else.
\(For emacs 26.x and newer) Automatically use the fetched URL's
redirection destination if it has one."
  (interactive "P")
  (let (image-url page-url)
    (cond
     ((equal '(4) option)           ;C-u
      (setq image-url t))
     ((equal '(16) option)          ;C-u C-u
      (setq page-url t))
     (t                             ;No prefix
      ))
    (>=e "26.0"
         (let* ((pt-on-url (shr-url-at-point nil))
                (pt-on-image (shr-url-at-point :image-url)))
           (unless (or pt-on-url
                       pt-on-image)
             (setq page-url t)) ;Get page URL if point is neither on URL nor image
           (if page-url
               (message "Copied page url: %s" (eww-copy-page-url))
             (let ((current-prefix-arg image-url))
               (call-interactively #'shr-probe-and-copy-url))))
         (if page-url
             (message "Copied page url: %s" (eww-copy-page-url))
           (when (string= (shr-copy-url image-url) "No URL under point") ;No prefix or C-u
             ;; Copy page url if COMMAND or C-u COMMAND returns
             ;; "No URL under point".
             (message "Copied page url: %s" (eww-copy-page-url)))))))


(defun modi/eww-browse-url-of-file ()
  "Browse the current file using `eww'."
  (interactive)
  (call-interactively #'browse-url-of-file))


;; Auto-refreshing eww buffer whenever the html file it's showing changes
;; Doesn't work on MacOS
;; http://emacs.stackexchange.com/a/2566/115
(defvar modi/eww--file-notify-descriptors-list ()
  "List to store file-notify descriptor for all files that have an
associated auto-reloading eww buffer.")

(defun modi/advice-eww-open-file-to-auto-reload (orig-fun &rest args)
  "When `eww-open-file' is called with \\[universal-argument], open
the file in eww and also add `file-notify' watch for it so that the eww
buffer auto-reloads when the HTML file changes."
  (prog1
      (apply orig-fun args)
    (when current-prefix-arg ; C-u M-x eww-open-file
      (require 'filenotify)
      (let ((file-name (car args)))
        (file-notify-add-watch file-name
                               '(change attribute-change)
                               #'modi/file-notify-callback-eww-reload)
        ;; Show the HTML file and its rendered form in eww side-by-side
        (find-file-other-window file-name))
      ;; Redefine the `q' binding in `eww-mode-map'
      (bind-key "q" #'modi/eww-quit-and-update-fn-descriptors eww-mode-map))))
(advice-add 'eww-open-file :around #'modi/advice-eww-open-file-to-auto-reload)

(defun modi/file-notify-callback-eww-reload (event)
  "On getting triggered, switch to the eww buffer, reload and switch
back to the working buffer. Also save the `file-notify-descriptor' of the
triggering event."
  (let* ((working-buffer (buffer-name)))
    (switch-to-buffer-other-window "eww")
    (eww-reload)
    (switch-to-buffer-other-window working-buffer))
  ;; `(car event)' will return the event descriptor
  (add-to-list 'modi/eww--file-notify-descriptors-list (car event)))

(defun modi/eww-quit-and-update-fn-descriptors ()
  "When quitting `eww', first remove any saved file-notify descriptors
specific to eww, while also updating `modi/eww--file-notify-descriptors-list'."
  (interactive)
  (dotimes (index (safe-length modi/eww--file-notify-descriptors-list))
    (file-notify-rm-watch (pop modi/eww--file-notify-descriptors-list)))
  (quit-window :kill))


;;; Dependency
;;; Emacs version check
(defmacro >=e (version &rest body)
  "Emacs VERSION check wrapper around BODY.
BODY can contain both `if' block (for stuff to execute if emacs
is equal or newer than VERSION) and `else' block (for stuff to
execute if emacs is older than VERSION).
Example:
  (>=e \"25.0\"
      (defun-compatible-with-25.0)
    (defun-not-compatible-in-older-version))"
  (declare (indent 2))          ;`if'-style indentation where this macro is used
  `(if (version<= ,version emacs-version)
       ,@body))


(provide 'init-eww)
;;; init-eww.el ends here
