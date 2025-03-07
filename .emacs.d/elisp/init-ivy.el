;;; init-ivy.el --- Use ivy for minibuffer completion and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; make sure ivy is loaded
(use-package ivy
  :ensure t
  :config
  (require 'ivy))


(when (maybe-require-package 'counsel)
  (with-eval-after-load 'counsel
    ;; don't override pop-to-mark-command
    (define-key counsel-mode-map [remap pop-to-mark-command] nil)
    ;; enable reading with thing-at-point
    (dolist (counsel-command '(counsel-git-grep counsel-git counsel-locate))
      (advice-add counsel-command :around #'smart/interactive-dwim-at-point)))

  (setq-default counsel-mode-override-describe-bindings t)
  (add-hook 'after-init-hook 'counsel-mode)

  (when (maybe-require-package 'ag)
    (defun smart/counsel-ag (initial-input)
      "Search using `counsel-rg' from the project root for INITIAL-INPUT.
If there is no project root, search from the current directory instead.
With prefix args, read directory from minibuffer."
      (interactive (list (smart/dwim-at-point)))
      (let* ((dir
              (if current-prefix-arg
                  (read-directory-name "Execute `ag' in directory: ")
                (ag/project-root default-directory)))
             ;; eat current-prefix-arg before calling counsel-ag
             (current-prefix-arg))
        (counsel-ag initial-input dir)))))


(when (maybe-require-package 'swiper)
  (with-eval-after-load 'ivy
    (defun smart/swiper-at-point (sym)
      "Use `swiper' to search for the symbol at point."
      (interactive (list (smart/read-from-minibuffer "Search string")))
      (swiper sym))))


(when (maybe-require-package 'ivy-xref)
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))


;; @see https://github.com/abo-abo/swiper/blob/master/ivy-hydra.el
(use-package ivy-hydra
  :ensure t
  :config
  (defvar ivy-dispatching-done-columns 2
    "Number of columns to use if the hint does not fit on one line."))

(defun ivy-dispatching-done-hydra ()
  "Select one of the available actions and call `ivy-done'."
  (interactive)
  (let* ((actions (ivy-state-action ivy-last))
         (estimated-len (+ 25 (length
                               (mapconcat
                                (lambda (x) (format "[%s] %s" (nth 0 x) (nth 2 x)))
                                (cdr actions) ", "))))
         (n-columns (if (> estimated-len (window-width))
                        ivy-dispatching-done-columns
                      nil)))
    (if (null (ivy--actionp actions))
        (ivy-done)
      (funcall
       (eval
        `(defhydra ivy-read-action (:color teal :columns ,n-columns)
           "action"
           ,@(mapcar (lambda (x)
                       (list (nth 0 x)
                             `(progn
                                (ivy-set-action ',(nth 1 x))
                                (ivy-done))
                             (nth 2 x)))
                     (cdr actions))
           ("M-o" nil "back")
           ("C-g" nil)))))))
(define-key ivy-minibuffer-map (kbd "M-o") 'ivy-dispatching-done-hydra)


;;; enable git-grep to filter by files as well
;;  e.g: filter python file: <*.py>def
(with-eval-after-load 'counsel
  ;; refer to counsel-git-grep-cmd-default
  (defconst git-grep-cmd-orig "git --no-pager grep -n --no-color -I -i -e \"%s\"")

  (defun counsel-git-grep-filter (orig-fun &optional input-str)
    "Apply git-grep for a specific file type for ORIG-FUN with arg INPUT-STR."
    (let ((file-pattern nil)
          (counsel-git-grep-cmd nil))
      (when input-str
	;; check file extension
	(when (string-match "^<\\(.+\\)>\\(.+\\)" input-str)
          (setq file-pattern (match-string 1 input-str)
		input-str (match-string 2 input-str)))
	;; filter with extension
	(if (and file-pattern input-str)
            ;; with filter: git --no-pager grep -n --no-color -I -i -e "text-to-grep" -- "*.txt"
            (setq counsel-git-grep-cmd
                  (format "%s -- \"%s\"" git-grep-cmd-orig file-pattern))
          ;; no filter
          (setq counsel-git-grep-cmd git-grep-cmd-orig))
	(message "git-grep actual input-str is %s" input-str)
	(message "git-grep actual cmd is %s" counsel-git-grep-cmd)
	(apply orig-fun (list input-str)))))

  (advice-add #'counsel-git-grep-function :around #'counsel-git-grep-filter))


;;; enable ivy with preview ability
(require 'ivy-preview)


(provide 'init-ivy)
;;; init-ivy.el ends here
