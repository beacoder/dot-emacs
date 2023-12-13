;;; init-ivy.el --- Use ivy for minibuffer completion and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
(defvar ivy-dispatching-done-columns 2
  "Number of columns to use if the hint does not fit on one line.")

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy-preview functions similar to ivy-file-preview mode
;; but with less code and cleanup files better than ivy-file-preview
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ivy-preview-window-configuration nil
  "The window configuration to be restored upon closing the buffer.")

(defvar ivy-preview-selected-window nil
  "The currently selected window.")

(defvar ivy-preview-selected-window-position nil
  "The currently selected window position.")

(defvar ivy-preview-created-buffers ()
  "List of newly created buffers.")

(defvar ivy-preview-previous-buffers ()
  "List of buffers created before opening counsel-git-grep-advice.")

(defconst ivy-preview-buffers-threshhold 25
  "Number of newly created buffers allowed before cleaning.")

(defun ivy-preview-clean ()
  "Clean newly created buffers."
  (with-temp-message "clean ivy-preview-created-buffers ..."
    (cl-loop for buffer in ivy-preview-created-buffers
             do (kill-buffer-if-not-modified buffer))
    (setq ivy-preview-created-buffers ())))

(defun ivy-preview-setup (&rest _)
  "Setup `ivy-preview'."
  (setq ivy-preview-window-configuration (current-window-configuration)
        ivy-preview-selected-window (frame-selected-window)
        ivy-preview-selected-window-position (point)
        ivy-preview-created-buffers ()
        ivy-preview-previous-buffers (buffer-list))
  (advice-add 'ivy-set-index :after #'ivy-preview-iterate-action)
  (advice-add 'ivy--exhibit :after #'ivy-preview-iterate-action)
  (add-hook 'minibuffer-exit-hook #'ivy-preview-quit))

(defun ivy-preview-quit ()
  "Quit `ivy-preview'."
  (when-let ((configuration ivy-preview-window-configuration)
             (selected-window ivy-preview-selected-window))
    (advice-remove 'ivy-set-index #'ivy-preview-iterate-action)
    (advice-remove 'ivy--exhibit #'ivy-preview-iterate-action)
    (remove-hook 'minibuffer-exit-hook #'ivy-preview-quit)
    (set-window-configuration configuration)
    (select-window selected-window)
    (goto-char ivy-preview-selected-window-position)
    (ivy-preview-clean)))

(defun ivy-preview-iterate-action (&optional arg)
  "Preview matched occurrence, ignore ARG."
  (save-selected-window
    (ignore arg)
    (deactivate-mark)
    (when-let* ((cur-string (nth ivy--index ivy--all-candidates))
                (found (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" cur-string))
                (file-name (match-string-no-properties 1 cur-string))
                (line-nb (match-string-no-properties 2 cur-string)))
      (when (> (length ivy-preview-created-buffers) ivy-preview-buffers-threshhold)
        (ivy-preview-clean))
      (find-file-read-only-other-window file-name)
      (delete-other-windows)
      (with-no-warnings (goto-char (point-min))
                        (forward-line (1- (string-to-number line-nb)))
                        (beacon-blink))
      (unless (member
               (buffer-name (window-buffer))
               (cl-loop for buffer in ivy-preview-previous-buffers
                        collect (buffer-name buffer)))
        (add-to-list 'ivy-preview-created-buffers (window-buffer))))))

;; hook up with any ivy-command which returns filename:linenumber as entry
(dolist (ivy-command '(counsel-git-grep counsel-ag))
  (advice-add ivy-command :before #'ivy-preview-setup))


(provide 'init-ivy)
;;; init-ivy.el ends here
