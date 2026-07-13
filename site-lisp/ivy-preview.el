;;; ivy-preview.el --- enable ivy with preview ability -*- lexical-binding: t -*-

;; Copyright (C) 2019-2023 Huming Chen

;; Author: Huming Chen <chenhuming@gmail.com>
;; Maintainer: Huming Chen <chenhuming@gmail.com>
;; URL: https://github.com/beacoder/ivy-preview
;; Version: 0.1
;; Keywords: programming, convenience
;; Created: 2021-07-26
;; Package-Requires: ((emacs "25"))
;;
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
;;
;; ivy-preview functions similar to ivy-file-preview mode
;; but with less code and cleanup files better than ivy-file-preview
;;

;;; Code:

(require 'cl-macs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
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

(defconst ivy-preview-buffers-threshhold 10
  "Number of newly created buffers allowed before cleaning.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(with-eval-after-load 'ivy-preview
  (dolist (ivy-command '(counsel-git-grep counsel-ag))
    (advice-add ivy-command :before #'ivy-preview-setup)))


(provide 'ivy-preview)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; ivy-preview.el ends here
