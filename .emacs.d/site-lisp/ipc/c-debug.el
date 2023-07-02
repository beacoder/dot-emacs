;;; c-debug.el --- Debugging helpers for XELB  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Huming Chen

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

;; This module collects functions that help in debugging.

;;; Code:

(defvar c-debug:buffer "*C-DEBUG*" "Buffer to write debug messages to.")

(defvar c-debug:backtrace-start-frame 5
  "From which frame to start collecting backtraces.")

(defvar c-debug:log-time-function #'c-debug:log-uptime
  "Function used for generating timestamps in XELB debug logs.

Here are some predefined candidates:
`c-debug:log-uptime': Display the uptime of this Emacs instance.
`c-debug:log-time': Display time of day.
`nil': Disable timestamp.")

(defun c-debug:log-uptime ()
  "Add uptime to XELB debug logs."
  (emacs-uptime "[%.2h:%.2m:%.2s] "))

(defun c-debug:log-time ()
  "Add time of day to XELB debug logs."
  (format-time-string "[%T] "))

(defun c-debug:-call-stack ()
  "Return the current call stack frames."
  (let (frames frame
        ;; No need to acount for our setq, while, let, ...
        (index c-debug:backtrace-start-frame))
    (while (setq frame (backtrace-frame index))
      (push frame frames)
      (cl-incf index))
    (cl-remove-if-not 'car frames)))

(defmacro c-debug:compile-time-function-name ()
  "Get the name of outermost definition at expansion time."
  (let* ((frame (cl-find-if
		 (lambda (frame)
		   (ignore-errors
		     (let ((clause (car (cl-third frame))))
		       (or (equal clause 'defalias)
			   (equal clause 'cl-defmethod)))))
		 (reverse (c-debug:-call-stack))))
	 (defn (cl-third frame))
	 (deftype (car defn)))
    (cl-case deftype
      ((defalias) (symbol-name (cl-cadadr defn)))
      ((cl-defmethod) (symbol-name (cadr defn)))
      (t "<unknown function>"))))

(defmacro c-debug:-with-debug-buffer (&rest forms)
  "Evaluate FORMS making sure `c-debug:buffer' is correctly updated."
  `(with-current-buffer (get-buffer-create c-debug:buffer)
     (let (windows-eob)
       ;; Note windows whose point is at EOB.
       (dolist (w (get-buffer-window-list c-debug:buffer t 'nomini))
         (when (= (window-point w) (point-max))
           (push w windows-eob)))
       (save-excursion
         (goto-char (point-max))
         ,@forms)
       ;; Restore point.
       (dolist (w windows-eob)
         (set-window-point w (point-max))))))

(defun c-debug:message (format-string &rest objects)
  "Print a message to `c-debug:buffer'.

The FORMAT-STRING argument follows the speficies how to print each of
the passed OBJECTS.  See `format' for details."
  (c-debug:-with-debug-buffer
   (insert (apply #'format format-string objects))))

(defmacro c-debug:backtrace ()
  "Print a backtrace to the `c-debug:buffer'."
  '(c-debug:-with-debug-buffer
    (let ((standard-output (get-buffer-create c-debug:buffer)))
      (backtrace))))

(defmacro c-debug:backtrace-on-error (&rest forms)
  "Evaluate FORMS.  Printing a backtrace if an error is signaled."
  `(let ((debug-on-error t)
         (debugger (lambda (&rest _) (c-debug:backtrace))))
     ,@forms))

(defun c-debug:clear ()
  "Clear the debug buffer."
  (interactive)
  (c-debug:-with-debug-buffer
   (erase-buffer)))

(defun c-debug:mark ()
  "Insert a mark in the debug buffer."
  (interactive)
  (c-debug:-with-debug-buffer
   (insert "\n")))



(provide 'c-debug)

;;; c-debug.el ends here
