;;; cg-server.el --- call-graph server. -*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Huming Chen

;; Author: Huming Chen <chenhuming@gmail.com>
;; Maintainer: Huming Chen <chenhuming@gmail.com>
;; URL: https://github.com/beacoder/call-graph

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

;; call-graph server mode:
;;
;; 1.Generate GTAGS in remote.
;; 2.Start call-graph server on remote.
;; 3.Start call-graph in local and config remote address.
;; 4.call-graph client gets tag information from server.
;; 5.call-graph build call-graph with this tag information.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom cg-server-port 10000
  "Port of the call-graph server."
  :type 'integer
  :group 'call-graph)

(defcustom cg-server-address "localhost"
  "Address of the call-graph server."
  :type 'string
  :group 'call-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cg--server-clients '()
    "Alist where KEY is a client process and VALUE is the string.")

(defvar cg--server-process nil
  "The server process.")

(defvar cg--client-process nil
  "The client process.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg--server-log (string &optional client)
  "Log message STRING from CLIENT."
  (message
   (concat (current-time-string)
           (if client (format " %s:" client) " " string) "\n")))

(defun cg--server-filter (proc string)
  "Callback function for server with PROC and STRING."
  (let ((pending (assoc proc cg--server-clients))
        message
        index)
    ;;create entry if required
    (unless pending
      (setq cg--server-clients (cons (cons proc "") cg--server-clients))
      (setq pending (assoc proc cg--server-clients)))
    (setq message (concat (cdr pending) string))
    (while (setq index (string-match "\n" message))
      (setq index (1+ index))
      (process-send-string proc (substring message 0 index))
      (cg--server-log  (substring message 0 index) proc)
      (setq message (substring message index)))
    (setcdr pending message)))

(defun cg--client-filter (proc string)
  "Callback function for client with PROC and STRING."
  (let ((pending (assoc proc cg--server-clients))
        message
        index)
    ;;create entry if required
    (unless pending
      (setq cg--server-clients (cons (cons proc "") cg--server-clients))
      (setq pending (assoc proc cg--server-clients)))
    (setq message (concat (cdr pending) string))
    (while (setq index (string-match "\n" message))
      (setq index (1+ index))
      (process-send-string proc (substring message 0 index))
      (cg--server-log  (substring message 0 index) proc)
      (setq message (substring message index)))
    (setcdr pending message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun cg-server-start ()
  "Start cg-server."
  (interactive)
  (setq cg--server-process
        (make-network-process
         :name "cg-server"
         :buffer "*cg-server*"
         :host cg-server-address
         :service cg-server-port
         :type 'datagram
         :server t
         :family 'ipv4
         ;; :sentinel 'cg--server-sentinel
         :filter 'cg--server-filter))
  (cg--server-log
   (format "call-graph server started on %s and listen on %d."
           cg-server-address cg-server-port)))

(defun cg--server-stop nil
  "Stop cg-server."
  (when cg--server-process
    (delete-process cg--server-process)
    (setq cg--server-process nil)))

(defun cg--client-stop nil
  "Stop cg-client."
  (when cg--client-process
    (delete-process cg--client-process)
    (setq cg--client-process nil)))

(defun cg--client-start ()
  "Start cg-client to talk to cg-server."
  (setq cg--client-process
        (make-network-process
         :name "cg-client"
         :buffer "*cg-client*"
         :host cg-server-address
         :service cg-server-port
         :type 'datagram
         :family 'ipv4
         ;; :sentinel 'cg--client-sentinel
         :filter 'cg--client-filter))
  (cg--server-log
   (format "cilent started to talk to %s on port %d."
           cg-server-address cg-server-port))
  (accept-process-output cg--client-process 3))

(defun cg--client-find-references (command)
  "Send COMMAND to find reference in remote."
  (unless cg--client-process (cg--client-start))
  (when (and cg--client-process command)
    (process-send-string cg--client-process command)

    ;; refer to url-retrieve-synchronously
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; @see cg-test.el


(provide 'cg-server)
;;; cg-server.el ends here
