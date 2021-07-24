;;; cg-server.el --- call-graph server. -*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Huming Chen

;; Author: Huming Chen <chenhuming@gmail.com>
;; Maintainer: Huming Chen <chenhuming@gmail.com>
;; URL: https://github.com/beacoder/call-graph
;; Version: 0.1.0
;; Keywords: programming, convenience
;; Created: 2018-01-07
;; Package-Requires: ((emacs "25") (hierarchy "0.7.0") (tree-mode "1.0.0") (ivy "0.10.0") (anaconda-mode "0.1.13"))
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

(defvar cg--server-process nil
  "The server process.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg--server-log (string)
  "Log message content STRING."
  (message
   (concat (format "(%s): %s \n" (current-time-string) string))))

(defun cg--server-stop nil
  "Stop cg-server."
  (when cg--server-process
    (delete-process cg--server-process)
    (setq cg--server-process nil)))

(defun cg--server-filter (proc string)
  "Callback function for server with PROC and STRING."
  (cg--server-log (format "Received message from client: %s" proc))
  (if (string-match "global" string)  ;; only handle global related command
      (let ((command-output (shell-command-to-string string)))
        ;; TODO: when output is too big, could introduce status code
        ;; 200 OK  => OK
        ;; 200 TBC => to be continued
        (process-send-string proc command-output))
    ;; be nice to client
    (process-send-string proc " ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun cg-server-start ()
  "Start cg-server."
  (interactive)
  (cg--server-stop)
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
   (format "Server started on %s and listen on %d."
           cg-server-address cg-server-port)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; @see cg-test.el


(provide 'cg-server)
;;; cg-server.el ends here
