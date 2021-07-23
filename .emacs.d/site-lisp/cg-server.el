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

(defvar cg--client-process nil
  "The client process.")

(defvar cg--retrieval-text nil
  "The message from server.")

(defvar cg--retrieval-done nil
  "Indicate whether server message is received.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg--server-log (string)
  "Log message content STRING."
  (message
   (concat (format "(%s): %s \n" (current-time-string) string))))

(defun cg--server-filter (proc string)
  "Callback function for server with PROC and STRING."
  (process-send-string proc string)
  (cg--server-log (format "Received message from client: %s" proc)))

(defun cg--client-filter (proc string)
  "Callback function for client with PROC and STRING."
  (cg--server-log (format "Received message from server: %s" proc))
  ;; response is ready
  (setq cg--retrieval-done t
        cg--retrieval-text string))

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
  (cg--client-stop)
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
   (format "Cilent started to talk to %s on port %d."
           cg-server-address cg-server-port))
  (accept-process-output cg--client-process 3))

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

(defun cg--client-find-references (command &optional timeout)
  "Send COMMAND to find reference in remote and return it.
timeout in TIMEOUT sec."
  (unless cg--client-process (cg--client-start))
  (when (and cg--client-process command)
    ;; refer to url-retrieve-synchronously
    (let ((cg--retrieval-done nil)
          (cg--retrieval-text nil)
          (start-time (current-time))
          (asynch-buffer (get-buffer "*cg-client*"))
          (timeout-in-sec (or timeout 3)))
      ;; send command and wait for response
      (process-send-string cg--client-process command)
      ;; busy-waiting for response to be ready
      (let ((proc (get-buffer-process asynch-buffer)))
        ;; If the access method was synchronous, `cg--retrieval-done' should
        ;; hopefully already be set to t.  If it is nil, and `proc' is also
        ;; nil, it implies that the async process is not running in
        ;; asynch-buffer.  This happens e.g. for FTP files.  In such a case
        ;; url-file.el should probably set something like a `url-process'
        ;; buffer-local variable so we can find the exact process that we
        ;; should be waiting for.  In the mean time, we'll just wait for any
        ;; process output.
        (while (and (not cg--retrieval-done)
                    (or (not timeout-in-sec)
                        (< (float-time (time-subtract
                                        (current-time) start-time))
                           timeout-in-sec)))
          (cg--server-log "Spinning in waiting for response from remote.")
          (if (and proc (memq (process-status proc)
                              '(closed exit signal failed))
                   ;; Make sure another process hasn't been started.
                   (eq proc (or (get-buffer-process asynch-buffer) proc)))
              ;; FIXME: It's not clear whether url-retrieve's callback is
              ;; guaranteed to be called or not.  It seems that url-http
              ;; decides sometimes consciously not to call it, so it's not
              ;; clear that it's a bug, but even then we need to decide how
              ;; url-http can then warn us that the download has completed.
              ;; In the mean time, we use this here workaround.
              ;; XXX: The callback must always be called.  Any
              ;; exception is a bug that should be fixed, not worked
              ;; around.
              (progn ;; Call delete-process so we run any sentinel now.
                (delete-process proc)
                (setq cg--retrieval-done t)))
          ;; We used to use `sit-for' here, but in some cases it wouldn't
          ;; work because apparently pending keyboard input would always
          ;; interrupt it before it got a chance to handle process input.
          ;; `sleep-for' was tried but it lead to other forms of
          ;; hanging.  --Stef
          (unless (or (with-local-quit
                        (accept-process-output proc 1))
                      (null proc))
            ;; accept-process-output returned nil, maybe because the process
            ;; exited (and may have been replaced with another).  If we got
            ;; a quit, just stop.
            (when quit-flag
              (delete-process proc))
            (setq proc (and (not quit-flag)
                            (get-buffer-process asynch-buffer))))))
      ;; return the response content
      cg--retrieval-text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; @see cg-test.el


(provide 'cg-server)
;;; cg-server.el ends here
