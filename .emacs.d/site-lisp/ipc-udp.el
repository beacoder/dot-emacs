;;; ipc-udp.el --- ipc based on udp. -*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Huming Chen

;; Author: Huming Chen <chenhuming@gmail.com>
;; Maintainer: Huming Chen <chenhuming@gmail.com>
;; URL: https://github.com/beacoder/ipc-udp
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
;; ipc based on udp: used to implement private protocol upon udp stack.
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ipc-udp--server-address "localhost"
  "Address of the call-graph server.")

(defvar ipc-udp--server-port 10000
  "Port of the call-graph server.")

(defvar ipc-udp--server-process nil
  "The server process.")

(defvar ipc-udp--client-process nil
  "The client process.")

(defvar ipc-udp--retrieval-text nil
  "The message from server.")

(defvar ipc-udp--retrieval-done nil
  "Indicate whether server message is received.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; steal from ag/dwim-at-point
(defun ipc-udp--dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

;;; improved version, based on ag/read-from-minibuffer
(defun ipc-udp--read-from-minibuffer (prompt)
  "Read a value from the minibuffer with PROMPT.
If there's a string at point, use it instead of prompt."
  (let* ((suggested (ipc-udp--dwim-at-point))
         (final-prompt
          (if suggested (format "%s (default %s): " prompt suggested)
            (format "%s: " prompt))))
    (if (or current-prefix-arg (string= "" suggested) (not suggested))
        (read-from-minibuffer final-prompt nil nil nil nil suggested)
      suggested)))

(defun ipc-udp--log (string)
  "Log message content STRING."
  (message
   (concat (format "(%s): %s \n" (current-time-string) string))))

(defun ipc-udp--client-start ()
  "Start ipc-udp-client to talk to ipc-udp-server."
  (ipc-udp-client-stop)
  (setq ipc-udp--client-process
        (make-network-process
         :name "ipc-udp-client"
         :buffer "*ipc-udp-client*"
         :host ipc-udp--server-address
         :service ipc-udp--server-port
         :type 'datagram
         :family 'ipv4
         ;; :sentinel 'ipc-udp--client-sentinel
         :filter 'ipc-udp--client-filter))
  (ipc-udp--log
   (format "Cilent started to talk to %s on port %d."
           ipc-udp--server-address ipc-udp--server-port))
  (accept-process-output ipc-udp--client-process 3))

(defun ipc-udp--server-filter (proc string)
  "Callback function for server with PROC and STRING."
  (ipc-udp--log (format "%s: Received message from client." proc))
  (sleep-for 1)  ;; simulate the network latency
  (if (string-match "global" string)  ;; only handle global related command
      (let ((command-output (shell-command-to-string string)))
        ;; TODO: when output is too big, could introduce status code
        ;; 200 OK  => OK
        ;; 200 TBC => to be continued
        (process-send-string proc command-output))
    ;; be nice to client
    (process-send-string proc " ")))

(defun ipc-udp--client-filter (proc string)
  "Callback function for client with PROC and STRING."
  (ipc-udp--log (format "%s: Received message from server." proc))
  ;; response is ready
  ;; todo: when response is too big, we may need to receive for multiple times
  ;; check status code first, see if need more time to receive
  (setq ipc-udp--retrieval-done t
        ipc-udp--retrieval-text string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun ipc-udp-server-start ()
  "Start ipc-udp-server."
  (interactive)
  (ipc-udp-server-stop)
  (setq ipc-udp--server-process
        (make-network-process
         :name "ipc-udp-server"
         :buffer "*ipc-udp-server*"
         :host ipc-udp--server-address
         :service ipc-udp--server-port
         :type 'datagram
         :server t
         :family 'ipv4
         ;; :sentinel 'ipc-udp--server-sentinel
         :filter 'ipc-udp--server-filter))
  (ipc-udp--log
   (format "Server started on %s and listen on %d."
           ipc-udp--server-address ipc-udp--server-port)))

;;;###autoload
(defun ipc-udp-client-send (command &optional timeout)
  "Send COMMAND to server and get response from it synchronously.
timeout in TIMEOUT seoncds."
  (interactive (list (smart/read-from-minibuffer "Send command")))
  (unless ipc-udp--client-process (ipc-udp--client-start))
  (when (and ipc-udp--client-process command)
    ;; refer to url-retrieve-synchronously
    (let ((ipc-udp--retrieval-done nil)
          (ipc-udp--retrieval-text nil)
          (start-time (current-time))
          (asynch-buffer (get-buffer "*ipc-udp-client*"))
          (timeout-in-sec (or timeout 3)))
      (process-send-string ipc-udp--client-process command)
      (ipc-udp--log "Client: Send command and wait for response.")
      ;; busy-waiting for response to be ready
      (let ((proc (get-buffer-process asynch-buffer)))
        ;; If the access method was synchronous, `ipc-udp--retrieval-done' should
        ;; hopefully already be set to t.  If it is nil, and `proc' is also
        ;; nil, it implies that the async process is not running in
        ;; asynch-buffer.  This happens e.g. for FTP files.  In such a case
        ;; url-file.el should probably set something like a `url-process'
        ;; buffer-local variable so we can find the exact process that we
        ;; should be waiting for.  In the mean time, we'll just wait for any
        ;; process output.
        (while (and (not ipc-udp--retrieval-done)
                    (or (not timeout-in-sec)
                        (< (float-time (time-subtract
                                        (current-time) start-time))
                           timeout-in-sec)))
          (ipc-udp--log "Client: Spinning in waiting for response from remote.")
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
                (setq ipc-udp--retrieval-done t)))
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
      (ipc-udp--log "Client: Received response from remote.")
      ;; return the response content
      (if (and (stringp ipc-udp--retrieval-text)
               (not (string-empty-p (string-trim ipc-udp--retrieval-text))))
          ;; split into list of strings by '\n'
          (split-string ipc-udp--retrieval-text "\n" t)
        nil))))

(defun ipc-udp-server-stop nil
  "Stop ipc-udp-server."
  (interactive)
  (when ipc-udp--server-process
    (delete-process ipc-udp--server-process)
    (setq ipc-udp--server-process nil)
    (ipc-udp--log "Server stoped.")))

(defun ipc-udp-client-stop nil
  "Stop ipc-udp-client."
  (interactive)
  (when ipc-udp--client-process
    (delete-process ipc-udp--client-process)
    (setq ipc-udp--client-process nil)
    (ipc-udp--log "Client stoped.")))


(provide 'ipc-udp)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; ipc-udp.el ends here
