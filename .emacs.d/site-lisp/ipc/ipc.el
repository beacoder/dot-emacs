;;; ipc.el --- IPC protocol Emacs Lisp Binding (IPC)  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Huming Chen

;; Author: Huming Chen <chenhuming@gmail.com>
;; Maintainer: Huming Chen <chenhuming@gmail.com>
;; URL: https://github.com/beacoder/ipc-udp
;; Version: 0.1
;; Keywords: programming, convenience
;; Created: 2023-07-01
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
;; IPC protocol implemented by elisp
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(require 'ipc-types)
(eval-when-compile (require 'cl-lib))

(defvar ipc:connection-timeout 3 "Connection timeout.")

;;;; IPC connection related

(defclass ipc:connection (c:--struct)
  ((process :initarg :process :initform nil)
   (connected :initform nil)  ;non-nil indicates connected to IPC server
   (auth-info :initarg :auth-info :initform nil)
   (socket :initarg :socket :initform nil)
   (lock :initform nil)
   (setup-data :initform nil)           ;IPC connection setup data
   (request-cache :initform [])         ;cache for outgoing requests
   (message-cache :initform [])         ;cache for incoming messages
   (event-lock :initform 0)
   (event-queue :initform nil)
   (error-plist :initform nil)
   (reply-plist :initform nil)
   (event-plist :initform nil)
   (request-sequence :initform 0)
   (last-seen-sequence :initform 0)
   (ipcid :initform 0)            ;last used IPC resource ID
   (extra-plist :initform nil)) ;for storing extra data (e.g. by extensions)
  :documentation "IPC connection.")

(defclass ipc:auth-info (c:--struct)
  ((name :initarg :name :initform "" :type string)
   (data :initarg :data :initform "" :type string))
  :documentation "IPC connection authentication info.")

(cl-defmethod ipc:-get-extra-plist ((conn ipc:connection) module prop)
  "Get the value of PROP from the extra plist for module MODULE."
  (plist-get (plist-get (slot-value conn 'extra-plist) module) prop))

(cl-defmethod ipc:-set-extra-plist ((conn ipc:connection) module prop val)
  "Set the value of PROP in the extra plist for module MODULE to VAL."
  (with-slots (extra-plist) conn
    (setf extra-plist
          (plist-put extra-plist module
                     (plist-put (plist-get extra-plist module) prop val)))))

(cl-defmethod ipc:-connect ((obj ipc:connection))
  "Connect to IPC server."
  (let* ((process (slot-value obj 'process))
         (auth-info (slot-value obj 'auth-info))
         (aname (slot-value auth-info 'name))
         (adata (slot-value auth-info 'data)))
    (set-process-plist process
                       (plist-put (process-plist process) 'connection obj))
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process #'ipc:-connection-setup-filter)
    (process-send-string                ;send setup packet
     process
     (apply #'unibyte-string
            (append                     ;convert vector to string
             (ipc:marshal
              (make-instance 'ipc:SetupRequest
                             :byte-order (if ipc:lsb #x6c #x42)
                             :protocol-major-version 11
                             :protocol-minor-version 0
                             :authorization-protocol-name-len (length aname)
                             :authorization-protocol-data-len (length adata)
                             :authorization-protocol-name aname
                             :authorization-protocol-data adata))
             nil)))
    ;; Wait for setup data ready
    (with-timeout (ipc:connection-timeout (ipc:disconnect obj)
                               (error "[IPC] Connection timeout"))
      (while (not (slot-value obj 'setup-data))
        (accept-process-output process 1 nil 1)))))

(defun ipc:-connection-filter (process message)
  "Filter function for an IPC connection.

Concurrency is disabled as it breaks the orders of errors, replies and events."
  (let* ((connection (plist-get (process-plist process) 'connection))
         ;; Temporarily disable GC here as typically it's about to do
         ;; lots of marshaling/unmarshaling.
         (gc-cons-threshold most-positive-fixnum)
         (cache (vconcat (slot-value connection 'message-cache) message))
         (cache-length (length cache)))
    (setf (slot-value connection 'message-cache) cache)
    (unless (slot-value connection 'lock)
      ;; Start parsing message
      (setf (slot-value connection 'lock) t)
      ;; Process error/reply/event
      (catch 'break
        (while (<= 32 (length cache))
          (pcase (aref cache 0)
            (0                          ;error
             (ipc:-log "Error received: %s" (substring cache 0 32))
             (let ((sequence (funcall (if ipc:lsb #'ipc:-unpack-u2-lsb
                                        #'ipc:-unpack-u2)
                                      cache 2))
                   (plist (slot-value connection 'error-plist))
                   struct)
               (setq sequence (ipc:-convert-sequence connection sequence))
               (when (plist-member plist sequence)
                 (setq struct (plist-get plist sequence))
                 (setf (slot-value connection 'error-plist)
                       (plist-put plist sequence
                                  (push `(,(aref cache 1) .
                                          ,(substring cache 0 32))
                                        struct))))
               (setq cache (substring cache 32))
               (setf (slot-value connection 'last-seen-sequence) sequence)))
            (1                          ;reply
             (let* ((reply-words (funcall (if ipc:lsb #'ipc:-unpack-u4-lsb
                                            #'ipc:-unpack-u4)
                                          cache 4))
                    (reply-length (+ 32 (* 4 reply-words)))
                    struct sequence plist)
               (when (< (length cache) reply-length) ;too short, do next time
                 (throw 'break nil))
               (ipc:-log "Reply received: %s" (substring cache 0 reply-length))
               (setq sequence (funcall (if ipc:lsb #'ipc:-unpack-u2-lsb
                                         #'ipc:-unpack-u2)
                                       cache 2)
                     sequence (ipc:-convert-sequence connection sequence))
               (setq plist (slot-value connection 'reply-plist))
               (setq struct (plist-get plist sequence))
               (when struct
                 (setf (slot-value connection 'reply-plist)
                       (plist-put plist sequence
                                  (if (symbolp struct)
                                      ;; Single reply or
                                      ;; first reply for multiple replies
                                      (list struct
                                            (substring cache 0 reply-length))
                                    ;; Multiple replies
                                    `(,(car struct) ,@(cdr struct)
                                      ,(substring cache 0 reply-length))))))
               (setq cache (substring cache reply-length))
               (setf (slot-value connection 'last-seen-sequence) sequence)))
            (x                          ;event
             (let (synthetic listener event-length)
               (when (/= 0 (logand x #x80)) ;synthetic event
                 (setq synthetic t
                       x (logand x #x7f))) ;low 7 bits is the event number
               (setq listener
                     (plist-get (slot-value connection 'event-plist) x))
               (pcase listener
                 (`xge
                  (setq event-length (funcall (if ipc:lsb
                                                  #'ipc:-unpack-u4-lsb
                                                #'ipc:-unpack-u4)
                                              cache 4)
                        ;; event-length indicates additional words to the
                        ;; first 32 bytes.
                        event-length (+ 32 (* 4 event-length)))
                  (when (< (length cache) event-length)
                    ;; Too short.
                    (throw 'break nil))
                  (setq listener
                        (lax-plist-get (slot-value connection 'event-plist)
                                       (vector (aref cache 1)
                                               (funcall
                                                (if ipc:lsb
                                                    #'ipc:-unpack-u2-lsb
                                                  #'ipc:-unpack-u2)
                                                cache 8)))))
                 (`xkb
                  (setq listener
                        (lax-plist-get (slot-value connection 'event-plist)
                                       (vector (aref cache 1))))))
               ;; Conventional events are 32 bytes in size.
               (unless event-length
                 (setq event-length 32))
               (when listener
                 (with-slots (event-queue) connection
                   (setf event-queue (nconc event-queue
                                            `([,listener
                                               ,(substring cache 0
                                                           event-length)
                                               ,synthetic])))))
               (ipc:-log "Event received: %s" (substring cache 0 event-length))
               (setq cache (substring cache event-length)))))))
      (setf (slot-value connection 'lock) nil))
    (unless (slot-value connection 'lock)
      (with-slots (message-cache) connection
        (let ((current-cache-length (length message-cache)))
          (setf message-cache
                (substring message-cache (- cache-length (length cache))))
          (when (/= current-cache-length cache-length)
            (ipc:-connection-filter process []))))
      (ipc:-process-events connection))))

(cl-defmethod ipc:-process-events ((conn ipc:connection))
  "Process cached events."
  (with-slots (event-lock event-queue) conn
    (unless (< 0 event-lock)
      (cl-incf event-lock)
      (unwind-protect
          (let (event data synthetic)
            (while (setq event (pop event-queue))
              (setq data (aref event 1)
                    synthetic (aref event 2))
              (dolist (listener (aref event 0))
                (unwind-protect
                    (xcb-debug:backtrace-on-error
                     (funcall listener data synthetic))))))
        (cl-decf event-lock)))))

(cl-defmethod ipc:disconnect ((obj ipc:connection))
  "Disconnect from X server."
  (when (slot-value obj 'connected)
    (ipc:flush obj)
    (delete-process (slot-value obj 'process))
    ;; Reset every slot to its default value
    (let ((slots (eieio-class-slots 'ipc:connection)))
      (dolist (slot slots)
        (setf (slot-value obj (eieio-slot-descriptor-name slot))
              (eieio-oref-default obj (eieio-slot-descriptor-name slot)))))))

;;;; Other routines

(cl-defmethod ipc:get-setup ((obj ipc:connection))
  "Get the setup info of IPC connection OBJ."
  (slot-value obj 'setup-data))

(cl-defmethod ipc:get-socket ((obj ipc:connection))
  "Get the socket of X connection OBJ."
  (slot-value obj 'socket))

(cl-defmethod ipc:get-maximum-request-length ((obj ipc:connection))
  "Get maximum request length from setup data."
  (slot-value (ipc:get-setup obj) 'maximum-request-length))

(cl-defmethod ipc:+event ((obj ipc:connection) event listener)
  "Attach function LISTENER to event EVENT.

Note that event listeners attached this way are shared with the super- and sub-
classes of EVENT (since they have the same event number)."
  (let* ((event-number (ipc:-error-or-event-class->number obj event))
         (plist (slot-value obj 'event-plist))
         key listeners)
    (when (consp event-number)
      (setq key (car event-number)
            event-number (cdr event-number)
            listeners (plist-get plist key))
      ;; Add a placeholder.
      (setf (slot-value obj 'event-plist)
            (plist-put plist key
                       (if (child-of-class-p event 'ipc:-generic-event)
                           'xge 'xkb))))
    (setq listeners (lax-plist-get plist event-number))
    (setf (slot-value obj 'event-plist)
          (lax-plist-put plist event-number (append listeners
                                                    (list listener))))))

(cl-defmethod ipc:flush ((obj ipc:connection))
  "Flush request data to X server."
  (let ((cache (slot-value obj 'request-cache)))
    (when (< 0 (length cache))
      (setf (slot-value obj 'request-cache) []) ;should be cleared ASAP
      (cl-incf (slot-value obj 'event-lock))
      (unwind-protect
          (process-send-string (slot-value obj 'process)
                               (apply #'unibyte-string (append cache nil)))
        (cl-decf (slot-value obj 'event-lock)))
      (ipc:-process-events obj))))

(cl-defmethod ipc:get-extension-data ((obj ipc:connection) namespace)
  "Fetch the extension data from X server (block until data is retrieved)."
  (let* ((plist (slot-value obj 'extension-plist))
         (data (plist-get plist namespace)))
    (if (eieio-object-p data)
        data
      (when (not data)                  ;the request has not been made
        (ipc:prefetch-extension-data obj namespace))
      (setq data (ipc:-+reply obj (plist-get (slot-value obj 'extension-plist)
                                             namespace)))
      (when (cadr data)                 ;has error
        (error "[XELB] %s" (cadr data)))
      (setq data (car data))
      (setf (slot-value obj 'extension-plist) (plist-put plist namespace data))
      ;; Cache major opcode, first event and first error if possible
      (with-slots (present major-opcode first-event first-error) data
        (when (= 1 present)
          (setf (slot-value obj 'extension-opcode-plist)
                (plist-put (slot-value obj 'extension-opcode-plist)
                           namespace major-opcode)
                (slot-value obj 'extension-first-event-alist)
                (nconc (slot-value obj 'extension-first-event-alist)
                       `((,namespace . ,first-event)))
                (slot-value obj 'extension-first-error-alist)
                (nconc (slot-value obj 'extension-first-error-alist)
                       `((,namespace . ,first-error))))))
      data)))

(cl-defmethod ipc:prefetch-extension-data ((obj ipc:connection) namespace)
  "Prefetch the extension data from X server."
  (when (not (plist-get (slot-value obj 'extension-plist) namespace))
    (let* ((extension-xname
            (symbol-value (intern-soft (concat (symbol-name namespace)
                                               ":-extension-xname"))))
           (sequence
            (ipc:-+request obj
                           (make-instance 'ipc:QueryExtension
                                          :name-len (length extension-xname)
                                          :name extension-xname))))
      (setf (slot-value obj 'extension-plist)
            (plist-put (slot-value obj 'extension-plist) namespace sequence))
      (ipc:flush obj))))

(cl-defmethod ipc:generate-id ((obj ipc:connection))
  "Generate new X ID."
  (let* ((setup (ipc:get-setup obj))
         (base (slot-value setup 'resource-id-base))
         (mask (slot-value setup 'resource-id-mask))
         (increment (logand mask (- mask)))
         (xid (+ (slot-value obj 'xid) increment)))
    (when (> xid mask)
      (error "[XELB] Unable to allocate new X resource ID"))
    (setf (slot-value obj 'xid) xid)
    (logior base xid)))
;;



(provide 'ipc)

;;; ipc.el ends here
