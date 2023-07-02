;;; c-types.el --- Type definitions for IPC  -*- lexical-binding: t -*-

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
;; type definitions for IPC
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(require 'c-types)

;;;; Types derived directly from `c:-struct'

(defclass c:-request (c:-struct)
  nil
  :documentation "X request type.")

(defclass c:-reply (c:-struct)
  ((~reply :initform 1 :type c:-u1))
  :documentation "X reply type.")

(defclass c:-event (c:-struct)
  ((~code :type c:-u1))
  :documentation "Event type.")
;; Implemented in 'xcb.el'
(cl-defgeneric c:-error-or-event-class->number ((obj c:connection) class))
;;
(cl-defmethod c:marshal ((obj c:-event) connection &optional sequence)
  "Return the byte-array representation of event OBJ.

This method is mainly designed for `c:SendEvent', where it's used to
generate synthetic events.  The CONNECTION argument is used to retrieve
the event number of extensions.  If SEQUENCE is non-nil, it is used as
the sequence number of the synthetic event (if the event uses sequence
number); otherwise, 0 is assumed.

This method auto-pads short results to 32 bytes."
  (let ((event-number
         (c:-error-or-event-class->number connection
                                            (eieio-object-class obj)))
        result)
    (when (consp event-number)
      (setq event-number (cdr event-number))
      (if (= 1 (length event-number))
          ;; XKB event.
          (setf (slot-value obj 'xkbType) (aref event-number 0))
        ;; Generic event.
        (setf (slot-value obj 'extensions) (aref event-number 0)
              (slot-value obj 'evtype) (aref event-number 1))))
    (when (slot-exists-p obj '~sequence)
      (setf (slot-value obj '~sequence) (or sequence 0)))
    (setq result (cl-call-next-method obj))
    (when (> 32 (length result))
      (setq result (vconcat result (make-vector (- 32 (length result)) 0))))
    result))

(defclass c:-generic-event (c:-event)
  ((~code :initform 35)
   (~extension :type c:CARD8)
   (~sequence :type c:CARD16)
   (~length :type c:CARD32)
   (~evtype :type c:CARD16))
  :documentation "Generic event type.")

(defclass c:-error (c:-struct)
  ((~error :initform 0 :type c:-u1)
   (~code :type c:-u1)
   (~sequence :type c:CARD16))
  :documentation "X error type.")

(defclass c:-union (c:-struct)
  ((~size :initarg :~size :type c:-ignore)) ;Size of the largest member.
  :documentation "Union type.")
;;
(cl-defmethod slot-unbound ((_object c:-union) _class _slot-name _fn)
  nil)
;;
(cl-defmethod c:marshal ((obj c:-union))
  "Return the byte-array representation of union OBJ.

This result is converted from the first bounded slot."
  (let ((slots (eieio-class-slots (eieio-object-class obj)))
        (size (slot-value obj '~size))
        result slot type name tmp)
    (while (and (not result) slots (> size (length result)))
      (setq slot (pop slots))
      (setq type (cl--slot-descriptor-type slot)
            name (eieio-slot-descriptor-name slot))
      (unless (or (not (slot-value obj name))
                  (eq type 'c:-ignore)
                  ;; Dealing with `c:-list' type
                  (and (eq type 'c:-list)
                       (not (slot-value obj (plist-get (slot-value obj name)
                                                       'name)))))
        (setq tmp (c:-marshal-field obj (cl--slot-descriptor-type slot)
                                      (slot-value obj name)))
        (when (> (length tmp) (length result))
          (setq result tmp))))
    (when (> size (length result))
      (setq result (vconcat result (make-vector (- size (length result)) 0))))
    result))
;;
(cl-defmethod c:unmarshal ((obj c:-union) byte-array &optional ctx
                             total-length)
  "Fill in every field in union OBJ, according to BYTE-ARRAY.

The optional argument CTX is for <paramref>."
  (unless total-length
    (setq total-length (length byte-array)))
  (let ((slots (eieio-class-slots (eieio-object-class obj)))
        slot-name tmp type)
    (dolist (slot slots)
      (setq type (cl--slot-descriptor-type slot))
      (unless (eq type 'c:-ignore)
        (setq slot-name (eieio-slot-descriptor-name slot)
              tmp (c:-unmarshal-field obj type byte-array 0
                                        (eieio-oref-default obj slot-name)
                                        ctx total-length))
        (setf (slot-value obj (eieio-slot-descriptor-name slot)) (car tmp))))
    (slot-value obj '~size)))



(provide 'ipc-types)

;;; ipc-types.el ends here
