;;; c-types.el --- Type definitions compatible to C/CPP  -*- lexical-binding: t -*-

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
;; type definitions compatible to c/cpp
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'cl-generic)
(require 'eieio)
(require 'c-debug)

(define-minor-mode c:debug
  "Debug-logging enabled if non-nil"
  :global t)

(defmacro c:-log (&optional format-string &rest objects)
  "Emit a message prepending the name of the function being executed.

FORMAT-STRING is a string specifying the message to output, as in
`format'.  The OBJECTS arguments specify the substitutions."
  (unless format-string (setq format-string ""))
  `(when c:debug
     (c-debug:message ,(concat "%s%s:\t" format-string "\n")
                        (if c-debug:log-time-function
                            (funcall c-debug:log-time-function)
                          "")
                        (c-debug:compile-time-function-name)
                        ,@objects)
     nil))

;;;; Utility functions

(defsubst c:-pack-u1 (value)
  "1 byte unsigned integer => byte array."
  (vector value))

(defsubst c:-pack-i1 (value)
  "1 byte signed integer => byte array."
  (c:-pack-u1 (if (>= value 0) value
                  (1+ (logand #xFF (lognot (- value)))))))

(defsubst c:-pack-u2 (value)
  "2 bytes unsigned integer => byte array (MSB first)."
  (vector (logand (lsh value -8) #xFF) (logand value #xFF)))

(defsubst c:-pack-u2-lsb (value)
  "2 bytes unsigned integer => byte array (LSB first)."
  (vector (logand value #xFF) (logand (lsh value -8) #xFF)))

(defsubst c:-pack-i2 (value)
  "2 bytes signed integer => byte array (MSB first)."
  (c:-pack-u2 (if (>= value 0) value
                  (1+ (logand #xFFFF (lognot (- value)))))))

(defsubst c:-pack-i2-lsb (value)
  "2 bytes signed integer => byte array (LSB first)."
  (c:-pack-u2-lsb (if (>= value 0) value
                      (1+ (logand #xFFFF (lognot (- value)))))))

;; Due to loss of significance of floating-point numbers, `c:-pack-u8' and
;; `c:-pack-u8-lsb' may return approximate results.
(eval-and-compile
  (if (/= 0 (lsh 1 32))
      ;; 64 bit
      (progn
        (defsubst c:-pack-u4 (value)
          "4 bytes unsigned integer => byte array (MSB first, 64-bit)."
          (vector (logand (lsh value -24) #xFF) (logand (lsh value -16) #xFF)
                  (logand (lsh value -8) #xFF) (logand value #xFF)))
        (defsubst c:-pack-u4-lsb (value)
          "4 byte unsigned integer => byte array (LSB first, 64-bit)."
          (vector (logand value #xFF)
                  (logand (lsh value -8) #xFF)
                  (logand (lsh value -16) #xFF)
                  (logand (lsh value -24) #xFF)))
        (defsubst c:-pack-u8 (value)
          "8 bytes unsigned integer => byte array (MSB first)."
          (if (integerp value)
              (vector (logand (lsh value -56) #xFF)
                      (logand (lsh value -48) #xFF)
                      (logand (lsh value -40) #xFF)
                      (logand (lsh value -32) #xFF)
                      (logand (lsh value -24) #xFF)
                      (logand (lsh value -16) #xFF)
                      (logand (lsh value -8) #xFF)
                      (logand value #xFF))
            (let* ((msdw (min 4294967295. (truncate value 4294967296.)))
                   (lsdw (min 4294967295.
                              (truncate (- value (* msdw 4294967296.0))))))
              (vector (logand (lsh msdw -24) #xFF) (logand (lsh msdw -16) #xFF)
                      (logand (lsh msdw -8) #xFF) (logand msdw #xFF)
                      (logand (lsh lsdw -24) #xFF) (logand (lsh lsdw -16) #xFF)
                      (logand (lsh lsdw -8) #xFF) (logand lsdw #xFF)))))
        (defsubst c:-pack-u8-lsb (value)
          "8 bytes unsigned integer => byte array (LSB first)."
          (if (integerp value)
              (vector (logand value #xFF)
                      (logand (lsh value -8) #xFF)
                      (logand (lsh value -16) #xFF)
                      (logand (lsh value -24) #xFF)
                      (logand (lsh value -32) #xFF)
                      (logand (lsh value -40) #xFF)
                      (logand (lsh value -48) #xFF)
                      (logand (lsh value -56) #xFF))
            (let* ((msdw (min 4294967295. (truncate value 4294967296.)))
                   (lsdw (min 4294967295.
                              (truncate (- value (* msdw 4294967296.0))))))
              (vector (logand lsdw #xFF) (logand (lsh lsdw -8) #xFF)
                      (logand (lsh lsdw -16) #xFF) (logand (lsh lsdw -24) #xFF)
                      (logand msdw #xFF)
                      (logand (lsh msdw -8) #xFF)
                      (logand (lsh msdw -16) #xFF)
                      (logand (lsh msdw -24) #xFF))))))
    ;; 32 bit (30-bit actually; large numbers are represented as float type)
    (defsubst c:-pack-u4 (value)
      "4 bytes unsigned integer => byte array (MSB first, 32-bit)."
      (if (integerp value)
          (vector (logand (lsh value -24) #xFF) (logand (lsh value -16) #xFF)
                  (logand (lsh value -8) #xFF) (logand value #xFF))
        (let* ((msw (truncate value #x10000))
               (lsw (truncate (- value (* msw 65536.0)))))
          (vector (logand (lsh msw -8) #xFF) (logand msw #xFF)
                  (logand (lsh lsw -8) #xFF) (logand lsw #xFF)))))
    (defsubst c:-pack-u4-lsb (value)
      "4 bytes unsigned integer => byte array (LSB first, 32-bit)."
      (if (integerp value)
          (vector (logand value #xFF) (logand (lsh value -8) #xFF)
                  (logand (lsh value -16) #xFF) (logand (lsh value -24) #xFF))
        (let* ((msw (truncate value #x10000))
               (lsw (truncate (- value (* msw 65536.0)))))
          (vector (logand lsw #xFF) (logand (lsh lsw -8) #xFF)
                  (logand msw #xFF) (logand (lsh msw -8) #xFF)))))
    (defsubst c:-pack-u8 (value)
      "8 bytes unsigned integer => byte array (MSB first, 32-bit)."
      (if (integerp value)
          (vector 0 0 0 0
                  (logand (lsh value -24) #xFF) (logand (lsh value -16) #xFF)
                  (logand (lsh value -8) #xFF) (logand value #xFF))
        (let* ((msw (min #xFFFF (truncate value 281474976710656.)))
               (w1 (min #xFFFF
                        (truncate (setq value
                                        (- value (* msw 281474976710656.0)))
                                  4294967296.)))
               (w2 (min #xFFFF
                        (truncate (setq value (- value (* w1 4294967296.0)))
                                  #x10000)))
               (lsw (min #xFFFF (truncate (- value (* w2 65536.0))))))
          (vector (logand (lsh msw -8) #xFF) (logand msw #xFF)
                  (logand (lsh w1 -8) #xFF) (logand w1 #xFF)
                  (logand (lsh w2 -8) #xFF) (logand w2 #xFF)
                  (logand (lsh lsw -8) #xFF) (logand lsw #xFF)))))
    (defsubst c:-pack-u8-lsb (value)
      "8 bytes unsigned integer => byte array (LSB first, 32-bit)."
      (if (integerp value)
          (vector (logand value #xFF) (logand (lsh value -8) #xFF)
                  (logand (lsh value -16) #xFF) (logand (lsh value -24) #xFF)
                  0 0 0 0)
        (let* ((msw (min #xFFFF (truncate value 281474976710656.)))
               (w1 (min #xFFFF
                        (truncate (setq value
                                        (- value (* msw 281474976710656.0)))
                                  4294967296.)))
               (w2 (min #xFFFF
                        (truncate (setq value (- value (* w1 4294967296.0)))
                                  #x10000)))
               (lsw (min #xFFFF (truncate (- value (* w2 65536.0))))))
          (vector (logand lsw #xFF) (logand (lsh lsw -8) #xFF)
                  (logand w2 #xFF) (logand (lsh w2 -8) #xFF)
                  (logand w1 #xFF) (logand (lsh w1 -8) #xFF)
                  (logand msw #xFF) (logand (lsh msw -8) #xFF)))))))

(defsubst c:-pack-i4 (value)
  "4 bytes signed integer => byte array (MSB first)."
  (c:-pack-u4 (if (>= value 0)
                    value
                  (+ value 4294967296.)))) ;treated as float for 32-bit

(defsubst c:-pack-i4-lsb (value)
  "4 bytes signed integer => byte array (LSB first)."
  (c:-pack-u4-lsb (if (>= value 0)
                        value
                      (+ value 4294967296.)))) ;treated as float for 32-bit

(defsubst c:-unpack-u1 (data offset)
  "Byte array => 1 byte unsigned integer."
  (aref data offset))

(defsubst c:-unpack-i1 (data offset)
  "Byte array => 1 byte signed integer."
  (let ((value (c:-unpack-u1 data offset)))
    (if (= 0 (logand #x80 value))
        value
      (- (logand #xFF (lognot (1- value)))))))

(defsubst c:-unpack-u2 (data offset)
  "Byte array => 2 bytes unsigned integer (MSB first)."
  (logior (lsh (aref data offset) 8) (aref data (1+ offset))))

(defsubst c:-unpack-u2-lsb (data offset)
  "Byte array => 2 bytes unsigned integer (LSB first)."
  (logior (aref data offset) (lsh (aref data (1+ offset)) 8)))

(defsubst c:-unpack-i2 (data offset)
  "Byte array => 2 bytes signed integer (MSB first)."
  (let ((value (c:-unpack-u2 data offset)))
    (if (= 0 (logand #x8000 value))
        value
      (- (logand #xFFFF (lognot (1- value)))))))

(defsubst c:-unpack-i2-lsb (data offset)
  "Byte array => 2 bytes signed integer (LSB first)."
  (let ((value (c:-unpack-u2-lsb data offset)))
    (if (= 0 (logand #x8000 value))
        value
      (- (logand #xFFFF (lognot (1- value)))))))

;; Due to loss of significance of floating-point numbers, `c:-unpack-u8' and
;; `c:-unpack-u8-lsb' may return approximate results.
(eval-and-compile
  (if (/= 0 (lsh 1 32))
      ;; 64-bit
      (progn
        (defsubst c:-unpack-u4 (data offset)
          "Byte array => 4 bytes unsigned integer (MSB first, 64-bit)."
          (logior (lsh (aref data offset) 24) (lsh (aref data (1+ offset)) 16)
                  (lsh (aref data (+ offset 2)) 8) (aref data (+ offset 3))))
        (defsubst c:-unpack-u4-lsb (data offset)
          "Byte array => 4 bytes unsigned integer (LSB first, 64-bit)."
          (logior (aref data offset) (lsh (aref data (1+ offset)) 8)
                  (lsh (aref data (+ offset 2)) 16)
                  (lsh (aref data (+ offset 3)) 24)))
        (defsubst c:-unpack-u8 (data offset)
          "Byte array => 8 bytes unsigned integer (MSB first)."
          (let ((msb (aref data offset)))
            (+ (if (> msb 31) (* msb 72057594037927936.0) (lsh msb 56))
               (logior (lsh (aref data (1+ offset)) 48)
                       (lsh (aref data (+ offset 2)) 40)
                       (lsh (aref data (+ offset 3)) 32)
                       (lsh (aref data (+ offset 4)) 24)
                       (lsh (aref data (+ offset 5)) 16)
                       (lsh (aref data (+ offset 6)) 8)
                       (aref data (+ offset 7))))))
        (defsubst c:-unpack-u8-lsb (data offset)
          "Byte array => 8 bytes unsigned integer (LSB first)."
          (let ((msb (aref data (+ offset 7))))
            (+ (if (> msb 31) (* msb 72057594037927936.0) (lsh msb 56))
               (logior (lsh (aref data (+ offset 6)) 48)
                       (lsh (aref data (+ offset 5)) 40)
                       (lsh (aref data (+ offset 4)) 32)
                       (lsh (aref data (+ offset 3)) 24)
                       (lsh (aref data (+ offset 2)) 16)
                       (lsh (aref data (1+ offset)) 8)
                       (aref data offset))))))
    ;; 32-bit (30-bit actually; large numbers are represented as float type)
    (defsubst c:-unpack-u4 (data offset)
      "Byte array => 4 bytes unsigned integer (MSB first, 32-bit)."
      (let ((msb (aref data offset)))
        (+ (if (> msb 31) (* msb 16777216.0) (lsh msb 24))
           (logior (lsh (aref data (1+ offset)) 16)
                   (lsh (aref data (+ offset 2)) 8)
                   (aref data (+ offset 3))))))
    (defsubst c:-unpack-u4-lsb (data offset)
      "Byte array => 4 bytes unsigned integer (LSB first, 32-bit)."
      (let ((msb (aref data (+ offset 3))))
        (+ (if (> msb 31) (* msb 16777216.0) (lsh msb 24))
           (logior (aref data offset)
                   (lsh (aref data (1+ offset)) 8)
                   (lsh (aref data (+ offset 2)) 16)))))
    (defsubst c:-unpack-u8 (data offset)
      "Byte array => 8 bytes unsigned integer (MSB first, 32-bit)."
      (+ (* (aref data offset) 72057594037927936.0)
         (* (aref data (1+ offset)) 281474976710656.0)
         (* (aref data (+ offset 2)) 1099511627776.0)
         (* (aref data (+ offset 3)) 4294967296.0)
         (* (aref data (+ offset 4)) 16777216.0)
         (logior (lsh (aref data (+ offset 5)) 16)
                 (lsh (aref data (+ offset 6)) 8)
                 (aref data (+ offset 7)))))
    (defsubst c:-unpack-u8-lsb (data offset)
      "Byte array => 8 bytes unsigned integer (LSB first, 32-bit)."
      (+ (* (aref data (+ offset 7)) 72057594037927936.0)
         (* (aref data (+ offset 6)) 281474976710656.0)
         (* (aref data (+ offset 5)) 1099511627776.0)
         (* (aref data (+ offset 4)) 4294967296.0)
         (* (aref data (+ offset 3)) 16777216.0)
         (logior (lsh (aref data (+ offset 2)) 16)
                 (lsh (aref data (1+ offset)) 8)
                 (aref data offset))))))

(defsubst c:-unpack-i4 (data offset)
  "Byte array => 4 bytes signed integer (MSB first)."
  (let ((value (c:-unpack-u4 data offset)))
    (if (< value 2147483648.)           ;treated as float for 32-bit
        value
      (- value 4294967296.))))          ;treated as float for 32-bit

(defsubst c:-unpack-i4-lsb (data offset)
  "Byte array => 4 bytes signed integer (LSB first)."
  (let ((value (c:-unpack-u4-lsb data offset)))
    (if (< value 2147483648.)           ;treated as float for 32-bit
        value
      (- value 4294967296.))))          ;treated as float for 32-bit

(defmacro c:-fieldref (field)
  "Evaluate a <fieldref> field."
  `(slot-value obj ,field))

(defmacro c:-paramref (field)
  "Evaluate a <paramref> field."
  `(slot-value ctx ,field))

(defsubst c:-popcount (mask)
  "Return the popcount of integer MASK."
  (apply #'+ (mapcar (lambda (i)
                       (logand (lsh mask i) 1))
                     ;; 32-bit number assumed (CARD32)
                     (eval-when-compile (number-sequence -31 0)))))

(defsubst c:-request-class->reply-class (request)
  "Return the reply class corresponding to the request class REQUEST."
  (intern-soft (concat (symbol-name request) "~reply")))

(defsubst c:-padding (N)
  "Pad N to 4 bytes."
  (% (- 4 (% N 4)) 4))

;;;; Basic types

;; typedef in C
(defmacro c:deftypealias (new-type old-type)
  "Define NEW-TYPE as an alias of type OLD-TYPE.

Also the fundamental type is stored in 'the c--typealias' variable
property (for internal use only)."
  `(progn
     ;; FIXME: `new-type' should probably just not be eval'd at all,
     ;; but that requires changing all callers not to quote their arg.
     (cl-deftype ,(eval new-type t) nil ,old-type)
     (put ,new-type 'c--typealias
          (or (get ,old-type 'c--typealias) ,old-type))))

;; 1/2/4 B signed/unsigned integer
(cl-deftype c:-i1 () t)
(cl-deftype c:-i2 () t)
(cl-deftype c:-i4 () t)
(cl-deftype c:-u1 () t)
(cl-deftype c:-u2 () t)
(cl-deftype c:-u4 () t)
;; 8 B unsigned integer
(cl-deftype c:-u8 () t)
;; <pad>
(cl-deftype c:-pad () t)
;; <pad> with align attribute
(cl-deftype c:-pad-align () t)
;; <fd>
(c:deftypealias 'c:fd 'c:-i4)
;; <list>
(cl-deftype c:-list () t)
;; <switch>
(cl-deftype c:-switch () t)
;; This type of data is not involved in marshalling/unmarshalling
(cl-deftype c:-ignore () t)
;; C types and types missing in XCB
(cl-deftype c:void () t)
(c:deftypealias 'c:char 'c:-u1)
(c:deftypealias 'c:BYTE 'c:-u1)
(c:deftypealias 'c:INT8 'c:-i1)
(c:deftypealias 'c:INT16 'c:-i2)
(c:deftypealias 'c:INT32 'c:-i4)
(c:deftypealias 'c:CARD8 'c:-u1)
(c:deftypealias 'c:CARD16 'c:-u2)
(c:deftypealias 'c:CARD32 'c:-u4)
(c:deftypealias 'c:CARD64 'c:-u8)
(c:deftypealias 'c:BOOL 'c:-u1)

;;;; Struct type

(eval-and-compile
  (defvar c:lsb t
    "Non-nil for LSB first (i.e., little-endian), nil otherwise.

Consider let-bind it rather than change its global value."))

(defclass c:--struct ()
  nil)

(cl-defmethod slot-unbound ((object c:--struct) class slot-name fn)
  (unless (eq fn #'oref-default)
    (c:-log "unbound-slot: %s" (list (eieio-class-name class)
                                       (eieio-object-name object)
                                       slot-name fn))))

(defclass c:-struct (c:--struct)
  ((~lsb :initarg :~lsb
         :initform (symbol-value 'c:lsb) ;see `eieio-default-eval-maybe'
         :type c:-ignore))
  :documentation "Struct type.")

(cl-defmethod c:marshal ((obj c:-struct))
  "Return the byte-array representation of struct OBJ."
  (let ((slots (eieio-class-slots (eieio-object-class obj)))
        result name type value)
    (catch 'break
      (dolist (slot slots)
        (setq type (cl--slot-descriptor-type slot))
        (unless (eq type 'c:-ignore)
          (setq name (eieio-slot-descriptor-name slot))
          (setq value (slot-value obj name))
          (when (symbolp value)        ;see `eieio-default-eval-maybe'
            (setq value (symbol-value value)))
          (setq result
                (vconcat result (c:-marshal-field obj type value
                                                    (length result))))
          (when (eq type 'c:-switch) ;c:-switch always finishes a struct
            (throw 'break 'nil)))))
    result))

(cl-defmethod c:-marshal-field ((obj c:-struct) type value &optional pos)
  "Return the byte-array representation of a field in struct OBJ of type TYPE
and value VALUE.

The optional POS argument indicates current byte index of the field (used by
`c:-pad-align' type)."
  (pcase (or (get type 'c--typealias) type)
    (`c:-u1 (c:-pack-u1 value))
    (`c:-i1 (c:-pack-i1 value))
    (`c:-u2
     (if (slot-value obj '~lsb) (c:-pack-u2-lsb value) (c:-pack-u2 value)))
    (`c:-i2
     (if (slot-value obj '~lsb) (c:-pack-i2-lsb value) (c:-pack-i2 value)))
    (`c:-u4
     (if (slot-value obj '~lsb) (c:-pack-u4-lsb value) (c:-pack-u4 value)))
    (`c:-i4
     (if (slot-value obj '~lsb) (c:-pack-i4-lsb value) (c:-pack-i4 value)))
    (`c:-u8
     (if (slot-value obj '~lsb) (c:-pack-u8-lsb value) (c:-pack-u8 value)))
    (`c:void (vector value))
    (`c:-pad
     (unless (integerp value)
       (setq value (eval value `((obj . ,obj)))))
     (make-vector value 0))
    (`c:-pad-align
     ;; The length slot in c:-request is left out
     (let ((len (if (object-of-class-p obj 'c:-request) (+ pos 2) pos)))
       (when (vectorp value)
         ;; Alignment with offset.
         (setq len (- len (aref value 1))
               value (aref value 0)))
       (unless (integerp value)
         (setq value (eval value `((obj . ,obj)))))
       (make-vector (% (- value (% len value)) value) 0)))
    (`c:-list
     (let* ((list-name (plist-get value 'name))
            (list-type (plist-get value 'type))
            (list-size (plist-get value 'size))
            (data (slot-value obj list-name)))
       (unless (integerp list-size)
         (setq list-size (eval list-size `((obj . ,obj))))
         (unless list-size
           (setq list-size (length data)))) ;list-size can be nil
       (cl-assert (= list-size (length data)))
       (mapconcat (lambda (i) (c:-marshal-field obj list-type i)) data [])))
    (`c:-switch
     (let ((slots (eieio-class-slots (eieio-object-class obj)))
           (expression (plist-get value 'expression))
           (cases (plist-get value 'cases))
           result condition name-list flag slot-type)
       (unless (integerp expression)
         (setq expression (eval expression `((obj . ,obj)))))
       (cl-assert (integerp expression))
       (dolist (i cases)
         (setq condition (car i))
         (setq name-list (cdr i))
         (setq flag nil)
         (cl-assert (or (integerp condition) (listp condition)))
         (if (integerp condition)
             (setq flag (/= 0 (logand expression condition)))
           (if (eq 'logior (car condition))
               (setq flag (/= 0 (logand expression
                                        (apply #'logior (cdr condition)))))
             (setq flag (memq expression condition))))
         (when flag
           (dolist (name name-list)
             (catch 'break
               (dolist (slot slots) ;better way to find the slot type?
                 (when (eq name (eieio-slot-descriptor-name slot))
                   (setq slot-type (cl--slot-descriptor-type slot))
                   (throw 'break nil))))
             (unless (eq slot-type 'c:-ignore)
               (setq result
                     (vconcat result
                              (c:-marshal-field obj slot-type
                                                  (slot-value obj name)
                                                  (+ pos
                                                     (length result)))))))))
       result))
    ((guard (child-of-class-p type 'c:-struct))
     (c:marshal value))
    (x (error "[C] Unsupported type for marshalling: %s" x))))

(cl-defmethod c:unmarshal ((obj c:-struct) byte-array &optional ctx
                             total-length)
  "Fill in fields of struct OBJ according to its byte-array representation.

The optional argument CTX is for <paramref>."
  (unless total-length
    (setq total-length (length byte-array)))
  (let ((slots (eieio-class-slots (eieio-object-class obj)))
        (result 0)
        slot-name tmp type)
    (catch 'break
      (dolist (slot slots)
        (setq type (cl--slot-descriptor-type slot))
        (unless (eq type 'c:-ignore)
          (setq slot-name (eieio-slot-descriptor-name slot)
                tmp (c:-unmarshal-field obj type byte-array 0
                                          (eieio-oref-default obj slot-name)
                                          ctx total-length))
          (setf (slot-value obj slot-name) (car tmp))
          (setq byte-array (substring byte-array (cadr tmp)))
          (setq result (+ result (cadr tmp)))
          (when (eq type 'c:-switch) ;c:-switch always finishes a struct
            (throw 'break 'nil)))))
    result))

(cl-defmethod c:-unmarshal-field ((obj c:-struct) type data offset
                                    initform &optional ctx total-length)
  "Return the value of a field in struct OBJ of type TYPE, byte-array
representation DATA, and default value INITFORM.

The optional argument CTX is for <paramref>.

This method returns a list of two components, with the first being the result
and the second the consumed length."
  (pcase (or (get type 'c--typealias) type)
    (`c:-u1 (list (aref data offset) 1))
    (`c:-i1 (let ((result (aref data offset)))
                (list (if (< result 128) result (- result 255)) 1)))
    (`c:-u2 (list (if (slot-value obj '~lsb)
                        (c:-unpack-u2-lsb data offset)
                      (c:-unpack-u2 data offset))
                    2))
    (`c:-i2 (list (if (slot-value obj '~lsb)
                        (c:-unpack-i2-lsb data offset)
                      (c:-unpack-i2 data offset))
                    2))
    (`c:-u4 (list (if (slot-value obj '~lsb)
                        (c:-unpack-u4-lsb data offset)
                      (c:-unpack-u4 data offset))
                    4))
    (`c:-i4 (list (if (slot-value obj '~lsb)
                        (c:-unpack-i4-lsb data offset)
                      (c:-unpack-i4 data offset))
                    4))
    (`c:-u8 (list (if (slot-value obj '~lsb)
                        (c:-unpack-u8-lsb data offset)
                      (c:-unpack-u8 data offset))
                    8))
    (`c:void (list (aref data offset) 1))
    (`c:-pad
     (unless (integerp initform)
       (when (eq 'quote (car initform))
         (setq initform (cadr initform)))
       (setq initform (eval initform `((obj . ,obj) (ctx . ,ctx)))))
     (list initform initform))
    (`c:-pad-align
     (let ((len (- total-length (- (length data) offset))))
       (if (vectorp initform)
           ;; Alignment with offset.
           (setq len (- len (aref initform 1))
                 initform (aref initform 0))
         (unless (integerp initform)
           (when (eq 'quote (car initform))
             (setq initform (cadr initform)))
           (setq initform (eval initform `((obj . ,obj) (ctx . ,ctx))))))
       (list initform (% (- initform (% len initform)) initform))))
    (`c:-list
     (when (eq 'quote (car initform))   ;unquote the form
       (setq initform (cadr initform)))
     (let ((list-name (plist-get initform 'name))
           (list-type (plist-get initform 'type))
           (list-size (plist-get initform 'size)))
       (unless (integerp list-size)
         (setq list-size (eval list-size `((obj . ,obj) (ctx . ,ctx)))))
       (cl-assert (integerp list-size))
       (pcase list-type
         (`c:char                     ;as Latin-1 encoded string
          (setf (slot-value obj list-name)
                (decode-coding-string
                 (apply #'unibyte-string
                        (append (substring data offset
                                           (+ offset list-size))
                                nil))
                 'iso-latin-1)))
         (`c:void                     ;for further unmarshalling
          (setf (slot-value obj list-name)
                (substring data offset (+ offset list-size))))
         (x
          (let ((count 0)
                result tmp)
            (dotimes (_ list-size)
              (setq tmp (c:-unmarshal-field obj x data (+ offset count) nil
                                              nil total-length))
              (setq result (nconc result (list (car tmp))))
              (setq count (+ count (cadr tmp))))
            (setf (slot-value obj list-name) result)
            (setq list-size count))))   ;to byte length
       (list initform list-size)))
    (`c:-switch
     (let ((slots (eieio-class-slots (eieio-object-class obj)))
           (expression (plist-get initform 'expression))
           (cases (plist-get initform 'cases))
           (count 0)
           condition name-list flag slot-type tmp)
       (unless (integerp expression)
         (setq expression (eval expression `((obj . ,obj) (ctx . ,ctx)))))
       (cl-assert (integerp expression))
       (dolist (i cases)
         (setq condition (car i))
         (setq name-list (cdr i))
         (setq flag nil)
         (cl-assert (or (integerp condition) (listp condition)))
         (if (integerp condition)
             (setq flag (/= 0 (logand expression condition)))
           (if (eq 'logior (car condition))
               (setq flag (/= 0 (logand expression
                                        (apply #'logior (cdr condition)))))
             (setq flag (memq expression condition))))
         (when flag
           (dolist (name name-list)
             (catch 'break
               (dolist (slot slots) ;better way to find the slot type?
                 (when (eq name (eieio-slot-descriptor-name slot))
                   (setq slot-type (cl--slot-descriptor-type slot))
                   (throw 'break nil))))
             (unless (eq slot-type 'c:-ignore)
               (setq tmp (c:-unmarshal-field obj slot-type data offset
                                               (eieio-oref-default obj name)
                                               nil total-length))
               (setf (slot-value obj name) (car tmp))
               (setq count (+ count (cadr tmp)))
               (setq data (substring data (cadr tmp)))))))
       (list initform count)))
    ((and x (guard (child-of-class-p x 'c:-struct)))
     (let* ((struct-obj (make-instance x))
            (tmp (c:unmarshal struct-obj (substring data offset) obj
                                total-length)))
       (list struct-obj tmp)))
    (x (error "[C] Unsupported type for unmarshalling: %s" x))))



(provide 'c-types)

;;; c-types.el ends here
