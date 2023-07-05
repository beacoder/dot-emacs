;;; etelnet-proto.el --- telnet protocol Emacs Lisp Binding  -*- lexical-binding: t -*-

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
;; telnet protocol implemented by elisp
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(require 'ipc-types)
(eval-when-compile (require 'cl-lib))

;; TELNET commands consist of at least a two byte sequence:
;; the "Interpret as Command" (IAC) escape character followed by
;; the code for the command.
;;
;; NAME               CODE              MEANING
;; --------------------------------------------------------------
;; SE                  240    End of subnegotiation parameters.
;; NOP                 241    No operation.
;; Data Mark           242    The data stream portion of a Synch.
;;                            This should always be accompanied
;;                            by a TCP Urgent notification.
;; Break               243    NVT character BRK.
;; Interrupt Process   244    The function IP.
;; Abort output        245    The function AO.
;; Are You There       246    The function AYT.
;; Erase character     247    The function EC.
;; Erase Line          248    The function EL.
;; Go ahead            249    The GA signal.
;; SB                  250    Indicates that what follows is
;;                            subnegotiation of the indicated
;;                            option.
;; WILL (option code)  251    Indicates the desire to begin
;;                            performing, or confirmation that
;;                            you are now performing, the
;;                            indicated option.
;; WON'T (option code) 252    Indicates the refusal to perform,
;;                            or continue performing, the
;;                            indicated option.
;; DO (option code)    253    Indicates the request that the
;;                            other party perform, or
;;                            confirmation that you are expecting
;;                            the other party to perform, the
;;                            indicated option.
;; DON'T (option code) 254    Indicates the demand that the
;;                            other party stop performing,
;;                            or confirmation that you are no
;;                            longer expecting the other party
;;                            to perform, the indicated option.
;; IAC                 255    Data Byte 255.

(defconst etelnet:Command:EOF 236)
(defconst etelnet:Command:SUS 237)
(defconst etelnet:Command:ABT 238)
(defconst etelnet:Command:EOR 239)
(defconst etelnet:Command:SE  240)
(defconst etelnet:Command:NOP 241)
(defconst etelnet:Command:DM  242)
(defconst etelnet:Command:BRK 243)
(defconst etelnet:Command:IP  244)
(defconst etelnet:Command:AO  245)
(defconst etelnet:Command:AYT 246)
(defconst etelnet:Command:EC  247)
(defconst etelnet:Command:EL  248)
(defconst etelnet:Command:GA  249)
(defconst etelnet:Command:SB  250)
(defconst etelnet:Command:WIL 251)
(defconst etelnet:Command:WOT 252)
(defconst etelnet:Command:DO  253)
(defconst etelnet:Command:DOT 254)
(defconst etelnet:Command:IAC 255)

;; TELNET options
(defconst etelnet:Option:BINARY         0)
(defconst etelnet:Option:ECHO           1)
(defconst etelnet:Option:RCP            2)
(defconst etelnet:Option:SGA            3)
(defconst etelnet:Option:NAMS           4)
(defconst etelnet:Option:STATUS         5)
(defconst etelnet:Option:TM             6)
(defconst etelnet:Option:RCTE           7)
(defconst etelnet:Option:NAOL           8)
(defconst etelnet:Option:NAOP           9)
(defconst etelnet:Option:NAOCRD         10)
(defconst etelnet:Option:NAOHTS         11)
(defconst etelnet:Option:NAOHTD         12)
(defconst etelnet:Option:NAOFFD         13)
(defconst etelnet:Option:NAOVTS         14)
(defconst etelnet:Option:NAOVTD         15)
(defconst etelnet:Option:NAOLFD         16)
(defconst etelnet:Option:XASCII         17)
(defconst etelnet:Option:LOGOUT         18)
(defconst etelnet:Option:BM             19)
(defconst etelnet:Option:DET            20)
(defconst etelnet:Option:SUPDUP         21)
(defconst etelnet:Option:SUPDUPOUTPUT   22)
(defconst etelnet:Option:SNDLOC         23)
(defconst etelnet:Option:TTYPE          24)
(defconst etelnet:Option:EOR            25)
(defconst etelnet:Option:TUID           26)
(defconst etelnet:Option:OUTMRK         27)
(defconst etelnet:Option:TTYLOC         28)
(defconst etelnet:Option:3270REGIME     29)
(defconst etelnet:Option:X3PAD          30)
(defconst etelnet:Option:NAWS           31)
(defconst etelnet:Option:TSPEED         32)
(defconst etelnet:Option:LFLOW          33)
(defconst etelnet:Option:LINEMODE       34)
(defconst etelnet:Option:XDISPLOC       35)
(defconst etelnet:Option:ENVIRON        36)
(defconst etelnet:Option:AUTHENTICATION 37)
(defconst etelnet:Option:ENCRYPT        38)
(defconst etelnet:Option:NEW_ENVIRON    39)
(defconst etelnet:Option:MSSP           70)
(defconst etelnet:Option:COMPRESS       85)
(defconst etelnet:Option:COMPRESS2      86)
(defconst etelnet:Option:ZMP            93)
(defconst etelnet:Option:EXOPL          255)
(defconst etelnet:Option:MCCP2          86)

;; When used for remote user access to service hosts (i.e., remote
;; terminal access) this protocol is assigned server port 23
(defconst etelnet:Server:Port 23)

;;;; telnet connection related

(defclass etelnet:connection (c:--struct)
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
  :documentation "Telnet connection.")

;;

