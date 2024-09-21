;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; package.lisp ---- Package definition for Nitory.
;;;
;;; Copyright (C) 2024  NagiNikaido <naginikaido@kuusouhakuchuu.cn>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;

(defpackage #:nitory
  (:use #:common-lisp)
  (:local-nicknames (:a :alexandria)
                    (:s :serapeum)
                    (:re :cl-ppcre)
                    (:j :yason)
                    (:v :org.shirakumo.verbose)
                    (:sig :trivial-signal)
                    (:bb :blackbird)
                    (:dex :dexador))
  (:import-from :serapeum #:->)
  (:export
   #:main
   #:dice/parse-dice-cell
   #:dice/parse-dice-term
   #:dice/parse-dice-expr
   #:dice/parse-dice-full-expr
   #:dice/exec-dice-tree
   #:dice/roll-dice
   ;; from utils
   #:encode-to-json-string
   ;; from napcatp-types
   #:alist-sim-p
   #:segment-p
   #:message-p
   #:message
   ;; from napcat
   #:napcat
   #:make-napcat
   #:url
   #:dry-run
   #:connect
   #:on
   #:once
   #:emit
   #:remove-listener
   #:remove-all-listeners
   #:listeners
   #:listener-count
   #:cur-packet-id
   #:receive-data
   #:receive-message
   #:receive-notice
   #:receive-request
   #:receive-meta-event
   #:receive-response
   #:send-data
   #:do-get-friends-with-category
   #:do-set-group-card
   #:do-can-send-record
   #:do-debug
   #:do-set-group-leave
   #:do-get-friend-list
   #:do-set-group-admin
   #:do-get-group-list
   #:do-forward-friend-single-msg
   #:do-set-friend-add-request
   #:do-set-msg-emoji-like
   #:do-get-group-file-list
   #:do-get-rebot-uin-range
   #:do-get-group-member-list
   #:do-get-group-member-info
   #:do-delete-msg
   #:do-get-status
   #:do-get-login-info
   #:do-get-group-info
   #:do-get-group-file-count
   #:do-clean-cache
   #:do-set-group-add-request
   #:do-set-group-kick
   #:do-can-send-image
   #:do-del-group-file-folder
   #:do-get-version-info
   #:do-send-group-msg
   #:do-set-group-whole-ban
   #:do-del-group-file
   #:do-set-group-file-folder
   #:do-get-record
   #:do-get-image
   #:do-set-group-ban
   #:do-send-private-msg
   #:do-get-cookies
   #:do-get-file
   #:do-set-qq-avatar
   #:do-send-msg
   #:do-forward-group-single-msg
   #:do-get-msg
   #:do-set-group-name
   #:do-reboot-normal))
