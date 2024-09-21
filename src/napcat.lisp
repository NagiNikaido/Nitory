;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; napcat.lisp ---- NapCat APIs.
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

(in-package #:nitory)

(defconstant +max-packet-count+ #x100000000)

(defclass napcat (event-bus)
  ((url
    :initarg :url
    :initform "ws://localhost:3001"
    :accessor url)
   (napcat-websocket-client
    :accessor client)
   (total-packets
    :initform 0
    :accessor total-packets)
   (dry-run ; for testing
    :initarg :dry-run
    :initform nil
    :accessor dry-run)))

(defun make-napcat (&key url address port dry-run)
  (v:info :napcat "Making NapCat.")
  (make-instance 'napcat :url (or url
                                  (s:fmt "ws://~a:~d"
                                          (or address "localhost")
                                          (or port 3001)))
                 :dry-run dry-run))

(defmethod initialize-instance :after ((napcat-instance napcat) &key url)
  (setf (client napcat-instance) (wsd:make-client url)))

(defmethod connect ((napcat-instance napcat))
  (v:info :napcat "Connect websocket at ~a" (url napcat-instance))
  (let ((client (client napcat-instance)))
    (emit ":socket.connecting" napcat-instance)
    (wsd:on :open client
            (lambda ()
              (emit ":socket.open" napcat-instance)))
    (wsd:on :message client
            (lambda (message)
              (receive-data napcat-instance message)))
    (wsd:on :error client
            (lambda (error)
              (emit ":socket.error" napcat-instance error)))
    (wsd:on :close client
            (lambda (&key code reason)
              (emit ":socket.close" napcat-instance :code code :reason reason)))
    (unless (dry-run napcat-instance)
      (handler-case (wsd:start-connection client)
        (error (e) (emit ":socket.error" napcat-instance e))))))

(defmethod receive-data ((napcat-instance napcat) data)
  (v:info :napcat "received data: ~A" data)
  (let* ((json (j:parse data))
         (event-type (gethash "post_type" json)))
    (str:string-case event-type
      ("message" (receive-message napcat-instance json))
      ("notice" (receive-notice napcat-instance json))
      ("request" (receive-request napcat-instance json))
      ("meta_event" (receive-meta-event napcat-instance json))
      (otherwise (if (gethash "echo" json)
                   (receive-response napcat-instance json)
                   (v:error :napcat "Unsupported event type: ~a." event-type))))))

(defmethod receive-message ((napcat-instance napcat) json)
  (let ((message-type (gethash "message_type" json)))
    (str:string-case message-type
      ("group" (emit ":message.group" napcat-instance json))
      ("private" (emit ":message.private" napcat-instance json))
      (otherwise (v:error :napcat "Unsupported message type: ~a." message-type)))))

(defmethod receive-notice ((napcat-instance napcat) json)
  (let ((notice-type (gethash "notice_type" json)))
    (str:string-case notice-type
      ("group_upload" (emit ":notice.group_upload" napcat-instance json))
      ("group_admin" (emit ":notice.group_admin" napcat-instance json))
      ("group_decrease" (emit ":notice.group_decrease" napcat-instance json))
      ("group_increase" (emit ":notice.group_increase" napcat-instance json))
      ("group_ban" (emit ":notice.group_ban" napcat-instance json))
      ("group_recall" (emit ":notice.group_recall" napcat-instance json))
      ("friend_add" (emit ":notice.friend_add" napcat-instance json))
      ("friend_recall" (emit "notice.friend_recall" napcat-instance json))
      ("notify" (let ((sub-type (gethash "sub_type" json)))
                  (str:string-case sub-type
                    ("poke" (emit ":notice.notify.poke" napcat-instance json))
                    ("lucky_king" (emit ":notice.notify.lucky_king" napcat-instance json))
                    ("honor" (emit ":notice.notify.honor" napcat-instance json))
                    (otherwise (v:error :napcat "Unsupported notice.notify type: ~a." sub-type)))))
      (otherwise (v:error :napcat "Unsupported notice type: ~a." notice-type)))))

(defmethod receive-request ((napcat-instance napcat) json)
  (let ((request-type (gethash "request_type" json)))
    (str:string-case request-type
      ("friend" (emit ":request.friend" napcat-instance json))
      ("group" (let ((sub-type (gethash "sub_type" json)))
                 (str:string-case sub-type
                   ("add" (emit ":request.group.add" napcat-instance json))
                   ("invite" (emit ":request.group.invite" napcat-instance json))
                   (otherwise (v:error :napcat "Unsupported request.group type: ~a." sub-type)))))
      (otherwise (v:error :napcat "Unsupported request type: ~a." request-type)))))

(defmethod receive-meta-event ((napcat-instance napcat) json)
  (let ((meta-event-type (gethash "meta_event_type" json)))
    (str:string-case meta-event-type
      ("heartbeat" (emit ":meta_event.heartbeat" napcat-instance json))
      ("lifecycle" (let ((sub-type (gethash "sub_type" json)))
                     (str:string-case sub-type
                       ("enable" (emit ":meta_event.enable" napcat-instance json))
                       ("disable" (emit ":meta_event.disable" napcat-instance json))
                       ("connect" (emit ":meta_event.connect" napcat-instance json))
                       (otherwise (v:error :napcat "Unsupported meta_event.lifecycle type: ~a." sub-type)))))
      (otherwise (v:error :napcat "Unsupported meta_event type: ~a." meta-event-type)))))

(defmethod receive-response ((napcat-instance napcat) json)
  (let ((echo (gethash "echo" json)))
    (emit echo napcat-instance json)))

(defmethod cur-packet-id ((napcat-instance napcat))
  (s:fmt ":x~8,'0x" (total-packets napcat-instance)))

(defmethod gen-packet-id ((napcat-instance napcat))
  (declare (inline gen-packet-id))
  (let ((serial (mod (1+ (total-packets napcat-instance)) +max-packet-count+)))
    (setf (total-packets napcat-instance) serial)
    (s:fmt ":x~8,'0x" serial)))

(defmethod send-data ((napcat-instance napcat) action params)
  (let* ((serial (gen-packet-id napcat-instance))
         (data (encode-to-json-string `((:action . ,action)
                                        (:params . ,params)
                                        (:echo . ,serial)))))
    (v:info :napcat "Sending data: ~a" data)
    (bb:with-promise (resolve reject)
      (once serial napcat-instance
            (lambda (json)
              (let ((data (gethash "data" json)))
                (if (= 0 (gethash "retcode" json))
                    (resolve data)
                    (reject json)))))
      (cond
        ((dry-run napcat-instance) data)
        ((not (client napcat-instance))
         (receive-data napcat-instance
                       (encode-to-json-string `((:status . "failed")
                                                (:retcode . -1)
                                                (:data . nil)
                                                (:message . "Invalid websocket client - maybe not initialized or early disposed?")
                                                (:echo . ,serial)))))
        ((not (eq :open (wsd:ready-state (client napcat-instance))))
         (receive-data napcat-instance
                       (encode-to-json-string `((:status . "failed")
                                                (:retcode . -1)
                                                (:data . nil)
                                                (:message . "Invalid websocket connection.")
                                                (:echo . ,serial)))))
        (t (wsd:send-text (client napcat-instance) data))))))

(macrolet ((%apis (&rest api-list)
             (flet ((%append-api (api param-model)
                      (let ((sym (to-sym (str:concat "do-" api))))
                        `(progn
                           (defmethod ,sym ((napcat-instance napcat) params)
                             (assert (alist-sim-p params ,param-model))
                             (send-data napcat-instance ,api params))
                           (export ',sym)))))
               `(block apis ,@(loop for (api param-model) on api-list by #'cddr
                        collect (%append-api api param-model))))))
  
  (%apis ;; from NapCat
         "reboot_normal" '((:delay? . integer))
         "get_rebot_uin_range" '()
         ;; "set_online_status"
         "get_friends_with_category" '()
         "set_qq_avatar" '((:file . string))
         "debug" '((:method . string)
                   (:args . vector))
         "get_file" '((:file-id . string))
         "forward_friend_single_msg" '((:message-id . integer)
                                       (:user-id . integer))
         "forward_group_single_msg" '((:message-id . integer)
                                      (:group-id . integer))
         ;; "translate_en2zh"
         "get_group_file_count" '((:group-id . integer))
         "get_group_file_list" '((:group-id . integer)
                                 (:start-index . integer)
                                 (:file-count . integer))
         "set_group_file_folder" '((:group-id . integer)
                                   (:folder-name . string))
         "del_group_file" '((:group-id . integer)
                            (:file-id . string))
         "del_group_file_folder" '((:group-id . integer)
                                   (:folder-id . string))
         ;; from OneBot v11
         ;; "reboot"
         ;; "send_like"
         "get_login_info" '()
         "get_friend_list" '()
         "get_group_info" '((:group-id . integer))
         "get_group_list" '()
         "get_group_member_info" '((:group-id . integer)
                                   (:user-id . integer)
                                   (:no-cache? . boolean))
         "get_group_member_list" '((:group-id . integer)
                                   (:no-cache? . boolean))
         "get_msg" '((:message-id . integer))
         "send_msg" '((:group-id? . integer)
                      (:user-id? . integer)
                      (:message . message)
                      (:auto-escape? . boolean))
         "send_private_msg" '((:user-id . integer)
                              (:message . message)
                              (:auto-escape? . boolean))
         "send_group_msg" '((:group-id . integer)
                            (:message . message)
                            (:auto-escape? . boolean))
         "delete_msg" '((:message-id . integer))
         "set_msg_emoji_like" '((:message-id . integer)
                                (:emoji-id . string))
         "set_group_add_request" '((:flag . string)
                                   (:approve? . boolean)
                                   (:reason? . string))
         "set_friend_add_request" '((:flag . string)
                                    (:approve? . boolean)
                                    (:remark? . string))
         "set_group_leave" '((:group-id . integer)
                             (:is-dismiss? . boolean))
         "get_version_info" '()
         "get_status" '()
         "can_send_record" '()
         "can_send_image" '()
         "set_group_kick" '((:group-id . integer)
                            (:user-id . integer)
                            (:reject-add-request? . boolean))
         "set_group_ban" '((:group-id . integer)
                           (:user-id . integer)
                           (:duration . integer))
         "set_group_whole_ban" '((:group-id . integer)
                                 (:enable? . boolean))
         "set_group_admin" '((:group-id . integer)
                             (:user-id . integer)
                             (:enable? . boolean))
         "set_group_card" '((:group-id . integer)
                            (:user-id . integer)
                            (:card . string))
         "set_group_name" '((:group-id . number)
                            (:group-name . string))
         "get_image" '((:file . string))
         "get_record" '((:file . string))
         "clean_cache" '()
         "get_cookies" '((:domain . string))
         ;; from GO-CQHTTP
         ))

(defmethod reply-to ((napcat-instance napcat) json msg &optional msg-type)
  (let* ((r-msg-type (or (when (string= msg-type "private") "private")
                         (gethash "message_type" json)))
         (group-id (gethash "group_id" json))
         (user-id (gethash "user_id" json)))
  (do-send-msg napcat-instance
    (list (str:string-case r-msg-type
            ("group" `(:group-id . ,group-id))
            ("private" `(:user-id . ,user-id)))
          `(:message-type . ,msg-type)
          `(:message . ,msg)))))
