;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; napcat-test.lisp ---- Test cases for NapCat.
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

(in-package #:nitory/test)

(defvar *test-client* (nitory:make-napcat :dry-run t))

(nitory:on :message *test-client*
           (lambda (json)
             (format t "message.~a" (gethash "message_type" json))))

(nitory:on :notice *test-client*
           (lambda (json)
             (format t "notice.~a" (gethash "notice_type" json))))

(nitory:on :request *test-client*
           (lambda (json)
             (format t "request.~a" (gethash "request_type" json))))

(nitory:on :meta-event *test-client*
           (lambda (json)
             (format t "meta-event.~a" (gethash "meta_event_type" json))))

(nitory:connect *test-client*)

(deftest receive-events
  (ok (outputs
          (nitory:receive-data *test-client*
                               (nitory:encode-to-json-string
                                '((:post-type . "message")
                                  (:message-type . "group"))))
          "message.group"))
  (ok (outputs
          (nitory:receive-data *test-client*
                               (nitory:encode-to-json-string
                                '((:post-type . "meta_event")
                                  (:meta-event-type . "heartbeat"))))
          "meta-event.heartbeat")))

(deftest do-sends
  (let ((message #(((:type . "text")
                    (:data . ((:text . "nihao")))))))
    (ok (typep message 'nitory:message))
    (let* ((sent (nitory:do-send-group-msg *test-client*
                   `((:group-id . 123456)
                     (:message . ,message))
                   ))
           (serial (nitory:cur-packet-id *test-client*)))
      (v:info :out "~a" sent)
      (nitory:receive-data *test-client*
                           (nitory:encode-to-json-string
                            `((:status . "ok")
                              (:retcode . 0)
                              (:data . ((:message-id . 123)))
                              (:echo . ,serial))))
      (ok (outputs
              (bb:catcher
               (bb:attach
                sent
                (lambda (json)
                  (v:info :promise "~a" json)
                  (format t "message_id=~a" (gethash "message_id" json))))
               (t (json)
                  (v:info :promise "~a" json)))        
           (s:fmt "message_id=123"))))))
