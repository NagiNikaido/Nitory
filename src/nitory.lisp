;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; nitory.lisp ---- A Multiple Purposed Chatbot based on OneBot framework. 
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

(defparameter *option-help*
  (adopt:make-option
   'help
   :long "help"
   :short #\h
   :help "Display help and exit."
   :reduce (constantly t)))

(defparameter *option-loglevel*
  (adopt:make-option
   'loglevel
   :parameter "LOGLEVEL"
   :long "level"
   :short #\l
   :help "Select logging level from TRACE, DEBUG, INFO, WARN, ERROR, SEVERE, FATAL."
   :reduce #'adopt:last
   :key (lambda (level-string)
          (a:switch (level-string :test #'string-equal)
            ("trace" :trace)
            ("debug" :debug)
            ("info" :info)
            ("warn" :warn)
            ("error" :error)
            ("severe" :severe)
            ("fatal" :fatal)
            (otherwise (error (format nil "Unsupported loglevel ~A." level-string)))))))

(defparameter *option-version*
  (adopt:make-option
   'version
   :long "version"
   :short #\v
   :help "Display version."
   :reduce (constantly t)))

(defparameter *option-url*
  (adopt:make-option
   'url
   :parameter "URL"
   :long "url"
   :short #\u
   :help "Websocket url of the NapCat instance."
   :reduce #'adopt:last
   :key #'identity))

(defparameter *option-addr*
  (adopt:make-option
   'address
   :parameter "ADDRESS"
   :long "addr"
   :short #\a
   :help "Websocket address of the NapCat instance."
   :reduce #'adopt:last
   :key #'identity))

(defparameter *option-port*
  (adopt:make-option
   'port
   :parameter "PORT"
   :long "port"
   :short #\p
   :help "Websocket port of the NapCat instance."
   :reduce #'adopt:last
   :key #'parse-integer))

(defparameter *ui*
  (adopt:make-interface
   :name "nitory"
   :summary "Nitory - A Multiple Purposed Chatbot based on OneBot v11 & NapCat."
   :usage "[OPTIONS]"
   :help ""
   :contents (list *option-help*
                   *option-loglevel*
                   *option-version*
                   *option-url*
                   *option-addr*
                   *option-port*)))

(defun nitory/main (opts)
  (setq *startup-timestamp* (cur-decoded-timestamp))
  (setf (v:repl-level) (or (gethash 'loglevel opts) :info))
  (v:start v:*global-controller*)
  (v:info :main "Hello from Nitory v~a, a multiple-purposed chatbot based on OneBot v11 & NapCat." +version+)
  (v:info :main "Running on ~a" (uiop:implementation-identifier))
  (setf *napcat-websocket-client* (make-napcat :url (gethash 'url opts)
                                               :address (gethash 'url opts)
                                               :port (gethash 'url opts)))
  (on :message *napcat-websocket-client* #'event/receive-message)
  (on :request *napcat-websocket-client* #'event/receive-request)
  (connect *napcat-websocket-client*)
  (loop (sleep 100)))

(defun nitory/cleanup ()
  (v:info :main "Exiting.")
  (v:stop v:*global-controller*))

(defun main ()
  (handler-case
      (multiple-value-bind (args opts) (adopt:parse-options *ui*)
        (when (gethash 'help opts)
          (adopt:print-help-and-exit *ui*))
        (when (gethash 'version opts)
          (format t "Nitory v~a~%Running on ~a~%" +version+ (uiop:implementation-identifier))
          (adopt:exit))
        (tagbody
           (sig:signal-handler-bind
               ((2 (lambda (c) (go :cleanup)))
                (15 (lambda (c) (go :cleanup))))
             (nitory/main opts))
         :cleanup
           (nitory/cleanup)))
    (error (c)
      (adopt:print-error-and-exit c))))

(defun event/receive-message (json)
  (let* ((message-type (a:ensure-gethash "message_type" json nil))
         (sub-type (a:ensure-gethash "sub_type" json nil))
         (user-id (a:ensure-gethash "user_id" json nil))
         (group-id (a:ensure-gethash "group_id" json nil))
         (msg (a:ensure-gethash "raw_message" json nil))
         (sender (a:ensure-gethash "sender" json nil))
         (ignore nil))
    (format t "~a~%" msg)
    (unless ignore
      (let ((rmsg (nitory/parse-and-respond sender msg)))
        (format t "~a~%" rmsg)
        (when rmsg
          (a:switch (message-type :test #'equal)
            ("private" (do-send-private-msg *napcat-websocket-client*
                         `((:user-id . ,user-id)
                           (:message . #(((:type . "text")
                                          (:data . ((:text . ,rmsg)))))))))
            ("group" (do-send-group-msg *napcat-websocket-client*
                       `((:group-id . ,group-id)
                         (:message . #(((:type . "text")
                                        (:data . ((:text . ,rmsg)))))))))))))))

(defun nitory/parse-and-respond (sender message)
  (let ((leading (char message 0))
        (rdmsg (concatenate 'string message " ")))
    (when (or (char= #\/ leading)
              (char= #\. leading))
      (let* ((pos (position #\space rdmsg))
             (command (subseq rdmsg 1 pos))
             (rest (string-trim " " (subseq rdmsg (+ 1 pos)))))
        (a:switch (command :test #'equal)
          ("help" (nitory/print-help rest))
          ("r" (dice/roll-dice rest sender))
          (otherwise (nitory/command-not-supported)))))))

(defun nitory/print-help (rest)
  "Project-Nitory v0.0.1 by NagiNikaido
指令列表:
.r: 掷骰指令（默认d20）
.help: 显示本帮助
更多功能开发中")


(defun nitory/command-not-supported ()
  "无效指令")

(defun event/receive-notice (notice)
  ())

(defun event/receive-request (json)
  (format t "event/receive-request:~%")
  (let* ((request-type (a:ensure-gethash "request_type" json nil))
         (sub-type (a:ensure-gethash "sub_type" json nil))
         (group-id (a:ensure-gethash "group_id" json nil))
         (user-id (a:ensure-gethash "user_id" json nil))
         (comment (a:ensure-gethash "comment" json nil))
         (flag (a:ensure-gethash "flag" json nil)))
    (format t "~A ~A~%" request-type sub-type)
    (if (string= request-type "friend")
        (respond/do-friend-request user-id comment flag)
        (if (string= request-type "group")
            (a:switch (sub-type :test #'equal)
              ("add" (respond/do-group-add-request group-id user-id comment flag))
              ("invite" (respond/do-group-invite-request group-id user-id comment flag)))))))

(defun respond/do-friend-request (user-id comment flag)
  (let ((approve nil))
    (do-set-friend-add-request *napcat-websocket-client*
      `((:flag . ,flag)
        (:approve . ,approve)))))

(defun respond/do-group-add-request (group-id user-id comment flag)
  ())

(defun respond/do-group-invite-request (group-id user-id comment flag)
  (let ((approve (= user-id 1203794101)))
    (do-set-group-add-request *napcat-websocket-client*
      `((:flag . ,flag)
        (:type . "invite")
        (:approve . ,approve)))))


(defun event/receive-meta-event (meta-event)
  ())
