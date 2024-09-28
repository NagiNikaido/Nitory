;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; nitory.lisp ---- A Multipurpose Chatbot based on OneBot framework. 
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
            (otherwise (error (s:fmt "Unsupported loglevel ~A." level-string)))))))

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
  (setf *startup-timestamp* (current-decoded-timestamp))
  (setf (v:repl-level) (or (@ opts 'loglevel) :info))
  (let ((log-file (s:path-join *prefix* "log.txt")))
    (v:define-pipe ()
      (v:file-faucet :file log-file)))
  (v:start v:*global-controller*)
  (v:info :main "Hello from Nitory v~a, a multipurpose chatbot based on OneBot v11 & NapCat." +version+)
  (v:info :main "Running on ~a" (uiop:implementation-identifier))
  (let ((admin (uiop:getenv "NITORY_ADMIN")))
    (when admin
      (setf *admin* (parse-integer admin))))
  (let ((prefix (uiop:getenv "NITORY_SAVE_PREFIX")))
    (when prefix
      (setf *prefix* prefix)))
  (setf *napcat-websocket-client* (make-napcat :url (@ opts 'url)
                                               :address (@ opts 'url)
                                               :port (@ opts 'url)))
  (v:info :main "Got NapCat instance.")
  (v:info :main "Updating event emitter.")
  (on :message *napcat-websocket-client* #'event/receive-command)
  (on :request *napcat-websocket-client* #'event/receive-request)
  (on :meta-event.heartbeat *napcat-websocket-client* (lambda (&rest rest) (db/save-dbs) (values)))
  (v:info :main "Enable nickname service.")
  (nick/enable-nick)
  (khst/enable-khst)
  (v:info :main "Starting formal connection.")
  (connect *napcat-websocket-client*)
  (v:info :main "Done. Trapped into a loop.")
  (bb:alet ((res (do-get-login-info *napcat-websocket-client* '())))
           (setf *self-id* (@ res "user_id"))
           (v:info :main "Logged in with ~a" *self-id*))
  (loop (sleep 100)))

(defun nitory/cleanup ()
  (v:info :main "Exiting.")
  (db/save-dbs)
  (v:stop v:*global-controller*))

(defun main ()
  (handler-case
      (multiple-value-bind (args opts) (adopt:parse-options *ui*)
        (when (@ opts 'help)
          (adopt:print-help-and-exit *ui*))
        (when (@ opts 'version)
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

(defun split-at (message)
  (let ((at? (first message)))
    (v:debug :main "~a" (@ at? "type"))
    (if (and at?
             (string= "at" (@ at? "type")))
        (list (parse-integer (@ at? "data" "qq"))
              (cdr message))
        (list nil message))))

(defun split-reply (message)
  (let ((reply? (first message)))
    (v:debug :main "~a" (@ reply? "type"))
    (if (and reply?
             (string= "reply" (@ reply? "type")))
        (cons (parse-integer (@ reply? "data" "id"))
              (split-at (cdr message)))
        (cons nil (split-at message)))))

(defun event/receive-command (json)
  (v:debug :main "~a" (split-reply (@ json "message")))
  (multiple-value-bind (reply-id at-id msg)
      (values-list (split-reply (@ json "message")))
    (when (and (= 1 (length msg))
               (string= "text" (@ (first msg) "type")))
      (let ((raw-msg (str:trim (@ (first msg) "data" "text"))))
        (when (command-string-p raw-msg)
          (v:debug :main "Received command: ~a" raw-msg)
          (let ((raw-args (str:words (subseq raw-msg 1))))
            (handler-case
                (unless (loop for cmd in *commands*
                              when (parse-command cmd json raw-args :reply reply-id
                                                                    :at at-id)
                                return t
                              end)
                  (nitory/cmd-not-supported json raw-args))
              (command-parse-error (c)
                (reply-to *napcat-websocket-client*
                          json (make-message
                                (or (error-message c)
                                    (s:fmt "告诉他我的系统出问题了"))))))))))))

(defun nitory/cmd-not-supported (json args &key &allow-other-keys)
  (let* ((msg "无效指令"))
    (reply-to *napcat-websocket-client*
              json (make-message msg))))

(defun event/receive-notice (notice)
  ())

(defun event/receive-request (json)
  (format t "event/receive-request:~%")
  (let* ((request-type (@ json "request_type"))
         (sub-type (@ json "sub_type"))
         (group-id (@ json "group_id"))
         (user-id (@ json "user_id"))
         (comment (@ json "comment"))
         (flag (@ json "flag")))
    (format t "~A ~A~%" request-type sub-type)
    (if (string= request-type "friend")
        (respond/do-friend-request user-id comment flag)
        (if (string= request-type "group")
            (str:string-case sub-type
              ("add" (respond/do-group-add-request group-id user-id comment flag))
              ("invite" (respond/do-group-invite-request group-id user-id comment flag)))))))

(defun respond/do-friend-request (user-id comment flag)
  (let ((approve t))
    (do-set-friend-add-request *napcat-websocket-client*
      `((:flag . ,flag)
        (:approve . ,approve)))))

(defun respond/do-group-add-request (group-id user-id comment flag)
  ())

(defun respond/do-group-invite-request (group-id user-id comment flag)
  (let ((approve (and *admin* (= user-id *admin*))))
    (do-set-group-add-request *napcat-websocket-client*
      `((:flag . ,flag)
        (:type . "invite")
        (:approve . ,approve)))))


(defun event/receive-meta-event (meta-event)
  ())
