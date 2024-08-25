;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; nick.lisp ---- Nickname database.
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

(defvar *nicknames* nil)

(defun nick/enable-nick ()
  (handler-case
      (with-open-file (s "nicknames"
			 :direction :input
			 :if-does-not-exist :error)
	(read *nicknames*))
    (error (c)
      (v:warn :nick "~a" c)))
  (on :meta-event.heartbeat *napcat-websocket-client* (lambda (json) (nick/save-nicks))))

(defun nick/save-nicks ()
  (with-open-file (s "nicknames"
		     :direction :output
		     :if-exists :supersede)
    (v:info :nick "Saveing nicknames...")
    (unwind-protect (print *nicknames* s))
    (v:info :nick "Done.")))

(defun nick/get-nick (user-id)
  (getf *nicknames* user-id))

(defun nick/set-nick (user-id nick)
  (setf (getf *nicknames* user-id) nick))

(defun nick/rm-nick (user-id)
  (remf *nicknames* user-id))

(defun nick/cmd-set-nick (json args)
  (let* ((msg-type (gethash "message_type" json))
	 (group-id (gethash "group_id" json))
	 (user-id (gethash "user_id" json))
	 (sender (gethash "sender" json))
	 (default-nick (gethash "nickname" sender))
	 (current-nick (second args))
	 (msg (let ((arg-len (length args)))
		(cond
		  ((= 1 arg-len) (progn
				   (nick/rm-nick user-id)
				   (format nil "* 恢复 ~a 的默认昵称" default-nick)))
		  ((= 2 arg-len) (progn
				   (nick/set-nick user-id current-nick)
				   (format nil "* ~a 现在的昵称为 ~a" default-nick current-nick)))
		  (t "指令格式错误")))))
    (do-send-msg *napcat-websocket-client*
      (list (a:switch (msg-type :test #'equal)
	      ("group" `(:group-id . ,group-id))
	      ("private" `(:user-id . ,user-id)))
	    `(:message-type . ,msg-type)
	    `(:message . #(((:type . "text")
			    (:data . ((:text . ,msg))))))))))
