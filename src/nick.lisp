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

(defvar *nicknames* (make-hash-table))
(defvar *nick-path* nil)

(defun nick/enable-nick ()
  (setf *nick-path* (merge-pathnames *prefix* "nicknames"))
  (ensure-directories-exist *nick-path*)
  (handler-case
      (with-open-file (s *nick-path*
			 :direction :input
			 :if-does-not-exist :error)
	(setf *nicknames* (a:plist-hash-table (read s) :test #'equal)))
    (error (c)
      (v:warn :nick "~a" c)))
  (on :meta-event.heartbeat *napcat-websocket-client* (lambda (json) (nick/save-nicks))))

(defun nick/save-nicks ()
  (with-open-file (s *nick-path*
		     :direction :output
		     :if-exists :supersede)
    (v:info :nick "Saving nicknames...")
    (unwind-protect (print (a:hash-table-plist *nicknames*) s))
    (v:info :nick "Done.")))

(defun nick/get-nick (user-id)
  (gethash user-id *nicknames*))

(defun nick/set-nick (user-id nick)
  (setf (gethash user-id *nicknames*) nick))

(defun nick/rm-nick (user-id)
  (remhash user-id *nicknames*))

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
				   (s:fmt "* 恢复 ~a 的默认昵称" default-nick)))
		  ((= 2 arg-len) (progn
				   (nick/set-nick user-id current-nick)
				   (s:fmt "* ~a 现在的昵称为 ~a" default-nick current-nick)))
		  (t "指令格式错误")))))
    (do-send-msg *napcat-websocket-client*
      (list (str:string-case msg-type
	      ("group" `(:group-id . ,group-id))
	      ("private" `(:user-id . ,user-id)))
	    `(:message-type . ,msg-type)
            (make-message msg)))))
