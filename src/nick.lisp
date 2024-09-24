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
  (@ *nicknames* user-id))

(defun nick/set-nick (user-id nick)
  (setf (@ *nicknames* user-id) nick))

(defun nick/rm-nick (user-id)
  (remhash user-id *nicknames*))

(defun nick/cmd-set-nick-deprecated (json args)
  (let* ((user-id (@ json "user_id"))
	 (sender (@ json "sender"))
	 (default-nick (@ sender "nickname"))
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
    (reply-to *napcat-websocket-client*
              json (make-message msg))))

(defun nick/cmd-set-nick (json nickname &key &allow-other-keys)
  (let* ((user-id (@ json "user_id"))
         (sender (@ json "sender"))
         (default-nick (@ sender "nickname"))
         (msg (if nickname
                  (progn
                    (nick/set-nick user-id nickname)
                    (s:fmt "* ~a 现在的昵称为 ~a" default-nick nickname))
                  (progn
                    (nick/rm-nick user-id)
                    (s:fmt "* 恢复 ~a 的默认昵称" default-nick)))))
    (reply-to *napcat-websocket-client*
              json (make-message msg))))

(register-command
 (make-command :display-name "nn"
               :cmd-face "nn"
               :hidden nil
               :short-usage "设置昵称"
               :options (list (make-option
                               "nickname"
                               :predicator
                               (lambda (opt)
                                 (when (command-string-p opt)
                                   (error 'command-parse-error :error-type :wrong-argument
                                          :error-message "昵称不可以.或/开头")
                                   t))
                               :optional t))
               :action #'nick/cmd-set-nick
               :usage
"设置昵称
.nn [新昵称]  将昵称设置为新昵称
默认昵称为QQ昵称
如将新昵称留空，则将当前昵称恢复为默认昵称"))
