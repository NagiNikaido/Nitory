;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; khst.lisp ---- Randomly pick an image in respond to certain keywords
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

(defvar *khst-lists* (make-hash-table))
(defvar *khst-lists-path* nil)
(defvar *khst-pic-prefix* nil)

(defun khst/enable-khst ()
  (setf *khst-lists-path* (merge-pathnames "khst-lists" *prefix*))
  (setf *khst-pic-prefix* (merge-pathnames "pics/" *prefix*))
  (ensure-directories-exist *khst-lists-path*)
  (handler-case
      (with-open-file (s *khst-lists-path*
			 :direction :input
			 :if-does-not-exist :error)
	(setf *khst-lists* (a:plist-hash-table (read s) :test #'equal)))
    (error (c)
      (v:warn :khst "~a" c)))
  (on :message.group *napcat-websocket-client*
      #'khst/capture-keyword-and-respond-image)
  (on :meta-event.heartbeat *napcat-websocket-client*
      (lambda (json) (khst/save-list))))

(defun khst/save-list ()
  (with-open-file (s *khst-lists-path*
		     :direction :output
		     :if-exists :supersede)
    (v:info :khst "Saving khst lists...")
    (unwind-protect (print (a:hash-table-plist *khst-lists*) s))
    (v:info :khst "Done.")))

(defun khst/capture-keyword-and-respond-image (json)
  (let ((message (gethash "message" json))
	(group-id (gethash "group_id" json)))
    (when (and (= 1 (length message))
               (string= "text" (gethash "type" (first message))))
      (let* ((raw-msg (gethash "text" (gethash "data" (first message))))
             (entries (gethash raw-msg *khst-lists*)))
        (v:debug :khst "Seeking keywords in: ~a" raw-msg)
        (when entries
          (let ((entry (elt entries (random (length entries)))))
            (v:info :khst "Found ~a for keyword \"~a\"" raw-msg)
            (do-send-group-msg *napcat-websocket-client*
              `((:group-id . ,group-id)
                (:message . #(((:type . "image")
                               (:data . ((:file . (concat "file://" entry)))))))))))))))

(defun khst/save-and-add-to-list (keyword picture)
  (let* ((filename (merge-pathnames (file-namestring picture) *khst-pic-prefix*)))
    (unless (gethash keyword *khst-lists*)
      (setf (gethash keyword *khst-lists*) nil))
    (push (namestring filename) (gethash keyword *khst-lists*))
    (ensure-directories-exist filename)
    (uiop:copy-file picture filename)))

(defun khst/cmd-khst (json args)
  (let* ((msg-type (gethash "message_type" json))
         (message (gethash "message" json))
         (group-id (gethash "group_id" json))
         (user-id (gethash "user_id" json))
         (msg nil))
    (if (string/= "group" msg-type)
        (setf msg "本指令仅在群聊中可用")
        (if (/= 2 (length args))
            (setf msg "格式错误")
            (let* ((keyword (elt args 1))
                   (sema (bt2:make-semaphore))
                   (cb (lambda (njson)
                         (let ((ngroup-id (gethash "group_id" njson))
                               (nuser-id (gethash "user_id" njson))
                               (nmsg (gethash "message" njson)))
                           (when (and (= ngroup-id group-id) (= nuser-id user-id))
                             (if (or (/= 1 (length message))
                                     (string/= "image" (gethash "type" (first nmsg))))
                                 (do-send-group-msg *napcat-websocket-client*
                                   `((:group-id . ,group-id)
                                     (:message . #(((:type . "text")
                                                    (:data . ((:text . "* 并非图片"))))))))
                                 (let ((file (gethash "file" (gethash "data" (first nmsg)))))
                                   (bb:chain
                                    (do-get-image *napcat-websocket-client*
                                      `((:file . ,file)))
                                    (:attach (data)
                                             (let ((downloaded (gethash "file" data)))
                                               (khst/save-and-add-to-list keyword downloaded)))
                                    (:finally (c)
                                              (bt2:signal-semaphore sema))))))))))
              (bt2:make-thread
               (lambda ()
                 (v:debug :khst "in thread ~a" (bt2:current-thread))
                 (on :message.group *napcat-websocket-client* cb)
                 (let ((msg (if (bt2:wait-on-semaphore sema :timeout 30)
                                (format nil "* 已收录~a~d" keyword
                                        (length (getf *khst-lists* keyword)))
                                "* 已超时，取消收录")))
                   (do-send-group-msg *napcat-websocket-client*
                     `((:group-id . ,group-id)
                       (:message . #(((:type . "text")
                                      (:data . ((:text . ,msg)))))))))
                 (remove-listener *napcat-websocket-client* :message.group cb)))
              :name "khst timeout daemon")))
        (when msg
          (do-send-msg *napcat-websocket-client*
            (list (a:switch (msg-type :test #'equal)
                    ("group" `(:group-id . ,group-id))
                    ("private" `(:user-id . ,user-id)))
                  `(:message-type . ,msg-type)
                  `(:message . #(((:type . "text")
                                  (:data . ((:text . ,msg)))))))))))
