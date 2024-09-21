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
(defvar *khst-history* (make-hash-table))
(defvar *khst-lists-path* nil)
(defvar *khst-history-path* nil)
(defvar *khst-pic-prefix* nil)

(defun khst/enable-khst ()
  (setf *khst-lists-path* (merge-pathnames "khst-lists" *prefix*))
  (setf *khst-history-path* (merge-pathnames "khst-history" *prefix*))
  (setf *khst-pic-prefix* (merge-pathnames "pics/" *prefix*))
  (ensure-directories-exist *khst-lists-path*)
  (ensure-directories-exist *khst-history-path*)
  (handler-case
      (with-open-file (s *khst-lists-path*
			 :direction :input
			 :if-does-not-exist :error)
	(setf *khst-lists* (a:plist-hash-table (read s) :test #'equal)))
    (error (c)
      (v:warn :khst "~a" c)))
  (handler-case
      (with-open-file (s *khst-history-path*
                         :direction :input
                         :if-does-not-exist :error)
        (setf *khst-history* (a:plist-hash-table (read s) :test #'equal)))
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
    (v:info :khst "Done."))
  (with-open-file (s *khst-history-path*
                     :direction :output
                     :if-exists :supersede)
    (v:info :khst "Saving khst history...")
    (unwind-protect (print (a:hash-table-plist *khst-history*) s))
    (v:info :khst "Done.")))

(defun khst/capture-keyword-and-respond-image (json)
  (let ((message (@ json "message")))
    (when (and (= 1 (length message))
               (string= "text" (@ (first message) "type")))
      (let* ((raw-msg (@ (first message) "data" "text"))
             (entries (@ *khst-lists* raw-msg)))
        (v:debug :khst "Seeking keywords ~a in: ~a" raw-msg entries)
        (when entries
          (let ((entry (elt entries (random (length entries)))))
            (v:info :khst "Found ~a for keyword \"~a\"" entry raw-msg)
            (bb:alet ((response (reply-to *napcat-websocket-client*
                                          json (make-message `(:image ,(str:concat "file://" entry)
                                                               :sub-type 1)))))
                     (let ((msg-id (@ response "message_id")))
                       (v:info :khst "Saving history line ~a:(~a ~a)" msg-id raw-msg entry)
                       (setf (@ *khst-history* msg-id)
                             (list raw-msg entry))))))))))

(defun khst/save-and-add-to-list (keyword picture)
  (let ((filename (merge-pathnames (file-namestring picture) *khst-pic-prefix*)))
    (unless (@ *khst-lists* keyword)
      (setf (@ *khst-lists* keyword) nil))
    (unless (find (namestring filename) (@ *khst-lists* keyword) :test #'equal)
      (push (namestring filename) (@ *khst-lists* keyword)))
    (ensure-directories-exist filename)
    (uiop:copy-file picture filename)))

(defun khst/save-remote-and-add-to-list (keyword picture uri)
  (let ((dest (merge-pathnames (file-namestring picture) *khst-pic-prefix*)))
    (unless (@ *khst-lists* keyword)
      (setf (@ *khst-lists* keyword) nil))
    (unless (find (namestring dest) (@ *khst-lists* keyword) :test #'equal)
      (push (namestring dest) (@ *khst-lists* keyword)))
    (ensure-directories-exist dest)
    (handler-case
        (dex:fetch uri dest)
      (file-error (c)
        (v:warn :khst "~a" c)))))

(defun khst/cmd-remove (json args &key reply at &allow-other-keys)
  (let ((msg-type (@ json "message_type"))
        (res nil))
    (if (string/= "group" msg-type)
        (setf res "本指令仅在群聊中可用")
        (if (or (null reply)
                (and at (/= at *self-id*))
                (/= 1 (length args)))
            (if (and (>= (length args) 2)
                     (string= "-rf" (second args))) ; It's an easter egg!
                (progn
                  (reply-to *napcat-websocket-client*
                            json (make-message `(:image ,*khst-are-you-sure*
                                                 :sub-type 1)))
                  (return-from khst/cmd-remove))
                (setf res "格式错误"))
            (let ((history-line (@ *khst-history* reply)))
              (if (null history-line)
                  (setf res "* 未找到看话说图记录。是否回复错误？")
                  (let* ((keyword (first history-line))
                         (entry (second history-line))
                         (entries (@ *khst-lists* keyword)))
                    (if (find entry entries :test #'equal)
                        (progn (setf (@ *khst-lists* keyword)
                                     (remove entry entries))
                               (setf res (s:fmt "* 已从关键词\"~a\"的条目中删除了该图片" keyword))
                               (v:info :khst "removed ~a from ~a:~a" entry keyword entries))
                        (setf res (s:fmt "* 该图片已不在关键词\"~a\"的条目中。是否已被删除？" keyword))))))))
    (reply-to *napcat-websocket-client*
              json (make-message res))))

(defun khst/cmd-khst (json args &key &allow-other-keys)
  (let* ((msg-type (@ json "message_type"))
         (message (@ json "message"))
         (group-id (@ json "group_id"))
         (user-id (@ json "user_id"))
         (res nil))
    (if (string/= "group" msg-type)
        (setf res "本指令仅在群聊中可用")
        (if (/= 2 (length args))
            (setf res "格式错误")
            (let ((keyword (elt args 1)))
              (if (or (char= #\/ (char keyword 0))
                      (char= #\. (char keyword 0)))
                  (setf res (s:fmt "关键词不可以~a开头" (char keyword 0)))
                   (let* ((sema (bt2:make-semaphore))
                          (cb (lambda (njson)
                                (let ((ngroup-id (@ njson "group_id"))
                                      (nuser-id (@ njson "user_id"))
                                      (nmsg (@ njson "message")))
                                  (when (and (= ngroup-id group-id) (= nuser-id user-id))
                                    (if (or (/= 1 (length message))
                                            (string/= "image" (@ (first nmsg) "type")))
                                        (reply-to *napcat-websocket-client*
                                                  json (make-message "* 并非图片"))
                                        (let ((file (@ (first nmsg) "data" "file"))
                                              (uri (@ (first nmsg) "data" "url")))
                                          (khst/save-remote-and-add-to-list keyword file uri)
                                          (bt2:signal-semaphore sema))))))))
                     (bt2:make-thread
                      (lambda ()
                        (v:debug :khst "in thread ~a" (bt2:current-thread))
                        (reply-to *napcat-websocket-client*
                                  json (make-message "* 等待添加图片中"))
                        (on :message.group *napcat-websocket-client* cb)
                        (let ((res (if (bt2:wait-on-semaphore sema :timeout 30)
                                       (s:fmt "* 已收录~a~d" keyword
                                               (length (@ *khst-lists* keyword)))
                                       "* 已超时，取消收录")))
                          (reply-to *napcat-websocket-client*
                                    json (make-message res)))
                        (v:debug :khst "removing cb ~a" cb)
                        (remove-listener *napcat-websocket-client* :message.group cb))
                      :name "khst timeout daemon"))))))
    (when res
      (reply-to *napcat-websocket-client*
                json (make-message res)))))
