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

(defvar *khst-lists* nil)
(defvar *khst-history* nil)
(defvar *khst-pic-prefix* nil)

(defun khst/enable-khst ()
  (setf *khst-lists* (register-db "khst-lists"))
  (setf *khst-history* (register-db "khst-history"))
  (setf *khst-pic-prefix* (merge-pathnames "pics/" *prefix*))
  (on :message.group *napcat-websocket-client*
      #'khst/capture-keyword-and-respond-image))

(defun khst/capture-keyword-and-respond-image (json)
  (let ((message (@ json "message")))
    (when (and (= 1 (length message))
               (string= "text" (@ (first message) "type")))
      (let* ((raw-msg (@ (first message) "data" "text"))
             (entries (db/@ *khst-lists* raw-msg)))
        (v:debug :khst "Seeking keywords ~a in: ~a" raw-msg entries)
        (when entries
          (let ((entry (elt entries (random (length entries)))))
            (v:info :khst "Found ~a for keyword \"~a\"" entry raw-msg)
            (bb:alet ((response (reply-to *napcat-websocket-client*
                                          json (make-message `(:image ,(str:concat "file://" entry)
                                                               :sub-type 1)))))
                     (let ((msg-id (@ response "message_id")))
                       (v:info :khst "Saving history line ~a:(~a ~a)" msg-id raw-msg entry)
                       (setf (db/@ *khst-history* msg-id)
                             (list raw-msg entry))))))))))

(defun khst/save-and-add-to-list (keyword picture)
  (let ((filename (merge-pathnames (file-namestring picture) *khst-pic-prefix*)))
    (unless (db/@ *khst-lists* keyword)
      (setf (db/@ *khst-lists* keyword) nil))
    (unless (find (namestring filename) (db/@ *khst-lists* keyword) :test #'equal)
      (push (namestring filename) (db/@ *khst-lists* keyword)))
    (ensure-directories-exist filename)
    (uiop:copy-file picture filename)))

(defun khst/save-remote-and-add-to-list (keyword picture uri)
  (let ((dest (merge-pathnames (file-namestring picture) *khst-pic-prefix*)))
    (unless (db/@ *khst-lists* keyword)
      (setf (db/@ *khst-lists* keyword) nil))
    (unless (find (namestring dest) (db/@ *khst-lists* keyword) :test #'equal)
      (push (namestring dest) (db/@ *khst-lists* keyword)))
    (ensure-directories-exist dest)
    (handler-case
        (dex:fetch uri dest)
      (file-error (c)
        (v:warn :khst "~a" c)))))

(defun khst/cmd-remove (json rf &key reply at &allow-other-keys)
  (let ((res nil))
    (if rf ; It's an easter egg!
        (setf res `(:image ,*khst-are-you-sure*
                    :sub-type 1))
        (if (or (null reply)
                (and at (/= at *self-id*)))
            (setf res "格式错误")
            (let ((history-line (db/@ *khst-history* reply)))
              (if (null history-line)
                  (setf res "* 未找到看话说图记录。是否回复错误？")
                  (let* ((keyword (first history-line))
                         (entry (second history-line))
                         (entries (db/@ *khst-lists* keyword)))
                    (if (find entry entries :test #'equal)
                        (progn (setf (db/@ *khst-lists* keyword)
                                     (remove entry entries))
                               (setf res (s:fmt "* 已从关键词\"~a\"的条目中删除了该图片" keyword))
                               (v:info :khst "removed ~a from ~a:~a" entry keyword entries))
                        (setf res (s:fmt "* 该图片已不在关键词\"~a\"的条目中。是否已被删除？" keyword))))))))
        (reply-to *napcat-websocket-client*
                  json (make-message res))))

(defun khst/cmd-khst (json keyword &key &allow-other-keys)
  (let* ((message (@ json "message"))
         (group-id (@ json "group_id"))
         (user-id (@ json "user_id")))
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
                               (length (db/@ *khst-lists* keyword)))
                        "* 已超时，取消收录")))
           (reply-to *napcat-websocket-client*
                     json (make-message res)))
         (v:debug :khst "removing cb ~a" cb)
         (remove-listener *napcat-websocket-client* :message.group cb))
       :name "khst timeout daemon"))))

(register-command
 (make-command :display-name "rm"
               :hidden nil
               :msg-type :group
               :short-usage "删除看话说图条目"
               :cmd-face "rm"
               :options (list (make-option
                               "rf"
                               :predicator
                               (lambda (opt)
                                 (string= "-rf" opt))
                               :optional t))
               :action #'khst/cmd-remove
               :usage
"删除看话说图条目
选中 bot 发出的图回复 .rm 即可将该图从对应关键词中删除"))

(register-command
 (make-command :display-name "khst"
               :hidden nil
               :msg-type :group
               :short-usage "看话说图"
               :cmd-face "khst"
               :options (list (make-option
                               "keyword"
                               :predicator
                               (lambda (opt)
                                 (when (command-string-p opt)
                                     (error 'command-parse-error
                                            :error-type :wrong-argument
                                            :error-message "关键词不可以.或/开头"))
                                 t)))
               :action #'khst/cmd-khst
               :usage
"看话说图
.khst [关键词]  为关键词添加随机图片项
输入指令后 bot 会进入交互模式，等待输入指令者发出图片。
交互过程最长为30秒，超时会中断交互。
添加完成后，再次输入关键词，bot 便会从已添加的所有图片中随机选取一张发出。"))
