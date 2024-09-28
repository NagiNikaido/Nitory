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
(defvar *khst-tags* nil)
(defvar *khst-pic-prefix* nil)

(defun khst/enable-khst ()
  (setf *khst-lists* (register-db "khst-lists"))
  (setf *khst-history* (register-db "khst-history"))
  (setf *khst-tags* (register-db "khst-tags"))
  (setf *khst-pic-prefix* (merge-pathnames "pics/" *prefix*))
  (on :message.group *napcat-websocket-client*
      #'khst/capture-keyword-and-respond-image))

(define-condition khst-error (error)
  ((error-type :initarg :error-type
               :initform nil
               :accessor error-type)
   (error-message :initarg :error-message
                  :initform nil
                  :accessor error-message)))

(defun khst/capture-keyword-and-respond-image (json)
  (let ((message (@ json "message")))
    (when (and (= 1 (length message))
               (string= "text" (@ (first message) "type")))
      (let* ((raw-msg (str:words (@ (first message) "data" "text")))
             (keyword (first raw-msg))
             (raw-tags (second raw-msg))
             (entries (db/@ *khst-lists* keyword)))
        (v:debug :khst "Seeking keywords ~a in: ~a" keyword entries)
        (when (and entries
                   (<= (length raw-msg) 2))
          (handler-case
              (progn
                (v:debug :khst "Splitting tags in: ~a" raw-tags)
                (when raw-tags
                  (setf entries (khst/%query-by-tags keyword (khst/%split-tags raw-tags))))
                (v:debug :khst "Found keywords ~a by tags ~a in: ~a" keyword entries raw-tags)
                (if (null entries)
                    (error 'khst-error :error-type :no-suitable-picture
                           :error-message "* 没有合适的图片……"))
                (let ((entry (elt entries (random (length entries)))))
                  (v:info :khst "Found ~a for keyword \"~a\"" entry raw-msg)
                  (bb:alet ((response (reply-to *napcat-websocket-client*
                                                json (make-message `(:image ,(str:concat "file://" entry)
                                                                     :sub-type 1)))))
                           (let ((msg-id (@ response "message_id")))
                             (v:info :khst "Saving history line ~a:(~a ~a)" msg-id keyword entry)
                             (setf (db/@ *khst-history* msg-id)
                                   (list keyword entry))))))
            (khst-error (c)
              (reply-to *napcat-websocket-client*
                        json (make-message (error-message c))))))))))

(defun khst/save-and-add-to-list (keyword picture)
  (let ((filename (merge-pathnames (file-namestring picture) *khst-pic-prefix*)))
    (unless (db/@ *khst-lists* keyword)
      (setf (db/@ *khst-lists* keyword) nil))
    (pushnew (namestring filename) (db/@ *khst-lists* keyword) :test #'equal)
    (ensure-directories-exist filename)
    (uiop:copy-file picture filename)
    (namestring filename)))

(defun khst/save-remote-and-add-to-list (keyword picture uri)
  (let ((dest (merge-pathnames (file-namestring picture) *khst-pic-prefix*)))
    (unless (db/@ *khst-lists* keyword)
      (setf (db/@ *khst-lists* keyword) nil))
    (pushnew (namestring dest) (db/@ *khst-lists* keyword) :test #'equal)
    (ensure-directories-exist dest)
    (handler-case
        (dex:fetch uri dest)
      (file-error (c)
        (v:warn :khst "~a" c)))
    (namestring dest)))

(defun khst/%probe-tag (tags pos)
  (loop for i from pos to (1- (length tags))
        if (find (elt tags i) "!./+-")
          return i
        finally (return (length tags))))

(defun khst/%split-tags (tags &key (pos 0))
  (if (>= pos (length tags))
      nil
      (let ((leading (elt tags pos)))
        (case leading
          (#\! (if (= 0 pos)
                   (cons :! (khst/%split-tags tags :pos (1+ pos)))
                   (error 'khst-error :error-type :tag-parsing
                          :error-message "* 清空符号 ! 不在标签序列的开头")))
          (#\+ (let ((nextpos (khst/%probe-tag tags (1+ pos))))
                 (when (= nextpos (1+ pos))
                   (error 'khst-error :error-type :tag-parsing
                          :error-message "* 标签格式有误"))
                 (cons :+ (cons (subseq tags (1+ pos) nextpos)
                                (khst/%split-tags tags :pos nextpos)))))
          (#\- (let ((nextpos (khst/%probe-tag tags (1+ pos))))
                 (when (= nextpos (1+ pos))
                   (error 'khst-error :error-type :tag-parsing
                          :error-message "* 标签格式有误"))
                 (cons :- (cons (subseq tags (1+ pos) nextpos)
                                (khst/%split-tags tags :pos nextpos)))))
          (t (error 'khst-error :error-type :tag-parsing
                    :error-message "* 标签格式有误"))))))

(defun khst/%clear-tags (keyword entry)
  (loop with entries
    for tag in (khst/%query-tags keyword entry)
        do (setf entries  (db/@ *khst-tags* keyword "tag" tag))
        do (setf (db/@ *khst-tags* keyword "tag" tag)
                 (remove entry entries :test #'equal)))
  (when (db/@ *khst-tags* keyword "pic")
    (remhash entry (db/@ *khst-tags* keyword "pic"))))

(defun khst/%insert-tag (keyword entry tag)
  (unless (db/@ *khst-tags* keyword "tag" tag)
    (setf (db/@ *khst-tags* keyword "tag" tag) nil))
  (pushnew entry (db/@ *khst-tags* keyword "tag" tag) :test #'equal)
  (unless (db/@ *khst-tags* keyword "pic" entry)
    (setf (db/@ *khst-tags* keyword "pic" entry) nil))
  (pushnew tag (db/@ *khst-tags* keyword "pic" entry) :test #'equal))

(defun khst/%remove-tag (keyword entry tag)
  (let ((tag-entries (db/@ *khst-tags* keyword "tag" tag))
        (pic-entries (db/@ *khst-tags* keyword "pic" entry)))
    (when (and tag-entries (find entry tag-entries :test #'equal))
      (setf (db/@ *khst-tags* keyword "tag" tag)
            (remove entry tag-entries :test #'equal)))
    (when (and pic-entries (find tag pic-entries :test #'equal))
      (setf (db/@ *khst-tags* keyword "pic" entry)
            (remove tag pic-entries :test #'equal)))))

(defun khst/%modify-tags (keyword entry tags)
  (when (eql (first tags) :!)
    (khst/%clear-tags keyword entry)
    (setf tags (rest tags)))
  (loop for (action tag) on tags by #'cddr
        do (case action
             (:+ (khst/%insert-tag keyword entry tag))
             (:- (khst/%remove-tag keyword entry tag)))))

(defun khst/%query-tags (keyword entry)
  (db/@ *khst-tags* keyword "pic" entry))

(defun khst/%query-by-tags (keyword tags)
  (loop with res = (db/@ *khst-lists* keyword)
        for (action tag) on tags by #'cddr
        until (null res)
        do (setf res
                 (case action
                   (:+ (intersection res (db/@ *khst-tags* keyword "tag" tag)
                                     :test #'string=))
                   (:- (set-difference res (db/@ *khst-tags* keyword "tag" tag)
                                       :test #'string=))
                   (t (error 'khst-error :error-type :tag-parsing
                             :error-message (s:fmt "* 标签格式有误：~a~a..." action tag)))))
        finally (return (sort res #'string<))))

(defun khst/cmd-tag (json tags &key reply at &allow-other-keys)
  (let ((res nil))
    (if (null reply)
        (setf res "格式错误")
        (let ((history-line (db/@ *khst-history* reply)))
          (if (null history-line)
              (setf res "* 未找到看话说图记录。是否回复错误？")
              (handler-case
                  (let* ((keyword (first history-line))
                         (entry (second history-line))
                         (entries (db/@ *khst-lists* keyword)))
                    (if (find entry entries :test #'equal)
                        (progn
                          (when tags
                            (khst/%modify-tags keyword entry (khst/%split-tags tags)))
                          (setf res (s:fmt "* 现有标签为：~{~a~^，~}" (khst/%query-tags keyword entry))))
                        (setf res (s:fmt "* 该图片已不在关键词\"~a\"的条目中。是否已被删除？" keyword))))
                (khst-error (c)
                  (setf res (error-message c)))))))
    (reply-to *napcat-websocket-client*
              json (make-message res))))

(defun khst/cmd-remove (json rf &key reply at &allow-other-keys)
  (let ((res nil))
    (if rf ; It's an easter egg!
        (setf res `(:image ,*khst-are-you-sure*
                    :sub-type 1))
        (if (null reply)
            (setf res "格式错误")
            (let ((history-line (db/@ *khst-history* reply)))
              (if (null history-line)
                  (setf res "* 未找到看话说图记录。是否回复错误？")
                  (let* ((keyword (first history-line))
                         (entry (second history-line))
                         (entries (db/@ *khst-lists* keyword)))
                    (if (find entry entries :test #'equal)
                        (progn (setf (db/@ *khst-lists* keyword)
                                     (remove entry entries :test #'equal))
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
                           (setf (db/@ *khst-history* (@ njson "message_id"))
                             (list keyword (khst/save-remote-and-add-to-list keyword file uri)))
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
 (make-command :display-name "tag"
               :hidden nil
               :msg-type :group
               :short-usage "修改或查看看话说图条目的标签"
               :cmd-face "tag"
               :options (list (make-option
                               "tags"
                               :optional t))
               :action #'khst/cmd-tag
               :usage
"修改或查看看话说图条目的标签
选中图片回复 .tag 即可查看该图现有标签
回复 .tag [+/-标签] 即可添加或删除对应标签
回复 .tag ! 即可清空该图现有标签
回复 .tag ![+/-标签] 即可清空现有标签，并添加新的标签
可同时增减多枚标签，如
  .tag +美味-搞笑        为图片添加\"美味\"标签，并删除\"搞笑\"标签
  .tag !+美味+搞笑-音乐   清空现有标签，并为图片添加\"美味\"和\"搞笑\"标签，并删除\"音乐\"标签（由于已被清空，因此该删除操作并无实际效果）"))

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
添加完成后，再次输入关键词，bot 便会从已添加的所有图片中随机选取一张发出。
如果在关键词之后附加标签，则会在符合标签的图片中随机选取一张发出，如：
（以下用>表示信息从用户处发出，用<表示信息从bot处发出）
  1> .khst test
  2> [图片]
  3< (添加成功信息)
  4> [引用信息2] .tag +good
  5< (添加成功信息)
  6> test +good
  7< [信息2中图片]
  8> test -good
  9< * 没有合适的图片……"))
