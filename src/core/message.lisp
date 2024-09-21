;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; message.lisp ---- Message types & operations for OneBot 11 protocol.
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

(export-always 'alist-p)
(-> alist-p (t) boolean)
(defun alist-p (list)
  (trivial-types:association-list-p list))

(export-always 'alist)
(deftype alist ()
  `(satisfies alist-p))

(export-always 'alist-sim-p)
(-> alist-sim-p (alist alist) boolean)
(defun alist-sim-p (list model)
  (loop for (key . val) in model
        do (let ((as (assoc (strip-optional key) list)))
             (unless (or (and (optional-p key)
                              (null as))
                         (and as
                              (typecase val
                                (number (= val (cdr as)))
                                (string (equal val (cdr as)))
                                (symbol (typep (cdr as) val))
                                (function (funcall val (cdr as)))
                                (list (alist-sim-p (cdr as) val)))))
               (return)))
        finally (return t)))

(export-always 'eliminate-nil)
(-> eliminate-nil (list) list)
(defun eliminate-nil (alist)
  (loop for x in alist if x collect x))

(export-always 'segment-p)
(-> segment-p (alist) boolean)
(defun segment-p (seg)
  (or (text-segment-p seg)
      (image-segment-p seg)
      (at-segment-p seg)
      (reply-segment-p seg)))

(export-always 'make-text-segment)
(-> make-text-segment (string) alist)
(defun make-text-segment (text)
  `((:type . "text")
    (:data . ((:text . ,text)))))

(export-always 'text-segment-p)
(-> text-segment-p (t) boolean)
(defun text-segment-p (seg)
  (alist-sim-p seg '((:type . "text")
                     (:data . ((:text . string))))))

(export-always 'make-image-segment)
(-> make-image-segment (string &key (:thumb string)
                        (:name string)
                        (:url string)
                        (:summary string)
                        (:sub-type integer)) alist)
(defun make-image-segment (file &key thumb name url summary sub-type)
  `((:type . "image")
    (:data . ,(eliminate-nil
               `((:file . ,file)
                 ,(when thumb `(:thumb . ,thumb))
                 ,(when name `(:name . ,name))
                 ,(when url `(:url . ,url))
                 ,(when summary `(:summary . ,summary))
                 ,(when sub-type `(:sub-type . ,sub-type)))))))

(export-always 'image-segment-p)
(-> image-segment-p (t) boolean)
(defun image-segment-p (seg)
  (alist-sim-p seg `((:type . "image")
                     (:data . ((:file . string)
                               (:thumb? . string)
                               (:name? . string)
                               (:url? . string)
                               (:summary? . string)
                               (:sub-type? . ,(lambda (x) (or (= x 0) (= x 1)))))))))

(export-always 'make-at-segment)
(-> make-at-segment (number &key (:name string)) alist)
(defun make-at-segment (qq &key name)
  `((:type . "at")
    (:data . ,(eliminate-nil
               `((:qq . ,qq)
                 ,(when name `(:name . ,name)))))))

(export-always 'at-segment-p)
(-> at-segment-p (t) boolean)
(defun at-segment-p (seg)
  (alist-sim-p seg '((:type . "at")
                     (:data . ((:qq . number)
                               (:name? . string))))))

(export-always 'make-reply-segment)
(-> make-reply-segment (string) alist)
(defun make-reply-segment (id)
  `((:type . "reply")
    (:data . ((:id . ,id)))))

(export-always 'reply-segment-p)
(-> reply-segment-p (t) boolean)
(defun reply-segment-p (seg)
  (alist-sim-p seg '((:type . "reply")
                     (:data . ((:id . string))))))

(export-always 'make-segment)
(-> make-segment ((or string symbol) &rest t) alist)
(defun make-segment (key &rest rest)
  (if (typep key 'string)
      (make-text-segment key)
      (case key
        (:text (apply #'make-text-segment rest))
        (:image (apply #'make-image-segment rest))
        (:at (apply #'make-at-segment rest))
        (:reply (apply #'make-reply-segment rest))
        (otherwise (error "Unsupported message segment type!")))))

(export-always 'make-message)
(-> make-message (&rest t) array)
(defun make-message (&rest rest)
  (let ((res (loop for seg in rest
                    collect (if (atom seg)
                                (make-segment seg)
                                (apply #'make-segment seg)))))
    (make-array (list (length res))
                :initial-contents res)))

(export-always 'message-p)
(-> message-p (t) boolean)
(defun message-p (msg)
  (or (stringp msg)
      (every #'segment-p msg)))

(export-always 'message)
(deftype message ()
  `(satisfies message-p))

(export-always 'segment)
(deftype segment ()
  `(satisfies segment-p))
