;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; napcat-types.lisp ---- Auxiliary types for napcat.
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

(defun alist-p (list model)
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
                                (list (alist-p (cdr as) val)))))
               (return)))
        finally (return t)))

(defun segment-p (seg)
  (or (alist-p seg '((:type . "text")
                     (:data . ((:text . string)))))
      (alist-p seg `((:type . "image")
                     (:data . ((:file . string)
                               (:thumb? . string)
                               (:name? . string)
                               (:url? . string)
                               (:summary? . string)
                               (:subtype? . ,(lambda (x) (or (= x 0) (= x 1))))))))
      (alist-p seg '((:type . "at")
                     (:data . ((:qq . number)
                               (:name? . string)))))
      (alist-p seg '((:type . "reply")
                     (:data . ((:id . string)))))))

(defun message-p (msg)
  (or (stringp msg)
      (every #'segment-p msg)))

(deftype message ()
  `(satisfies message-p))
