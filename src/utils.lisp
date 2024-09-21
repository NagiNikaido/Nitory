;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; utils.lisp ---- brief
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

;; managed by version bumper
(defparameter +version+ "0.0.8")
(setf *random-state* (make-random-state t))
(setf re:*allow-named-registers* t)
(setf v:*process-locally* t)

(defvar *napcat-websocket-client* nil)
(defvar *timestring-format* '(:year #\/ (:month 2) #\/ (:day 2) #\  (:hour 2) #\: (:min 2) #\: (:sec 2) " GMT" :gmt-offset))
(defvar *startup-timestamp* nil)
(defvar *admin* nil)
(defvar *prefix* "/opt/nitory/")
(defvar *self-id* nil)

(-> to-string ((or string symbol)) string)
(defun to-string (string-or-sym)
  (typecase string-or-sym
    (string string-or-sym)
    (symbol (write-to-string string-or-sym))))

(-> to-sym ((or string symbol)) symbol)
(defun to-sym (string-or-sym)
  (typecase string-or-sym
    (string (read-from-string (substitute #\- #\_ string-or-sym)))
    (symbol string-or-sym)))

(-> strip-optional ((or string symbol)) symbol)
(defun strip-optional (keysym)
  "Strip the trilling ? if keysym is an optional key."
  (if (optional-p keysym)
      (to-sym
       (remove #\? (to-string keysym) :from-end t :count 1))
      keysym))

(-> bool-value (t) boolean)
(defun bool-value (x)
  (not (null x)))

(-> optional-p ((or string symbol)) boolean)
(defun optional-p (keysym)
  "Check whether KEYSYM is an optional key. It should be either end with ? or ?|."
  (let* ((str (to-string keysym))
         (len (length str)))
    (bool-value (find #\? (reverse str) :end (min 2 len)))))

(-> keysym-name-to-json ((or string symbol)) string)
(defun keysym-name-to-json (keysym)
  "Translating keysym NAME into json keys. Noticing that cl-json gives us (symbol-name keysym)
which strips ||, we have to determine whether to convert name into underline style or just to
preserve the current style by checking if all characters of NAME are uppercase."
  (let ((name (if (symbolp keysym)
                  (symbol-name keysym)
                  keysym)))
   (if (str:upcasep name)
      (str:downcase (substitute #\_ #\- name))
      name)))

(setf j:*list-encoder* #'j:encode-alist)
(setf j:*symbol-key-encoder* #'keysym-name-to-json)

(defun encode-to-json-string (alist-or-dict)
  "Wrapping yason:encode with a string buffer. Therefore, we no more need the old cl-json
library ...?"
  (with-output-to-string (string-buffer)
    (j:encode alist-or-dict string-buffer)
    string-buffer))

(defun current-decoded-timestamp ()
  (local-time:format-timestring nil (local-time:now) :format *timestring-format*))
