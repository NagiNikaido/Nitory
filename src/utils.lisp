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
(defparameter +version+ "0.0.1")
(setf json:*lisp-identifier-name-to-json* #'(lambda (sym)
                                              (string-downcase
                                               (substitute #\_ #\- sym))))
(setf *random-state* (make-random-state))
(setq re:*allow-named-registers* t)
(setq v:*process-locally* t)

(defvar *napcat-websocket-client* nil)
(defvar *startup-timestamp* nil)
(defvar *admin* nil)


(defun join (str list)
  (declare (inline join))
  (case (length list)
    (0 "")
    (1 (first list))
    (otherwise (reduce (lambda (x y) (concat x str y)) list))))

(defun concat (&rest strings)
  (declare (inline concat))
  (apply #'concatenate 'string strings))

(defun to-string (string-or-sym)
  (typecase string-or-sym
    (string string-or-sym)
    (symbol (write-to-string string-or-sym))
    (t (error (format nil "~a is no STRING nor SYMBOL!" string-or-sym)))))

(defun to-sym (string-or-sym)
  (typecase string-or-sym
    (string (read-from-string (substitute #\- #\_ string-or-sym)))
    (symbol string-or-sym)
    (t (error (format nil "~a is no STRING nor SYMBOL!" string-or-sym)))))

(defun strip-optional (keysym)
  (if (optional-p keysym)
      (to-sym
       (reverse (subseq (reverse (to-string keysym)) 1)))
      keysym))

(defun optional-p (keysym)
  (char= #\? (char (reverse (to-string keysym)) 0)))

(defun cur-decoded-timestamp ()
  (multiple-value-bind (second minute hour day month year _1 _2 tz)
      (get-decoded-time)
    (format nil "~d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d (GMT~@d)"
            year month day hour minute second (- tz))))
