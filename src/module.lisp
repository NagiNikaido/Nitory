;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; module.lisp ---- Chatbot module manipulation.
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

(defvar *nitory-modules* (make-hash-table :test #'equal))

(defmacro defmodule (name command entrypoint
		     &key
		       (short-help "")
		       (long-help)
		       enable-cb
		       disable-cb)
  (let ((entry (gensym))
	(enabler (to-sym (concat "module/enable-" name)))
	(disabler (to-sym (concat "module/disable-" name))))
    `(let ((,entry ,entrypoint))
       (defun ,enabler ()
	 ,(when enable-cb
	    `(funcall ,enable-cb)))
       (defun ,disabler ()
	 ,(when disable-cb
	    `(funcall ,disable-cb)))
       (setf (gethash *nitory-modules ,name)
	     (list :command ,command
		   :entrypoint ,entry	   
		   :short-help ,short-help
		   :long-help ,(or long-help
				   short-help)
		   :enablefn #',enabler
		   :disablefn #',disabler
		   :enabled nil)))))

(defun module/enable (name)
  (let ((entry (gethash name *nitory-modules*)))
    (when (and entry
	       (not (getf entry :enabled)))
      (funcall (getf entry :enablefn))
      (setf (getf entry :enabled) t))))

(defun module/disable (name)
  (let ((entry (gethash name *nitory-modules*)))
    (when (and entry
	       (getf entry :enabled))
      (funcall (getf entry :disablefn))
      (setf (getf entry :enabled) nil))))

