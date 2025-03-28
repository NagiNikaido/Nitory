;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; email.lisp ---- Sending email to administrator.
;;;
;;; Copyright (C) 2025  NagiNikaido <naginikaido@kuusouhakuchuu.cn>
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

(defun send-email (subject message)
  (if (and *admin-email-address*
	   *nitory-email-address*
	   *nitory-email-server*)
      (cl-smtp:send-email *nitory-email-server*
			  *nitory-email-address*
			  *admin-email-address*
			  (s:fmt "[Nitory] ~a" subject)
			  message
			  :ssl *nitory-email-ssl*
			  :authentication `(:login ,*nitory-email-address*
						   ,*nitory-email-password*))
      (v:warn :email "Mailing settings are not complete, while SEND-EMAIL method is called.")))
