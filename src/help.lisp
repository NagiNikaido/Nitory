;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; help.lisp ---- Help really helps.
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

(in-package :nitory)

(defun help/print-help-help ()
  (str:join #\Newline
	    `(,(s:fmt "Project-Nitory v~a by NagiNikaido" +version+)
	      ,(s:fmt "启动于 ~a" *startup-timestamp*)
	      ,@(loop for cmd in *commands*
		      unless (hidden cmd)
			collect (s:fmt ".~a: ~a" (display-name cmd)
				       (short-usage cmd)))
	      "更多功能开发中")))

(defun help/print-help-cmd (cmd)
  (let ((target (find-if (lambda (x)
			   (and (string= (display-name x) cmd)
				(not (hidden x))))
			 *commands*)))
    (if target
	(usage target)
	"未找到这条指令。是否输入错误？")))

(defun help/print-help (json cmd &key &allow-other-keys)
  (reply-to *napcat-websocket-client*
	    json (make-message
		  (if cmd
		      (help/print-help-cmd cmd)
		      (help/print-help-help)))))

(register-command
 (make-command
  :display-name "help"
  :short-usage "显示本帮助"
  :cmd-face "help"
  :options (list (make-option "cmd"
		  :optional t))
  :action #'help/print-help
  :usage
"帮助指令
.help [指令] 可查看对应指令的详细说明"))
