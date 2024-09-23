;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; command-test.lisp ---- Test cases for commands.
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

(in-package #:nitory/test)

(deftest simple-commands
  (ok (let ((c (nitory:make-command
		:cmd-face "c"
		:action (lambda (&rest body) nil))))
	(nitory:parse-command c nil '("c"))))
  (ng (let ((d (nitory:make-command
		:cmd-face "d"
		:action (lambda (&rest body) nil))))
	(nitory:parse-command d nil '("c")))))

(deftest msg-type
  (let ((c (nitory:make-command
	    :cmd-face "c"
	    :msg-type :private
	    :action (lambda (&rest body) nil))))
    (ok (nitory:parse-command c (s:dict "message_type" "private")
			      '("c")))
    (ok (signals
	    (nitory:parse-command c (s:dict "message_type" "group")
				  '("c"))
	    'nitory:command-parse-error))))

(deftest simple-options
  (let ((c (nitory:make-command
	    :cmd-face "c"
	    :options
	    (list
	     (nitory:make-option "a")
	     (nitory:make-option "b"))
	    :action (lambda (json a b)
		      (format t "~a ~a" a b)))))
    (ok (outputs (nitory:parse-command c nil '("c" "a" "b"))
	"a b"))
    (ok (signals
	    (nitory:parse-command c nil '("c" "a"))
	    'nitory:command-parse-error))
    (ok (signals
	    (nitory:parse-command c nil '("c" "a" "b" "d"))
	    'nitory:command-parse-error)))
  (let ((c (nitory:make-command
	    :cmd-face "c"
	    :options
	    (list
	     (nitory:make-option "a" :optional t)
	     (nitory:make-option "b" :optional t))
	    :action (lambda (json a b)
		      (format t "~a ~a" a b)))))
    (ok (outputs (nitory:parse-command c nil '("c" "a" "b"))
	    "a b"))
    (ok (outputs (nitory:parse-command c nil '("c" "a"))
	    "a NIL"))
    (ok (outputs (nitory:parse-command c nil '("c"))
	    "NIL NIL"))
    (ok (signals
	    (nitory:parse-command c nil '("c" "a" "b" "d"))
	    'nitory:command-parse-error))))

(deftest option-predicator
  (let ((c (nitory:make-command
	    :cmd-face "c"
	    :options
	    (list
	     (nitory:make-option
	      "a"
	      :predicator (lambda (x) (re:scan "a\\d*" x))
	      :optional t)
	     (nitory:make-option
	      "b"
	      :predicator (lambda (x) (string= x "b"))
	      :optional t))
	    :action (lambda (json a b)
		      (format t "~a ~a" a b)))))
    (ok (outputs (nitory:parse-command c nil '("c" "a" "b"))
	    "a b"))
    (ok (outputs (nitory:parse-command c nil '("c" "b"))
	    "NIL b"))
    (ok (outputs (nitory:parse-command c nil '("c" "a123"))
	    "a123 NIL"))
    (ok (signals
	    (nitory:parse-command c nil '("c" "b" "a"))
	    'nitory:command-parse-error))))

(deftest cmd-face
  (let ((c (nitory:make-command
	    :cmd-face
	    (lambda (cmd args kwargs)
	      (multiple-value-bind (match arguments)
		  (re:scan-to-strings "^r((\\d+)|([+-]\\d+))" cmd)
		(when match
		  (s:push-end (s:fmt "~ad~a"
				     (or (elt arguments 1) "")
				     (or (elt arguments 2) ""))
			      args)
		  (values t args kwargs))))
	    :options (list (nitory:make-option "desc" :optional t))
	    :action (lambda (json expr desc)
		      (format t "~a ~a" expr desc)))))
    (ok (outputs (nitory:parse-command c nil '("r+1"))
	    "d+1 NIL"))
    (ok (outputs (nitory:parse-command c nil '("r4" "test"))
	    "4d test"))
    (ng (nitory:parse-command c nil '("rd")))
    (ng (nitory:parse-command c nil '("r")))
    (ok (signals
	    (nitory:parse-command c nil '("r4" "a" "b"))
	    'nitory:command-parse-error)))
  (defun for-test (json &key private &allow-other-keys)
    (format t "~a ~a" json private))
  (let ((c (nitory:make-command
	    :cmd-face "rh"
	    :action (lambda (&rest rest)
		      (apply #'for-test (append rest (list :private t)))))))
    (ok (outputs (nitory:parse-command c nil '("rh"))
	    "NIL T"))))

(deftest supplemental-args
  (let ((c (nitory:make-command
	    :cmd-face "c"
	    :options nil
	    :action (lambda (json &key at-id reply-id)
		      (format t "~a ~a" at-id reply-id)))))
    (ok (outputs (nitory:parse-command c nil '("c") :at-id 123 :reply-id 456)
	    "123 456"))
    (ok (outputs (nitory:parse-command c nil '("c") :at-id 123)
	    "123 NIL"))
    (ok (outputs (nitory:parse-command c nil '("c") :reply-id 456)
	    "NIL 456"))
    (ok (signals
	    (nitory:parse-command c nil '("c" "d"))
	    'nitory:command-parse-error))))
