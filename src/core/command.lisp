;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; command.lisp ---- Definition for commands and options for the chatbot.
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

(export-always 'option)
(export-always 'make-option)
(defstruct (option (:constructor make-option (name &key predicator optional)))
  name predicator optional)

(export-always 'command)
(defclass command ()
  ((display-name
    :initarg :display-name
    :accessor display-name
    :documentation
    "The display name presented in help list.")
   (hidden
    :initarg :hidden
    :accessor hidden
    :documentation
    "Whether to hide this command from help list. Usually used to make
aliases/specialized commands.")
   (short-usage
    :initarg :short-usage
    :accessor short-usage
    :documentation
    "A brief description of this command in a single line.")
   (usage
    :initarg :usage
    :accessor usage
    :documentation
    "A comprehensive description of this command.")
   (cmd-face
    :initarg :cmd-face
    :accessor cmd-face
    :documentation
    "Either a string to be found or a function which accepts a string and parses it into
the command plus extra options.")
   (options
    :initarg :options
    :accessor options
    :documentation
    "A list of options. Parsed from head to tail.")
   (action
    :initarg :action
    :accessor action
    :documentation
    "Target action of this command.")
   (msg-type
    :initarg :msg-type
    :accessor msg-type
    :documentation
    "To which type this command responds. MSG-TYPE can be either NIL, :PRIVATE or :GROUP.")))

(export-always 'make-command)
(-> make-command (&key (:display-name string)
                       (:hidden boolean)
                       (:short-usage string)
                       (:usage string)
                       (:cmd-face (or string function))
                       (:options list)
                       (:action function)
                       (:msg-type symbol)) command)
(defun make-command (&key display-name hidden short-usage usage cmd-face options action msg-type)
  (make-instance 'command
                 :display-name display-name
                 :hidden hidden
                 :short-usage short-usage
                 :usage usage
                 :cmd-face cmd-face
                 :options options
                 :action action
                 :msg-type msg-type))

(export-always 'error-message)
(export-always 'error-type)
(export-always 'command-parse-error)
(define-condition command-parse-error (error)
  ((error-type :initarg :error-type
               :initform nil
               :accessor error-type)
   (error-message :initarg :error-message
                  :initform nil
                  :accessor error-message)))

(export-always 'parse-command)
(defmethod parse-command ((command-instance command) json raw-args &rest supplemental-args)
  (let* ((cmd (first raw-args))
         (args nil)
         (kwargs nil)
         (cmd-face (cmd-face command-instance)))
    (when (typecase cmd-face
            (string (string= cmd-face cmd))
            (function (multiple-value-bind (c ar kw)
                          (funcall cmd-face cmd args kwargs)
                        (when c
                          (setf args ar)
                          (setf kwargs kw)
                          t)))) ; We've found the corresponding command. Now we are going to parse the options.
      (when (and (msg-type command-instance)
                 (string-not-equal (symbol-name (msg-type command-instance))
                                   (@ json "message_type")))
        (error 'command-parse-error :error-type :wrong-msg-type
               :error-message (case (msg-type command-instance)
                                (:group "本指令仅可在群聊中使用")
                                (:private "本指令仅可在私聊中使用")
                                (t nil))))
      (loop with remaining = (rest raw-args)
            for opt in (options command-instance)
            if (eq opt :rest)
              do (setf args (append args remaining))
              and return nil
            end
            if (or (and (null (option-predicator opt))
                        (first remaining))
                   (and (option-predicator opt)
                        (funcall (option-predicator opt) (first remaining))))
              do (s:push-end (first remaining) args)
              and do (setf remaining (rest remaining))
            else
              if (option-optional opt)
                do (s:push-end nil args)
              else
                do (error 'command-parse-error :error-type :wrong-argument
                          :error-message "格式错误")
              end
            end
            finally
               (when remaining
                 (error 'command-parse-error :error-type :unparsed-arguments
                        :error-message "格式错误，参数量过多")))
      (apply (action command-instance) json (append args kwargs supplemental-args))
      t)))

(-> command-leading-p (character) boolean)
(defun command-leading-p (leading)
  (or (char= #\/ leading)
      (char= #\. leading)))

(-> command-string-p (string) boolean)
(defun command-string-p (str)
  (command-leading-p (char str 0)))

(defparameter *commands* nil)

(export-always 'register-command)
(defmacro register-command (command-instance)
  `(eval-when (:load-toplevel
               :execute)
     (push ,command-instance *commands*)))
