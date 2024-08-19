;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; nitory.lisp ---- A Multiple Purposed Chatbot based on OneBot framework. 
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

(setq re:*allow-named-registers* t)

(defun main ()
  (format t "Hello from nitory.~%")
  (let ((client (wsd:make-client "ws://localhost:3001/")))
    (defun sender/send-message (message)
      (format t "~A~%" message)
      (wsd:send-text client message))
    (wsd:start-connection client)
    (wsd:on :message client
            (lambda (message)
              (event/receive-event message)))
    (loop
      (sleep 100))))


(defun sender/build-response (action params)
  (json:encode-json-alist-to-string `((:action . ,action)
                                      (:params . ,params))))

(defun event/receive-event (message)
  (format t "~A~%" message)
  (let* ((json (yason:parse message))
         (event-type (a:ensure-gethash "post_type" json nil)))
    (format t "Event-type:~A~%" event-type)
    (a:switch (event-type :test #'equal)
              ("message" (event/receive-message message))
              ("notice" (event/receive-notice message))
              ("request" (event/receive-request message))
              ("meta_event" (event/receive-meta-event message))
              (otherwise (format nil "G!~%")))))

(defun event/receive-message (message)
  (let* ((json (yason:parse message))
         (message-type (a:ensure-gethash "message_type" json nil))
         (sub-type (a:ensure-gethash "sub_type" json nil))
         (user-id (a:ensure-gethash "user_id" json nil))
         (group-id (a:ensure-gethash "group_id" json nil))
         (msg (a:ensure-gethash "raw_message" json nil))
         (sender (a:ensure-gethash "sender" json nil))
         (ignore nil))
    (format t "~a~%" msg)
    (if (not ignore)
        (let ((rmsg (nitory/parse-and-respond sender msg)))
          (format t "~a~%" rmsg)
          (if rmsg
              (a:switch (message-type :test #'equal)
                        ("private" (respond/do-send-private-message user-id rmsg))
                        ("group" (respond/do-send-group-message group-id rmsg))))))))

(defun nitory/parse-and-respond (sender message)
  (let ((leading (char message 0))
        (rdmsg (concatenate 'string message " ")))
    (if (or (char= #\/ leading)
            (char= #\. leading))
        (let* ((pos (position #\space rdmsg))
               (command (subseq rdmsg 1 pos))
               (rest (string-trim " " (subseq rdmsg (+ 1 pos)))))
          (a:switch (command :test #'equal)
                    ("help" (nitory/print-help rest))
                    ("r" (nitory/roll-dice rest sender))
                    (otherwise (nitory/command-not-supported)))))))

(defun nitory/print-help (rest)
  "Project-Nitory v0.0.1 by NagiNikaido
指令列表:
.r: 掷骰指令（默认d20）
.help: 显示本帮助
更多功能开发中")

(defun nitory/dice-expr-leading-p (leading)
  (or (nitory/dice-expr-legal-leading-p leading)
      (char= #\) leading)
      (char= #\+ leading)
      (char= #\- leading)
      (char= #\* leading)
      (char= #\/ leading)
      (char= #\# leading)))

(defun nitory/dice-expr-legal-leading-p (leading)
  (or (digit-char-p leading)
      (char= #\d leading)
      (char= #\h leading)
      (char= #\l leading)
      (char= #\( leading)))

(defun nitory/dice-expr-legal-operator-p (leading)
  (or (char= #\+ leading)
      (char= #\- leading)))

(defun nitory/dice-term-legal-operator-p (leading)
  (or (char= #\* leading)
      (char= #\/ leading)))

(defun nitory/dice-expr-op-p (op opcat)
  (if (eq opcat :expr)
      (or (eq op :+) (eq op :-))
      (if (eq opcat :term)
          (or (eq op :*) (eq op :/))
          nil)))

(defun nitory/rotate-fix- (tree opcat)
  (if (and (nitory/dice-expr-op-p (car tree) opcat)
           (nitory/dice-expr-op-p (caaddr tree) opcat))
      (nitory/rotate-fix- `(,(caaddr tree)
                            (,(car tree) ,(cadr tree) ,(car (cdaddr tree)))
                            ,@(cdr (cdaddr tree))) opcat)
      tree))

(defun nitory/parse-dice-cell (cell)
  (if (= 0 (length cell))
      '((:error) 0)
      (let ((leading (char cell 0)))
        (if (not (nitory/dice-expr-legal-leading-p leading))
            '((:error) 0)
            (if (char= #\( leading)
                (let* ((rp (nitory/parse-dice-expr (subseq cell 1)))
                       (res (car rp))
                       (pos (cadr rp)))
                  (if (or (eq :error (car res))
                          (>= (+ pos 1) (length cell))
                          (char/= #\) (char cell (+ pos 1))))
                      '((:error) 0)
                      `((:braket ,res) ,(+ pos 2))))
                (multiple-value-bind (match regs)
                    (re:scan-to-strings "^(((?<dice>\\d+)?d(?<face>\\d+)?(h(?<high>\\d+)|l(?<low>\\d+))?)|(?<const>\\d+))" cell)
                  (if match
                      ;; regs: #(whole dice-cell dice face hl high low const)
                      (multiple-value-bind (whole dice-cell dice face hl high low const)
                          (values-list (mapcar
                                        (lambda (x)
                                          (ignore-errors
                                           (parse-integer x)))
                                        (coerce regs 'list)))
                        (if const
                            `((:const ,const) ,(length (elt regs 0)))
                            (if (and (>= (if dice dice 1)
                                         (if high high 1))
                                     (>= (if dice dice 1)
                                         (if low low 1)))
                                `((:roll ,@(if dice `(:dice ,dice) '())
                                         ,@(if face `(:face ,face) '())
                                         ,@(if high `(:high ,high) '())
                                         ,@(if low  `(:low ,low) '())) ,(length (elt regs 0)))
                                '((:error) 0))))
                      '((:error) 0))))))))

(defun nitory/parse-dice-term (term)
  (let* ((rp (nitory/parse-dice-term- term))
         (res (car rp))
         (pos (cadr rp)))
    `(,(nitory/rotate-fix- res :term) ,pos)))

(defun nitory/parse-dice-expr (expr)
  (let* ((rp (nitory/parse-dice-expr- expr))
         (res (car rp))
         (pos (cadr rp)))
    `(,(nitory/rotate-fix- res :expr) ,pos)))

(defun nitory/parse-dice-term- (term)
  (let* ((rp1 (nitory/parse-dice-cell term))
         (res1 (car rp1))
         (pos1 (cadr rp1)))
    (if (eq :error (car res1))
        '((:error) 0)
        (if (>= pos1 (length term))
            rp1
            (let ((op (char term pos1)))
              (if (or (nitory/dice-expr-legal-operator-p op)
                      (char= #\) op))
                  rp1
                  (if (nitory/dice-term-legal-operator-p op)
                      (let* ((rp2 (nitory/parse-dice-term- (subseq term (+ pos1 1))))
                             (res2 (car rp2))
                             (pos2 (cadr rp2)))
                        (if (eq :error (car res2))
                            '((:error) 0)
                            `((,(if (char= #\* op)
                                    :* :/) ,res1 ,res2) ,(+ pos1 1 pos2))))
                      '((:error) 0))))))))

(defun nitory/parse-dice-expr- (expr)
  (let* ((rp1 (nitory/parse-dice-term expr))
         (res1 (car rp1))
         (pos1 (cadr rp1)))
    (if (eq :error (car res1))
        '((:error) 0)
        (if (>= pos1 (length expr))
            rp1
            (let ((op (char expr pos1)))
              (if (char= #\) op)
                  rp1
                  (if (nitory/dice-expr-legal-operator-p op)
                      (let* ((rp2 (nitory/parse-dice-expr- (subseq expr (+ pos1 1))))
                             (res2 (car rp2))
                             (pos2 (cadr rp2)))
                        (if (eq :error (car rp2))
                            '((:error) 0)
                            `((,(if (char= #\+ op)
                                    :+ :-) ,res1 ,res2) ,(+ pos1 1 pos2))))
                      '((:error) 0))))))))

(defun nitory/parse-dice-expr-init (expr)
  (let ((rp (nitory/parse-dice-expr
             (if (= 0 (length expr))
                 "d"
                 expr))))
    rp))

(defun nitory/parse-dice-full-expr (expr)
  (multiple-value-bind (match regs)
      (re:scan-to-strings "^(?<repeat>\\d+)#" expr)
    (if (/= (count #\( expr)
            (count #\) expr))
        '(:error)
        (let* ((single-expr (subseq expr (length match)))
               (post (nitory/parse-dice-expr-init single-expr)))
          (if (eq :error (caar post))
              '(:error)
              `(:repeat  ,(if regs (parse-integer (elt regs 0)) 1) (,expr ,single-expr) ,(car post)))))))

(defun nitory/join (str list)
  (reduce (lambda (x y)
            (concatenate 'string x str y)) list))

(defmacro nitory/concat (&rest body)
  `(concatenate 'string ,@body))

(defun nitory/generate-dices (&key (dice 1) (face 20) (high nil) (low nil) &allow-other-keys)
  (defun nitory/dice-pretty-concat (list kth)
    (nitory/concat "{"
                   (nitory/join ","
                                (loop for d in list
                                      for i = 1 then (incf i)
                                      collect (concatenate 'string (if (= i 1) "[" "")
                                                           (write-to-string d)
                                                           (if (= i kth) "]" ""))))
                   "}"))
  (let ((rolled (loop repeat dice collect (+ 1 (random face)))))
    (if high
        (let ((sorted (sort rolled #'>)))
          `(,(nitory/dice-pretty-concat sorted high)
            ,(reduce #'+ (subseq sorted 0 high))))
        (if low
            (let ((sorted (sort rolled #'<)))
              `(,(nitory/dice-pretty-concat sorted low)
                ,(reduce #'+ (subseq sorted 0 low))))
            `(,(nitory/concat "{"
                              (nitory/join "," (mapcar #'write-to-string rolled))
                              "}")
              ,(reduce #'+ rolled))))))

(defun nitory/exec-dice-tree (tree)
  (defun nitory/binary-op-- (op)
    `,(let ((f (nitory/exec-dice-tree (cadr tree)))
            (g (nitory/exec-dice-tree (caddr tree))))
        `,(mapcar #'eval
                  `((nitory/concat ,(car f) ,(symbol-name op) ,(car g))
                    (,op ,(cadr f) ,(cadr g))))))
  (case (car tree)
    (:+ (nitory/binary-op-- '+))
    (:- (nitory/binary-op-- '-))
    (:* (nitory/binary-op-- '*))
    (:/ (nitory/binary-op-- '/))
    (:braket (let ((f (nitory/exec-dice-tree (cadr tree))))
               `(,(nitory/concat "(" (car f) ")")
                 ,(cadr f))))
    (:roll (apply 'nitory/generate-dices (cdr tree)))
    (:const `(,(write-to-string (cadr tree))
              ,(cadr tree)))
    (:repeat `(,(caddr tree)
               ,(loop repeat (cadr tree)
                      collect (nitory/exec-dice-tree (cadddr tree)))))
    (:error (error "G!"))))

(defun nitory/roll-dice (rest &optional sender)
  (let ((leading (if (= (length rest) 0)
                     #\d
                     (char rest 0)))
        (space (position #\space rest)))
    (let ((expr (nitory/parse-dice-full-expr
                 (if space
                     (subseq rest 0 space)
                     (if (nitory/dice-expr-leading-p leading)
                         rest
                         ""))))
          (comment (string-trim
                    " "
                    (if space
                        (subseq rest (+ 1 space))
                        (if (nitory/dice-expr-leading-p leading)
                            ""
                            rest)))))
      (let ((res (handler-case (nitory/exec-dice-tree expr)
                   (error () nil))))
        (if res
            (nitory/join '(#\newline)
                         `(,(nitory/concat (if sender (a:ensure-gethash "nickname" sender "") "") " 掷骰 " (caar res)
                                           (if (string/= comment "") (nitory/concat " (" comment ")")) ":")
                           ,@(loop for a in (cadr res)
                                   collect (nitory/join "=" `(,(cadar res)
                                                              ,(car a)
                                                              ,(write-to-string (cadr a)))))))
            "掷骰失败")))))

(defun nitory/command-not-supported ()
  "无效指令")

(defun respond/do-send-private-message (user-id rmsg)
  (sender/send-message
   (sender/build-response
    "send_private_msg"
    `((:user_id . ,user-id)
      (:message . (((:type . "text")
                    (:data . ((:text . ,rmsg))))))))))

(defun respond/do-send-group-message (group-id rmsg)
  (sender/send-message
   (sender/build-response
    "send_group_msg"
    `((:group_id . ,group-id)
      (:message . (((:type . "text")
                    (:data . ((:text . ,rmsg))))))))))

(defun event/receive-notice (notice)
  ())

(defun event/receive-request (request)
  (format t "event/receive-request:~%")
  (let* ((json (yason:parse request))
         (request-type (a:ensure-gethash "request_type" json nil))
         (sub-type (a:ensure-gethash "sub_type" json nil))
         (group-id (a:ensure-gethash "group_id" json nil))
         (user-id (a:ensure-gethash "user_id" json nil))
         (comment (a:ensure-gethash "comment" json nil))
         (flag (a:ensure-gethash "flag" json nil)))
    (format t "~A ~A~%" request-type sub-type)
    (if (string= request-type "friend")
        (respond/do-friend-request user-id comment flag)
        (if (string= request-type "group")
            (a:switch (sub-type :test #'equal)
                      ("add" (respond/do-group-add-request group-id user-id comment flag))
                      ("invite" (respond/do-group-invite-request group-id user-id comment flag)))))))

(defun respond/do-friend-request (user-id comment flag)
  (let ((approve nil))
    (sender/send-message
     (sender/build-response
      "set_friend_add_request"
      `((:flag . ,flag)
        (:approve . ,approve))))))

(defun respond/do-group-add-request (group-id user-id comment flag)
  ())

(defun respond/do-group-invite-request (group-id user-id comment flag)
  (let ((approve (= user-id 1203794101)))
    (sender/send-message
     (sender/build-response
      "set_group_add_request"
      `((:flag . ,flag)
        (:type . "invite")
        (:approve . ,approve))))))


(defun event/receive-meta-event (meta-event)
  ())
