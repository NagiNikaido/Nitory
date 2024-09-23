;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; dice.lisp ---- Dice rolling module.
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

(export-always 'dice/dice-expr-leading-p)
(defun dice/dice-expr-leading-p (leading)
  (or (dice/dice-expr-legal-leading-p leading)
      (dice/dice-expr-legal-operator-p leading)
      (dice/dice-term-legal-operator-p leading)
      (char= #\) leading)
      (char= #\# leading)))

(export-always 'dice/dice-expr-legal-leading-p)
(defun dice/dice-expr-legal-leading-p (leading)
  (or (digit-char-p leading)
      (char= #\d leading)
      (char= #\h leading)
      (char= #\l leading)
      (char= #\( leading)))

(export-always 'dice/dice-expr-legal-operator-p)
(defun dice/dice-expr-legal-operator-p (leading)
  (or (char= #\+ leading)
      (char= #\- leading)))

(export-always 'dice/dice-term-legal-operator-p)
(defun dice/dice-term-legal-operator-p (leading)
  (or (char= #\* leading)
      (char= #\/ leading)))

(export-always 'dice/dice-expr-op-p)
(defun dice/dice-expr-op-p (op op-category)
  (case op-category
    (:expr (or (eq op :+)
               (eq op :-)))
    (:term (or (eq op :*)
               (eq op :/)))
    (otherwise nil)))

(defun dice/%rotate-fix (tree op-category)
  (if (and (dice/dice-expr-op-p (car tree) op-category)
           (dice/dice-expr-op-p (caaddr tree) op-category))
      (dice/%rotate-fix `(,(caaddr tree)
                          (,(car tree) ,(cadr tree) ,(car (cdaddr tree)))
                          ,@(cdr (cdaddr tree))) op-category)
      tree))

(export-always 'dice/parse-dice-cell)
(defun dice/parse-dice-cell (cell)
  (let ((leading (ignore-errors (char cell 0))))
    (assert (and (> (length cell) 0)
                 (dice/dice-expr-legal-leading-p leading)))
    (if (char= #\( leading)
        (let* ((rp (dice/parse-dice-expr (subseq cell 1)))
               (res (car rp))
               (pos (cadr rp)))
          (assert (and (< (1+ pos) (length cell))
                       (char= #\) (char cell (1+ pos)))))
          `((:braket ,res) ,(+ pos 2)))
        (multiple-value-bind (match regs)
            (re:scan-to-strings "^(((?<dice>\\d+)?d(?<face>\\d+)?(h(?<high>\\d+)|l(?<low>\\d+))?)|(?<const>\\d+))" cell)
          (assert match)
          ;; regs: #(whole dice-cell dice face hl high low const)
          (multiple-value-bind (whole dice-cell dice face hl high low const)
              (values-list (mapcar
                            (lambda (x)
                              (ignore-errors
                               (parse-integer x)))
                            (coerce regs 'list)))
            (if const
                `((:const ,const) ,(length (elt regs 0)))
                (progn
                  (assert (and (>= (or dice 1)
                                   (or high 1))
                               (>= (or dice 1)
                                   (or low 1))))
                  `((:roll ,@(when dice `(:dice ,dice))
                           ,@(when face `(:face ,face))
                           ,@(when high `(:high ,high))
                           ,@(when low  `(:low ,low))) ,(length (elt regs 0))))))))))

(export-always 'dice/parse-dice-term)
(defun dice/parse-dice-term (term)
  (let* ((rp (dice/%parse-dice-term term))
         (res (car rp))
         (pos (cadr rp)))
    `(,(dice/%rotate-fix res :term) ,pos)))

(defun dice/%parse-dice-term (term)
  (let* ((rp1 (dice/parse-dice-cell term))
         (res1 (car rp1))
         (pos1 (cadr rp1))
         (op (ignore-errors (char term pos1))))
    (if (or (>= pos1 (length term))
            (dice/dice-expr-legal-operator-p op)
            (char= #\) op))
        rp1
        (progn
          (assert (dice/dice-term-legal-operator-p op))
          (let* ((rp2 (dice/%parse-dice-term (subseq term (1+ pos1))))
                 (res2 (car rp2))
                 (pos2 (cadr rp2)))
            `((,(if (char= #\* op)
                    :* :/) ,res1 ,res2) ,(+ pos1 1 pos2)))))))

(export-always 'dice/parse-dice-expr)
(defun dice/parse-dice-expr (expr)
  (let* ((rp (dice/%parse-dice-expr expr))
         (res (car rp))
         (pos (cadr rp)))
    `(,(dice/%rotate-fix res :expr) ,pos)))

(defun dice/%parse-dice-expr (expr)
  (let* ((rp1 (dice/parse-dice-term expr))
         (res1 (car rp1))
         (pos1 (cadr rp1))
         (op (ignore-errors (char expr pos1))))
    (if (or (>= pos1 (length expr))
            (char= #\) op))
        rp1
        (progn
          (assert (dice/dice-expr-legal-operator-p op))
          (let* ((rp2 (dice/%parse-dice-expr (subseq expr (1+ pos1))))
                 (res2 (car rp2))
                 (pos2 (cadr rp2)))
            `((,(if (char= #\+ op)
                    :+ :-) ,res1 ,res2) ,(+ pos1 1 pos2)))))))

(export-always 'dice/parse-dice-expr-init)
(defun dice/parse-dice-expr-init (expr)
  (dice/parse-dice-expr
   (if (= 0 (length expr))
       "d"
       expr)))

(export-always 'dice/parse-dice-full-expr)
(defun dice/parse-dice-full-expr (expr)
  (multiple-value-bind (match regs)
      (re:scan-to-strings "^(?<repeat>\\d+)#" expr)
    (assert (= (count #\( expr) (count #\) expr)))
    (let* ((single-expr (subseq expr (length match)))
           (post (dice/parse-dice-expr-init single-expr)))
      `(:repeat  ,(if regs (parse-integer (elt regs 0)) 1) (,expr ,single-expr) ,(car post)))))

(export-always 'dice/generate-dices)
(defun dice/generate-dices (&key (dice 1) (face 20) (high nil) (low nil) &allow-other-keys)
  (flet ((dice-pretty-concat (list kth)
           (s:fmt "{~a}"
                  (str:join ","
                            (loop for d in list
                                  for i = 1 then (incf i)
                                  collect (s:fmt "~a~d~a"
                                                 (if (= i 1) "[" "")
                                                 d
                                                 (if (= i kth) "]" "")))))))
    (let ((rolled (loop repeat dice collect (1+ (random face)))))
      (if high
          (let ((sorted (sort rolled #'>)))
            `(,(dice-pretty-concat sorted high)
              ,(reduce #'+ (subseq sorted 0 high))))
          (if low
              (let ((sorted (sort rolled #'<)))
                `(,(dice-pretty-concat sorted low)
                  ,(reduce #'+ (subseq sorted 0 low))))
              `(,(s:fmt "{~a}" (str:join "," (mapcar #'write-to-string rolled)))
                ,(reduce #'+ rolled)))))))

(export-always 'dice/exec-dice-tree)
(defun dice/exec-dice-tree (tree)
  (flet ((binary-op (op)
           (let ((f (dice/exec-dice-tree (cadr tree)))
                 (g (dice/exec-dice-tree (caddr tree))))
             `(,(str:concat (car f) (symbol-name op) (car g))
               ,(eval `(,op ,(cadr f) ,(cadr g)))))))
    (case (car tree)
      (:+ (binary-op '+))
      (:- (binary-op '-))
      (:* (binary-op '*))
      (:/ (binary-op '/))
      (:braket (let ((f (dice/exec-dice-tree (cadr tree))))
                 `(,(s:fmt "(~a)" (car f))
                   ,(cadr f))))
      (:roll (apply #'dice/generate-dices (cdr tree)))
      (:const `(,(write-to-string (cadr tree))
                ,(cadr tree)))
      (:repeat `(,(caddr tree)
                 ,(loop repeat (cadr tree)
                        collect (dice/exec-dice-tree (cadddr tree)))))
      (otherwise (error "G!")))))

(defun dice/roll-dice-deprecated (rest &optional sender)
  (let* ((leading (if (= (length rest) 0)
                     #\d
                     (char rest 0)))
         (space (position #\space rest))
         (expr (handler-case (dice/parse-dice-full-expr
                              (if space
                                  (subseq rest 0 space)
                                  (if (dice/dice-expr-leading-p leading)
                                      rest
                                      "")))
                 (error () (return-from dice/roll-dice-deprecated "掷骰失败"))))
         (comment (str:trim
                   (if space
                       (subseq rest (1+ space))
                       (if (dice/dice-expr-leading-p leading)
                           ""
                           rest))))
         (res (handler-case (dice/exec-dice-tree expr)
                (error () nil))))
        (if (not res)
            "掷骰失败"
            (str:join #\newline
                      `(,(str:concat (if sender
                                         (or (nick/get-nick (@ sender "user_id"))
                                             (a:ensure-gethash "nickname" sender ""))
                                         "") " 掷骰 " (caar res)
                                         (if (string/= comment "") (s:fmt " (~a)" comment)) ":")
                        ,@(loop for a in (cadr res)
                                collect (str:join "=" `(,(cadar res)
                                                        ,(car a)
                                                        ,(write-to-string (cadr a))))))))))

(defun dice/cmd-roll-deprecated (json args)
  (multiple-value-bind (match arguments)
      (re:scan-to-strings "r(h)?((\\d+)|([+-]\\d+))?" (first args))
    (let* ((msg-type (when (elt arguments 0) "private"))
           (sender (@ json "sender"))
           (rest (str:join " "
                           (if (elt arguments 1)
                               (cons (s:fmt "~ad~a"
                                            (or (elt arguments 2) "")
                                            (or (elt arguments 3) ""))
                                     (cdr args))
                               (cdr args))))
           (msg (dice/roll-dice-deprecated rest sender)))
      (reply-to *napcat-websocket-client*
                json (make-message msg) msg-type))))


(export-always 'dice/cmd-roll)
(defun dice/cmd-roll (json expr desc &key private &allow-other-keys)
  (let* ((sender (@ json "sender"))
         (msg-type (when private "private"))
         (msg (handler-case
                  (let* ((expr-tree (dice/parse-dice-full-expr expr))
                         (res (dice/exec-dice-tree expr-tree)))
                    (str:join #\newline
                              `(,(str:concat (if sender
                                                 (or (nick/get-nick (@ sender "user_id"))
                                                     (a:ensure-gethash "nickname" sender ""))
                                                 "")
                                             " 掷骰 "
                                             (caar res)
                                             (when desc (s:fmt " (~a)" desc))
                                             ":")
                                ,@(loop for a in (cadr res)
                                        collect (str:join "=" `(,(cadar res)
                                                                ,(car a)
                                                                ,(write-to-string (cadr a))))))))
               (error () "掷骰失败"))))
    (reply-to *napcat-websocket-client*
              json (make-message msg) msg-type)))

(register-command
 (make-command :display-name "r"
               :hidden nil
               :short-usage "掷骰指令（默认d20）"
               :cmd-face "r"
               :options (list (make-option
                               "expr"
                               :predicator
                               (lambda (opt)
                                 (dice/dice-expr-leading-p (char opt 0)))
                               :optional t)
                              (make-option
                               "desc"
                               :optional t))
               :action #'dice/cmd-roll
               :usage ""))

(register-command
 (make-command :hidden t
               :cmd-face "rh"
               :options (list (make-option
                               "expr"
                               :predicator
                               (lambda (opt)
                                 (dice/dice-expr-leading-p (char opt 0)))
                               :optional t)
                              (make-option
                               "desc"
                               :optional t))
               :action (lambda (&rest rest)
                         (apply #'dice/cmd-roll (append rest (list :private t))))))

(register-command
 (make-command :hidden t
               :cmd-face (lambda (cmd args kwargs)
                           (multiple-value-bind (match arguments)
                               (re:scan-to-strings "^r((\\d+)|([+-]\\d+))$" cmd)
                             (when match
                               (s:push-end (s:fmt "~ad~a"
                                                  (or (elt arguments 1) "")
                                                  (or (elt arguments 2) ""))
                                           args)
                               (values t args kwargs))))
               :options (list (make-option
                               "desc"
                               :optional t))
               :action #'dice/cmd-roll))
