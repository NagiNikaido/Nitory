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
      (char= #\a leading)
      (char= #\b leading)
      (char= #\h leading)
      (char= #\l leading)
      (char= #\e leading)
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
            (re:scan-to-strings "^((?<dice-or-const>\\d*)(?:d(?<face>\\d*))?(?:a(?<lb>\\d+)|b(?<ub>\\d+)|h(?<high>\\d+)|l(?<low>\\d+))?(?:e(?<extra>\\d+))?)" cell)
          (assert match)
          ;; regs: #(whole const dice-cell dice df face abhl lb ub high low)
          (multiple-value-bind (whole dice-or-const face lb ub high low extra)
              (values-list (mapcar
                            (lambda (x)
                              (ignore-errors
                               (parse-integer x)))
                            (coerce regs 'list)))
            
            (if (notany #'identity (subseq regs 2))
                `((:const ,dice-or-const) ,(length (elt regs 0)))
                (let ((dice dice-or-const))
                  (assert (and (>= (or dice 1)
                                   (or high 1))
                               (>= (or dice 1)
                                   (or low 1))))
                  (assert (and (>= (or face 20) (or lb 1))
                               (<= 1 (or lb 1))))
                  (assert (and (>= (or face 20) (or ub 1))
                               (<= 1 (or ub 1))))
                  (assert (or (not extra)
                              (and ub (< extra (or face 20)))
                              (> extra 1)))
                  `((:roll ,@(when dice `(:dice ,dice))
                           ,@(when face `(:face ,face))
                           ,@(when lb `(:lb ,lb))
                           ,@(when ub `(:ub ,ub))
                           ,@(when high `(:high ,high))
                           ,@(when low  `(:low ,low))
                           ,@(when extra `(:extra ,extra))) ,(length (elt regs 0))))))))))

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

(defun dice/%pretty-concat (list selected)
  (s:fmt "{~{~a~}}"
         (loop with first? = t
               with previous-selected = nil
               with s
               for d in list
               for i = 0 then (incf i)
               do (setf s (typecase selected
                            (sequence (elt selected i))
                            (function (funcall selected i))))
               collect (s:fmt "~@[]~1*~]~@[,~1*~]~@[[~1*~]~d"
                              (and previous-selected (not s))
                              (not first?)
                              (and (not previous-selected) s)
                              d) into dice-list
               do (setf first? nil)
               do (setf previous-selected s)
               finally (return
                         (if previous-selected
                             (s:append1 dice-list "]")
                             dice-list)))))

(export-always 'dice/generate-dices)
(defun dice/generate-dices (&key (dice 1) (face 20) (high nil) (low nil)
                            (lb nil) (ub nil) (extra nil) &allow-other-keys)
  (let ((rolled
          (if (not extra)
              (loop repeat dice collect (1+ (random face)))
              (flet ((extra-p (cd)
               (if ub
                   (<= cd extra)
                   (>= cd extra))))
                (loop with nd
                      for i from 1 to dice
                      do (setf nd (1+ (random face)))
                      if (extra-p nd) do (decf i)
                      collect nd)))))
    (cond
      (high (let ((sorted (sort rolled #'>)))
              `(,(dice/%pretty-concat sorted (lambda (i) (< i high)))
                ,(reduce #'+ (subseq sorted 0 high)))))
      (low (let ((sorted (sort rolled #'<)))
             `(,(dice/%pretty-concat sorted (lambda (i) (< i low)))
               ,(reduce #'+ (subseq sorted 0 low)))))
      (lb `(,(dice/%pretty-concat rolled (mapcar (lambda (x) (>= x lb)) rolled))
            ,(count-if (lambda (x) (>= x lb)) rolled)))
      (ub `(,(dice/%pretty-concat rolled (mapcar (lambda (x) (<= x ub)) rolled))
            ,(count-if (lambda (x) (<= x ub)) rolled)))
      (t `(,(s:fmt "{~{~d~^,~}}" rolled)
           ,(reduce #'+ rolled))))))

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

(export-always 'dice/roll-dice)
(defun dice/roll-dice (expr &optional desc sender)
  (handler-case
      (let* ((expr-tree (dice/parse-dice-full-expr (or expr "")))
             (res (dice/exec-dice-tree expr-tree)))
        (s:fmt "~@[~a~] 掷骰 ~@[~a~] ~@[(~a)~]:~%~{~{~a~^=~}~%~}"
               (when sender (or (nick/get-nick (@ sender "user_id"))
                                (@ sender "nickname")))
               (caar res)
               desc
               (loop for a in (cadr res)
                     collect (list (cadar res)
                                   (car a)
                                   (cadr a)))))
    (error () "掷骰失败")))

(export-always 'dice/cmd-roll)
(defun dice/cmd-roll (json expr desc &key private &allow-other-keys)
  (let* ((sender (@ json "sender"))
         (msg-type (when private "private"))
         (msg (dice/roll-dice expr desc sender)))
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
                                 (and (stringp opt)
                                      (dice/dice-expr-leading-p (char opt 0))))
                               :optional t)
                              (make-option
                               "desc"
                               :optional t))
               :action #'dice/cmd-roll
               :usage
"掷骰指令（默认d20）
.r [重复次数#][掷骰表达式] [备注]
掷骰表达式为掷骰单元及常数组成的算术表达式
掷骰单元形如 [枚数][d面数][a目标下限][b目标上限][h取高枚数][l取低枚数][e追加目标]
其中：
  目标下限：每掷出一个大于等于目标下限的骰子计为一个成功数
  目标上限：每掷出一个小于等于目标上限的骰子计为一个成功数
  取高枚数：取最高的若干枚骰子
  取低枚数：取最低的若干枚骰子
  追加目标：每掷出大于等于追加目标的结果则追加一枚骰子，若同时设定了目标上限则转为“小于等于追加目标”，其余不变
例：
    3d6        掷3枚d6
    2d20h1     掷2枚d20，取其中较高的1枚
    3d20l2     掷2枚d20，取其中最低的2枚
    6d20b5e1   掷6枚d20，每掷出小于等于5的结果就计为一次成功，每掷出小于等于1的结果就追加一枚骰子
    6d10a8e10  掷6枚d10，每掷出大于等于8的结果就计为一次成功，每掷出大于等于10的结果就追加一枚骰子
需注意：
  目标上限、目标下限、取高枚数与取低枚数最多有一项
  取高枚数与取低枚数需小于等于总枚数
  追加目标必须大于1（需要大于等于追加目标时）或小于面数（需要小于等于追加目标时），否则将无限追加
另外，.rh 指令用于暗骰，但需要添加好友才能收到信息
当掷骰较为简单时，可将枚数或运算合并至r上，如：
    .r3  === .r 3d20
    .r+2 === .r 1d20+2
此功能与暗骰可同时生效，如：
    .rh3 === .rh 3d20
    .rh*2 === .rh 1d20*2
但不支持括号，如 .r+(2*3) "))

(register-command
 (make-command :hidden t
               :cmd-face "rh"
               :options (list (make-option
                               "expr"
                               :predicator
                               (lambda (opt)
                                 (and (stringp opt)
                                      (dice/dice-expr-leading-p (char opt 0))))
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
                               (re:scan-to-strings "^r(h)?((\\d+)|([+\\-\\*/]\\d+))$" cmd)
                             (when match
                               (when (elt arguments 0)
                                   (progn
                                     (s:push-end :private kwargs)
                                     (s:push-end t kwargs)))
                               (when (elt arguments 1)
                                 (s:push-end (s:fmt "~ad~a"
                                                    (or (elt arguments 2) "")
                                                    (or (elt arguments 3) ""))
                                             args))
                               (values t args kwargs))))
               :options (list (make-option
                               "desc"
                               :optional t))
               :action #'dice/cmd-roll))
