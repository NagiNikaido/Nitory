;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; nitory-test.lisp ---- Tests for Nitory.
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

(deftest dice-cell-test
   (testing "If parse-dice-cell gives right output."
     (ok (equal (nitory:dice/parse-dice-cell "3d10")
		'((:roll :dice 3 :face 10) 4)))
     (ok (equal (nitory:dice/parse-dice-cell "d")
                '((:roll) 1)))
     (ok (equal (nitory:dice/parse-dice-cell "3d10h1l2")
                '((:roll :dice 3 :face 10 :high 1) 6)))
     (ok (equal (nitory:dice/parse-dice-cell "3d10l2")
                '((:roll :dice 3 :face 10 :low 2) 6)))
     (ok (equal (nitory:dice/parse-dice-cell "3l1")
                '((:roll :dice 3 :low 1) 3)))
     (format t "~a" (nitory:dice/parse-dice-cell "e10"))
     (ok (equal (nitory:dice/parse-dice-cell "e10")
                '((:roll :extra 10) 3)))
     (ok (signals (nitory:dice/parse-dice-cell "")))
     (ok (signals (nitory:dice/parse-dice-cell "$8")))
     (ok (equal (nitory:dice/parse-dice-cell "8")
                '((:const 8) 1)))
     (ok (equal (nitory:dice/parse-dice-cell "(8)")
                '((:braket (:const 8)) 3)))
     (ok (signals (nitory:dice/parse-dice-cell "(8")))
     (ok (equal (nitory:dice/parse-dice-cell "8)))")
                '((:const 8) 1)))))

(deftest dice-term-test
   (testing "If parse-dice-term gives right output."
     (ok (equal (nitory:dice/parse-dice-term "8")
                (nitory:dice/parse-dice-cell "8")))
     (ok (equal (nitory:dice/parse-dice-term "8*3")
                '((:* (:const 8) (:const 3)) 3)))
     (ok (equal (nitory:dice/parse-dice-term "8*3))")
                '((:* (:const 8) (:const 3)) 3)))
     (ok (equal (nitory:dice/parse-dice-term "8*3/4*7")
                '((:* (:/ (:* (:const 8) (:const 3))
                          (:const 4))
                      (:const 7)) 7)))
     (ok (signals (nitory:dice/parse-dice-term "3d10h1l2")))))

(deftest dice-expr-test
   (testing "If parse-dice-expr gives right output."
     (ok (equal (nitory:dice/parse-dice-expr "8+6d20*3")
                `((:+ (:const 8)
                      ,(car (nitory:dice/parse-dice-term "6d20*3"))) 8)))
     (ok (equal (nitory:dice/parse-dice-expr "8+6d20*3")
                `((:+ (:const 8)
                      ,(car (nitory:dice/parse-dice-term "6d20*3"))) 8)))
     (ok (signals (nitory:dice/parse-dice-expr "3d10h1l2")))))

(deftest function-test
  (defun print-roll (expr &optional desc sender)
    (format t "~A~%~%" (nitory:dice/roll-dice expr desc sender)))
  (print-roll "3d10h2*3d20" "如何")
  (print-roll "10#10d20h3" "怎样")
  (print-roll "38)" "什么")
  (print-roll "")
  (print-roll "3d10a8")
  (print-roll "3d10b3")
  (print-roll "3l1")
  (print-roll "3a15")
  (print-roll "3b5")
  (print-roll "3d10a8e10")
  (print-roll "3d10b3e1")
  (print-roll nil))

(defun run-all-tests ()
  (run :nitory/test))
