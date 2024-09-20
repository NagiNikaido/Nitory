;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; package.lisp ---- Package definition for Nitory-test.
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

(defpackage #:nitory/test
  (:use #:common-lisp
        #:rove)
  (:local-nicknames (:a :alexandria)
		    (:re :cl-ppcre)
                    (:bb :blackbird)
                    (:s :serapeum)
                    (:v :org.shirakumo.verbose))
  (:import-from #:nitory)
  (:export
   #:run-all-tests))

