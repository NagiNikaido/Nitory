;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; nitory.asd ---- ASDF system definition for Nitory.
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

(asdf:defsystem "nitory"
  :description "Nitory: "
  :version "0.0.1"
  :author "NagiNikaido <naginikaido@kuusouhakuchuu.cn>"
  :license "GPL-v3.0-or-later"
  :depends-on ("alexandria"
	       "websocket-driver"
	       "cl-json"
               "yason"
               "cl-ppcre")
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "nitory"))))
  :build-operation "program-op"
  :build-pathname "build/nitory"
  :entry-point "nitory:main"
  :in-order-to ((asdf:test-op (asdf:test-op "nitory/test"))))

(asdf:defsystem "nitory/test"
  :depends-on ("alexandria"
               "nitory"
               "rove"
               "cl-json"
               "cl-ppcre")
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "package")
     (:file "nitory-test"))))
  :perform (asdf:test-op (o c)
                         (uiop:symbol-call :nitory/test
                                           :run-all-tests)))
