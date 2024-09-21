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

(in-package #:cl-user)
(push :verbose-no-init *features*)

(asdf:defsystem "nitory"
  :description "Nitory: A multipurpose chatbot based on OneBot & NapCat."
  :version (:read-file-form "VERSION")
  :author "NagiNikaido <naginikaido@kuusouhakuchuu.cn>"
  :license "GPL-v3.0-or-later"
  :depends-on ("adopt"
               "alexandria"
               "blackbird"
               "bordeaux-threads"
               "cl-ppcre"
               "dexador"
               "serapeum"
               "str"
               "trivial-signal"
               "trivial-package-local-nicknames"
               "trivial-types"
               "verbose"
	       "websocket-driver"
               "yason")
  :components
  ((:static-file "LICENSE")
   (:static-file "VERSION")
   (:module "src"
    :serial t
    :components
    ((:file "package")
     (:module "core"
      :serial t
      :components
      ((:file "event-bus")
       (:file "message")))
     (:file "utils")
     (:file "napcat")
     (:file "nick")
     (:file "dice")
     (:file "khst-easter-egg")
     (:file "khst")
     (:file "nitory"))))
  :build-operation "program-op"
  :build-pathname "build/nitory"
  :entry-point "nitory:main"
  :in-order-to ((asdf:test-op (asdf:test-op "nitory/test"))))

(asdf:defsystem "nitory/test"
  :depends-on ("nitory"
               "alexandria"
               "blackbird"
               "bordeaux-threads"
               "cl-ppcre"
               "rove"
               "serapeum"
               "str"
               "verbose")
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "package")
     (:file "napcat-test")
     (:file "nitory-test"))))
  :perform (asdf:test-op (o c)
                         (uiop:symbol-call :nitory/test
                                           :run-all-tests)))
