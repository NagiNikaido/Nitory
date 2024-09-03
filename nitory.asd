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
  :depends-on ("alexandria"
;               "event-emitter"
	       "websocket-driver"
               "dexador"
	       "cl-json"
               "yason"
               "cl-ppcre"
               "verbose"
               "adopt"
               "trivial-signal"
               "blackbird"
               "bordeaux-threads")
  :components
  ((:static-file "LICENSE")
   (:static-file "VERSION")
   (:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "utils")
     (:file "event-bus")
     (:file "napcat-types")
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
  :depends-on ("alexandria"
               "nitory"
               "rove"
               "cl-json"
               "cl-ppcre"
               "blackbird"
               "bordeaux-threads"
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
