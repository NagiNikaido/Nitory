;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; package.lisp ---- Package definition for Nitory.
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

#+sb-package-locks
(serapeum:eval-always
  (when (find-package :nitory)
    (sb-ext:unlock-package :nitory)))

(uiop:define-package :nitory
  (:use :cl)
  (:export #:main))

#+sb-package-locks
(sb-ext:lock-package :nitory)

(in-package :nitory)
(defvar *imports* '((:serapeum #:-> #:@ #:export-always)))
(loop for (package . symbols) in *imports*
      do (import (mapcar (lambda (symbol) (intern (symbol-name symbol) package))
                         symbols)
                 :nitory))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for (nickname package) in
        '((:a :alexandria)
          (:s :serapeum)
          (:re :cl-ppcre)
          (:j :yason)
          (:v :org.shirakumo.verbose)
          (:sig :trivial-signal)
          (:bb :blackbird)
          (:dex :dexador))
        do (trivial-package-local-nicknames:add-package-local-nickname nickname package :nitory)))

