;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; event-bus.lisp ---- A modification of fukamachi's event-emitter.
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

(export-always 'event-bus)
(defclass event-bus ()
  ((silo :initform (make-hash-table :test 'eq)
	 :accessor silo)
   (level :initform (bt2:make-atomic-integer :value 0)
	  :accessor level)
   (lock :initform (bt2:make-lock)
         :accessor lock)))

(defstruct (listener (:constructor make-listener (function &key once (removed nil))))
  function once removed)

(defun %add-listener (obj ev listener)
  (bt2:with-lock-held ((lock obj))
    (let* ((event (to-sym ev))
           (silo (silo obj))
           (listeners (@ silo event)))
      (if listeners
          (progn (vector-push-extend listener listeners)
                 listeners)
          (setf (@ silo event)
                (make-array 1 :element-type 'listener
                              :adjustable t :fill-pointer 1
                              :initial-contents (list listener)))))))

(defun %remove-dead-listeners (obj)
  (bt2:with-lock-held ((lock obj))
    (let ((silo (silo obj)))
      (loop for key being the hash-key
            using (hash-value listeners) of silo do
            (setf (@ silo key)
                  (loop for x across listeners
                        when (not (listener-removed x))
                          collect x into v
                        finally (return (make-array (length v) :element-type 'listener
                                                    :adjustable 1 :fill-pointer (length v)
                                                    :initial-contents v))))))))

(export-always 'add-listener)
(defun add-listener (obj event listener)
  (%add-listener obj event (make-listener listener)))

(export-always 'on)
(defun on (event obj listener)
  (%add-listener obj event (make-listener listener)))

(export-always 'once)
(defun once (event obj listener)
  (%add-listener obj event (make-listener listener :once t)))

(export-always 'remove-listener)
(defun remove-listener (obj event listener &key (start 0))
  (bt2:with-lock-held ((lock obj))
    (let* ((silo (silo obj))
           (listeners (@ silo event)))
      (unless listeners
        (return-from remove-listener))
      (let ((pos (position-if (lambda (x) (and (eq listener (listener-function x))
                                               (not (listener-removed x)))) listeners
                                               :start start)))
        (setf (listener-removed (elt listeners pos)) t)))))

(export-always 'remove-all-listeners)
(defun remove-all-listeners (obj &optional event)
  (bt2:with-lock-held ((lock obj))
    (let ((silo (silo obj)))
      (loop for key being the hash-key
            using (hash-value listeners) of silo do
            (loop for x across listeners do
                  (setf (listener-removed x) t))))))

(export-always 'listeners)
(defun listeners (obj event)
  (bt2:with-lock-held ((lock obj))
    (let* ((silo (silo obj))
           (listeners (@ silo event)))
      (or listeners
          (setf (@ silo event)
                (make-array 0 :element-type 'listener
                              :adjustable t :fill-pointer 0))))))

;; (defun listener-count (obj event))

(defun %emit-accurate (ev obj &rest args)
  (let* ((event (to-sym ev))
         (listeners (listeners obj event))
         (max-size (length listeners)))
    (when (zerop max-size)
      (return-from %emit-accurate nil))
	(do ((indx 0 (1+ indx)))
		((>= indx max-size))
	  (when (not (listener-removed (elt listeners indx)))
            (apply (listener-function (elt listeners indx)) args)
            (when (listener-once (elt listeners indx))
              (setf (listener-removed (elt listeners indx)) t))))
    t))
(export-always 'emit)
(defun emit (event obj &rest args)
  (bt2:atomic-integer-incf (level obj))
  (let ((res (let ((cemit (apply #'%emit-accurate event obj args))
                   (npos (position #\. (reverse event))))
               (if npos
                   (or (apply #'emit
                              (subseq event 0 (- (length event) (1+ npos)))
                              obj args)
                       cemit)
                   cemit))))
    (when (= 0 (bt2:atomic-integer-decf (level obj)))
      (%remove-dead-listeners obj))
    res))
