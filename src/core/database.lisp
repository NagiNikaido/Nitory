;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; database.lisp ---- brief
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

(defclass database ()
  ((db :initform (make-hash-table :test #'equal)
       :accessor db)
   (name :initarg :name
	 :accessor name)
   (path :initarg :path
	 :accessor path)))

(export-always 'make-database)
(-> make-database (&key (:name string)
		        (:path pathname)) database)
(defun make-database (&key name path)
  (make-instance 'database
		 :name name
		 :path path))

(defmethod db/@ ((dbi database) &rest keys)
  (apply #'s:href-default nil (db dbi) keys))

(defmethod (setf db/@) (value (dbi database) key &rest keys)
  (s:nlet rec ((table (db dbi))
	       (keys (cons key keys)))
    (if (rest keys)
	(progn
          (unless (gethash (car keys) table)
            (setf (gethash (car keys) table) (make-hash-table :test 'equal)))
          (rec (gethash (car keys) table) (cdr keys)))
	(setf (gethash (car keys) table) value))))

(-> hash-table-dict (hash-table) list)
(defun hash-table-dict (table)
 `(dict
   ',(hash-table-test table)
   ,@(loop for key being the hash-keys of table
           using (hash-value value)
         collect key
         collect (typecase value
                   (hash-table (hash-table-dict value))
                   (number value)
                   (string value)
                   (array value)
                   (list `',value)
                   (symbol `',value)
                   (t (error "Unsupported object type: ~A" (type-of value)))))))

(defmethod load-db ((dbi database))
  (ensure-directories-exist (path dbi))
  (with-open-file (s (path dbi)
		     :direction :input
		     :if-does-not-exist :error)
    (setf (db dbi) (eval (read s)))))

(defmethod save-db ((dbi database))
  (ensure-directories-exist (path dbi))
  (with-open-file (s (path dbi)
		     :direction :output
		     :if-exists :supersede)
    (v:info :db "Saving ~a..." (name dbi))
    (unwind-protect (print (hash-table-dict (db dbi)) s))
    (v:info :db "Done.")))

(defparameter *dbs* nil)

(defmacro register-db (name)
  `(eval-when (:load-toplevel :execute)
     (let ((db (make-database :name ,name
			      :path (s:path-join *prefix* ,name))))
       (handler-case
	   (load-db db)
	 (error (c)
	   (v:warn :db "~a" c)))
       (push db *dbs*)
       db)))

(defun db/save-dbs ()
  (loop for db in *dbs*
	do (save-db db)))
