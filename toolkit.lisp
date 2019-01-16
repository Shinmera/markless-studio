#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.studio)
(in-readtable :qtools)

(defvar *main*)

(defun find-any (items sequence)
  (loop for item in sequence
        thereis (find item items)))

(define-condition quit () ())

(defun read-safely (string &optional (package #.*package*))
  (with-standard-io-syntax
    (let ((*package* (etypecase package
                       ((or symbol string) (find-package package))
                       (package package)))
          (*read-eval* NIL))
      (read-from-string string))))

(defun strip-or-null-type (type)
  (if (and (listp type) (eq 'or (first type)))
      (let ((type (remove 'null type)))
        (if (cddr type)
            type
            (second type)))
      type))

(defun coerce-to-lisp (value)
  (typecase value
    (qobject
     (qtypecase value
       ("QDateTime"
        (q+:to-string value (q+:qt.isodate)))
       ("QDate"
        (q+:to-string value (q+:qt.isodate)))
       (T
        (error "Don't know how to coerce ~a to a lisp value." value))))
    (string
     (when (string/= "" value)
       value))
    (T value)))
