#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.studio)
(in-readtable :qtools)

(defvar *last-directory* (user-homedir-pathname))

(define-widget file-dialog (QFileDialog)
  ())

(defmethod initialize-instance :after ((file-dialog file-dialog) &key direction file-type directory)
  (let* ((file-type (or file-type '("mess" . "Markless")))
         (type (if (consp file-type) (car file-type) file-type))
         (label (if (consp file-type) (cdr file-type) (format NIL "~@(~a~)" type))))
    (setf (q+:accept-mode file-dialog)
          (ecase direction
            ((NIL :input) (q+:qfiledialog.accept-open))
            (:output (q+:qfiledialog.accept-save))))
    (setf (q+:file-mode file-dialog)
          (ecase direction
            ((NIL :input) (q+:qfiledialog.existing-file))
            (:output (q+:qfiledialog.any-file))))
    (setf (q+:view-mode file-dialog) (q+:qfiledialog.detail))
    (setf (q+:name-filters file-dialog) (list (format NIL "~a files (*.~a)" label type)
                                              "Any files (*)"))
    (setf (q+:default-suffix file-dialog) type)
    (setf (q+:directory file-dialog) (uiop:native-namestring (or directory *last-directory*)))))

(defun open-file (direction &rest args &key file-type directory)
  (declare (ignore file-type directory))
  (with-finalizing ((dialog (apply #'make-instance 'file-dialog :direction direction args)))
    (when (< 0 (q+:exec dialog))
      (setf *last-directory* (uiop:parse-native-namestring
                              (q+:absolute-path (q+:directory dialog))
                              :ensure-directory T))
      (let ((file (first (q+:selected-files dialog))))
        (when file
          (setf file (uiop:parse-native-namestring file))
          (unless (uiop:directory-pathname-p file)
            file))))))
