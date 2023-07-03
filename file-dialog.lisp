(in-package #:org.shirakumo.markless.studio)
(in-readtable :qtools)

(defvar *last-directory* (user-homedir-pathname))

(define-widget file-dialog (QFileDialog)
  ())

(defmethod initialize-instance :after ((file-dialog file-dialog) &key direction file-type directory)
  (let ((file-type (or file-type '(("mess" "Markless")))))
    (setf (q+:accept-mode file-dialog)
          (ecase direction
            ((NIL :input) (q+:qfiledialog.accept-open))
            (:output (q+:qfiledialog.accept-save))))
    (setf (q+:file-mode file-dialog)
          (ecase direction
            ((NIL :input) (q+:qfiledialog.existing-file))
            (:output (q+:qfiledialog.any-file))))
    (setf (q+:view-mode file-dialog) (q+:qfiledialog.detail))
    (setf (q+:name-filters file-dialog) (append (loop for (type name) in file-type
                                                      collect (format NIL "~a files (*.~a)" name type))
                                                (list "Any files (*)")))
    (setf (q+:default-suffix file-dialog) (caar file-type))
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
