#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.studio)

(deftype file (direction type label)
  (declare (ignore direction type label))
  'pathname)

(deftype date ()
  'string)

(defclass export-profile ()
  ((target :initarg :target :initform #p"" :accessor target :type (file :output "*" "Any files")
           :documentation "The path to witch to export the document to.")))

(defgeneric label (export-profile))

(defmethod label ((_ export-profile)) "?")

(defgeneric profile-format (export-profile))

(defgeneric profile-properties (profile)
  (:method-combination append :most-specific-last))

(defmethod profile-properties append ((profile export-profile))
  '(target))

(defmethod export-mess ((text string) (profile export-profile))
  (cl-markless:output text :target (target profile)
                           :format (profile-format profile)))

(defun list-export-profiles ()
  (let ((classes ()))
    (labels ((traverse (class)
               (dolist (subclass (cl-markless::subclasses class))
                 (push subclass classes)
                 (traverse subclass))))
      (traverse (find-class 'export-profile)))
    (sort classes #'string< :key #'class-name)))

(defclass epub-export-profile (export-profile cl-markless-epub:epub)
  ((target :type (file :output "epub" "EPUB files"))
   (cl-markless-epub:id :type (or null string)
                        :documentation "The unique document ID")
   (cl-markless-epub:date :type (or null date)
                          :documentation "The date on which this document was made.")
   (cl-markless-epub:title :type (or null string)
                           :documentation "The document's title. Can be inferred.")
   (cl-markless-epub:cover :type (or null (file :input "*" "Image files"))
                           :documentation "The cover image for the document. Can be a JPG, PNG, or SVG.")
   (cl-markless-epub:stylesheet :type (or null (file :input "css" "Cascading stylesheets"))
                                :documentation "The stylesheet used to style the document.")
   (cl-markless-epub:if-exists :initform :supersede))
  (:documentation "EPUB files for e-readers."))

(defmethod label ((_ epub-export-profile)) "EPUB")

(defmethod profile-properties append ((profile epub-export-profile))
  '(cl-markless-epub:id
    cl-markless-epub:date
    cl-markless-epub:title
    cl-markless-epub:cover
    cl-markless-epub:stylesheet))

(defclass html-export-profile (export-profile cl-markless-plump:plump)
  ((target :type (file :output "html" "HTML files"))
   (template :initarg :template :initform NIL :type (or null (file :input "ctml" "Clip templates"))
             :documentation "A Clip HTML template to insert the contents into."))
  (:documentation "HTML files for web publishing."))

(defmethod label ((_ html-export-profile)) "HTML")

(defmethod profile-properties append ((profile html-export-profile))
  '(template))
