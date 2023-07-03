(in-package #:org.shirakumo.markless.studio)
(in-readtable :qtools)

(deftype file (direction &rest file-type)
  (declare (ignore direction file-type))
  'pathname)

(deftype date ()
  'string)

(defmethod real-type ((slot c2mop:standard-slot-definition))
  (c2mop:slot-definition-type slot))
(defclass type-capturing-slot (c2mop:standard-slot-definition)
  ((type :initarg :type :initform NIL :reader real-type)))
(defclass direct-type-capturing-slot (type-capturing-slot c2mop:standard-direct-slot-definition) ())
(defclass effective-type-capturing-slot (type-capturing-slot c2mop:standard-effective-slot-definition) ())
(defclass type-capturing-class (standard-class) ())
(defmethod c2mop:validate-superclass ((class standard-class) (superclass type-capturing-class)) T)
(defmethod c2mop:validate-superclass ((class type-capturing-class) (superclass standard-class)) T)
(defmethod c2mop:direct-slot-definition-class ((class type-capturing-class) &key) (find-class 'direct-type-capturing-slot))
(defmethod c2mop:effective-slot-definition-class ((class type-capturing-class) &key) (find-class 'effective-type-capturing-slot))
(defmethod c2mop:compute-effective-slot-definition ((class type-capturing-class) name direct-slots)
  (declare (ignore name))
  (let ((effective-slot (call-next-method)))
    (loop for direct-slot in direct-slots
          do (when (and (typep direct-slot 'direct-type-capturing-slot)
                        (eql (c2mop:slot-definition-name direct-slot)
                             (c2mop:slot-definition-name effective-slot)))
               (setf (slot-value effective-slot 'type) (real-type direct-slot))
               (return)))
    effective-slot))

(defclass export-profile ()
  ((target :initarg :target :initform #p"" :accessor target :type (file :output)
                    :documentation "The path to witch to export the document to."))
  (:metaclass type-capturing-class))

(defgeneric label (export-profile))
(defgeneric icon (export-profile))
(defgeneric profile-format (export-profile))
(defgeneric profile-properties (profile)
  (:method-combination append :most-specific-last))
(defgeneric profile-complete-p (profile)
  (:method-combination progn :most-specific-last))

(defmethod label ((_ export-profile)) "?")

(defmethod icon ((_ export-profile)) "x-office-document")

(defmethod profile-properties append ((profile export-profile))
  '(target))

(defmethod profile-complete-p progn ((profile export-profile))
  (not (null (target profile))))

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
  ((target :type (file :output ("epub" "EPUB files")))
   (cl-markless-epub:id :type (or null string)
                        :documentation "The unique document ID")
   (cl-markless-epub:date :type (or null date)
                          :documentation "The date on which this document was made.")
   (cl-markless-epub:title :type (or null string)
                           :documentation "The document's title. Can be inferred.")
   (cl-markless-epub:cover :type (or null (file :input ("*" "Image")
                                                       ("svg" "Scalable Vector Graphic")
                                                       ("png" "Portable Network Graphic")
                                                       ("jpg" "JPEG Image")))
                           :documentation "The cover image for the document. Can be a JPG, PNG, or SVG.")
   (cl-markless-epub:stylesheet :type (or null (file :input ("css" "Cascading Style Sheet")))
                                :documentation "The stylesheet used to style the document.")
   (cl-markless-epub:if-exists :initform :supersede))
  (:metaclass type-capturing-class)
  (:documentation "EPUB files for e-readers."))

(defmethod initialize-instance :after ((profile epub-export-profile) &key)
  (when (boundp '*main*)
    (cl-markless-epub::autocomplete-metadata
     (cl-markless:parse (q+:to-plain-text (slot-value *main* 'editor)) T)
     profile)))

(defmethod profile-format ((profile epub-export-profile)) profile)

(defmethod label ((_ epub-export-profile)) "EPUB")

(defmethod icon ((_ epub-export-profile)) "application-epub-zip")

(defmethod profile-properties append ((profile epub-export-profile))
  '(cl-markless-epub:id
    cl-markless-epub:title
    cl-markless-epub:date
    cl-markless-epub:cover
    cl-markless-epub:stylesheet))

(defvar *content*)

(defclass html-export-profile (export-profile cl-markless-plump:plump)
  ((target :type (file :output ("html" "HTML")))
   (template :initarg :template :initform NIL :accessor template :type (or null (file :input ("ctml" "Clip templates")))
             :documentation "A Clip HTML template to insert the contents into.
Use the tag <c:document/> where the contents should be."))
  (:metaclass type-capturing-class)
  (:documentation "HTML files for web publishing."))

(defmethod label ((_ html-export-profile)) "HTML")

(defmethod icon ((_ html-export-profile)) "text-html")

(defmethod profile-format ((profile html-export-profile))
  (if (template profile)
      profile
      'cl-markless-plump:plump))

(defmethod cl-markless:output-component ((component cl-markless-components:component) (target pathname) (format html-export-profile))
  (let ((*content* component)
        (plump:*tag-dispatchers* plump:*html-tags*))
    (with-open-file (stream target :direction :output
                                   :element-type 'character
                                   :if-exists :supersede)
      (plump:serialize (clip:process (template format)) stream))))

(clip:define-tag-processor "document" (node)
  (cl-markless:output *content* :target node :format 'cl-markless-plump:plump))

(defmethod profile-properties append ((profile html-export-profile))
  '(template))
