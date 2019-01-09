#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.studio)
(in-readtable :qtools)

(define-widget viewer (QTextBrowser)
  ())

(define-initializer (viewer setup)
  (setf (q+:open-external-links viewer) T)
  (setf (q+:style-sheet viewer) "*{background: white; color: black; font-size: 12pt;}ul p{margin: 0}"))

(define-override (viewer load-resource) (type name)
  (or (when (= type (q+:qtextdocument.image-resource))
        (ignore-errors
         (q+:make-qimage (truename (q+:to-local-file name)))))
      (stop-overriding)))

(defmethod (setf content) ((ast cl-markless-components:component) (viewer viewer))
  (setf (content viewer) (cl-markless:output (fixup ast viewer) :target NIL :format 'cl-markless-plump:plump)))

(defmethod (setf content) ((html string) (viewer viewer))
  (setf (q+:html viewer) html))

(defmethod (setf content) ((null null) (viewer viewer))
  (q+:clear viewer))

(defmethod fixup (thing (viewer viewer))
  thing)

(defmethod fixup ((ast cl-markless-components:parent-component) (viewer viewer))
  (let ((children (cl-markless-components:children ast)))
    (loop for i from 0 below (length children)
          do (setf (aref children i) (fixup (aref children i) viewer))))
  ast)

(defmethod fixup ((ast cl-markless-components:compound) (viewer viewer))
  (setf (cl-markless-components:options ast)
        (loop for option in (cl-markless-components:options ast)
              collect (fixup option viewer)))
  ast)

(defmethod fixup ((ast cl-markless-components:size-option) (viewer viewer))
  (if (eq :em (cl-markless-components:unit ast))
      (make-instance 'cl-markless-components:size-option
                     :size (* 12 (cl-markless-components:size ast))
                     :unit :pt)
      ast))
