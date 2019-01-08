#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.studio)
(in-readtable :qtools)

(defvar *error-format*)

(defun start ()
  (with-main-window (w 'main :name "Markless Studio")
    (let ((format (q+:make-qtextcharformat)))
      (setf (q+:underline-style format) (q+:qtextcharformat.wave-underline))
      (setf *error-format* format))))

(define-widget main (QMainWindow)
  ())

(define-subwidget (main split) (q+:make-qsplitter (q+:qt.horizontal) main)
  (setf (q+:central-widget main) split)
  (setf (q+:children-collapsible split) T))

(define-subwidget (main editor) (make-instance 'editor)
  (q+:add-widget split editor))

(define-subwidget (main viewer) (make-instance 'viewer)
  (q+:add-widget split viewer))

(define-slot (main update) ()
  (declare (connected editor (text-changed)))
  (multiple-value-bind (ast error) (parse-safely (q+:to-plain-text editor))
    (if ast
        (setf (content viewer) ast)
        (markup-error editor error))))

(defun parse-safely (text)
  (handler-case (cl-markless:parse text T)
    (cl-markless:markless-condition (error)
      (values NIL error))))

(define-menu (main file)
  (:item ("&Open..." (ctrl o)))
  (:item ("&Save" (ctrl s)))
  (:item ("Save &As..." (ctrl alt s)))
  (:separator)
  (:item ("&Export" (ctrl e)))
  (:item ("Export As..." (ctrl alt e)))
  (:separator)
  (:item ("&Quit" (ctrl q))
         (q+:close main)))

(define-menu (main help)
  (:item ("&About" (ctrl h))))

(define-widget editor (QPlainTextEdit)
  ())

(defmethod markup-error ((editor editor) error)
  (let ((cursor (q+:text-cursor editor))
        (selection (q+:make-qtextedit-extraselection)))
    (q+:select cursor (q+:qtextcursor.line-under-cursor))
    (setf (q+:cursor selection) cursor)
    (setf (q+:format selection) *error-format*)
    ;; Wait for upstream fix
    ;(setf (q+:extra-selections editor) (list selection))
    ))

(define-widget viewer (QTextBrowser)
  ())

(define-initializer (viewer setup)
  (setf (q+:open-external-links viewer) T)
  (setf (q+:style-sheet viewer) "html{background: white; color: black; font-size: 12pt;}"))

(define-override (viewer load-resource) (type name)
  (if (= type (q+:qtextdocument.image-resource))
      (q+:make-qimage (uiop:native-namestring (uiop:resolve-symlinks (q+:to-local-file name))))
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
