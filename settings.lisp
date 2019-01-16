#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.studio)
(in-readtable :qtools)

(define-widget settings (QDialog)
  ())

(define-subwidget (settings general) (make-instance 'general-editor))

(define-subwidget (settings keys) (make-instance 'keychord-editor))

(define-subwidget (settings tabs) (q+:make-qtabwidget settings)
  (q+:add-tab tabs general "General")
  (q+:add-tab tabs keys "Key Chords"))

(define-subwidget (settings ok) (q+:make-qpushbutton "&Ok" settings))

(define-subwidget (settings cancel) (q+:make-qpushbutton "&Cancel" settings)
  (connect! cancel (clicked) settings (reject)))

(define-subwidget (settings layout) (q+:make-qgridlayout settings)
  (q+:resize settings 500 500)
  (q+:add-widget layout tabs 0 0 4 4)
  (q+:add-widget layout ok 5 2 1 1)
  (q+:add-widget layout cancel 5 3 1 1))

(define-slot (settings accept) ()
  (declare (connected ok (clicked)))
  (apply-changes settings)
  (q+:accept settings))

(defmethod apply-changes ((settings settings))
  (apply-changes (slot-value settings 'general))
  (apply-changes (slot-value settings 'keys)))

(define-widget general-editor (QWidget)
  ())

(define-subwidget (general-editor font) (make-instance 'font-input :font (q+:font (slot-value *main* 'editor))))

(define-subwidget (general-editor template) (q+:make-qpushbutton "Edit ..." general-editor))

(define-subwidget (general-editor layout) (q+:make-qformlayout general-editor)
  (q+:add-row layout "Font" font)
  (q+:add-row layout "Template" template))

(define-slot (general-editor edit-template) ()
  (declare (connected template (clicked)))
  (let ((file (config-file "template" "mess")))
    (ensure-directories-exist file)
    (with-open-file (stream file :direction :output
                                 :if-exists NIL
                                 :if-does-not-exist :create))
    (open-mess NIL file)))

(defmethod apply-changes ((editor general-editor))
  (setf (q+:font (slot-value *main* 'editor)) (value (slot-value editor 'font))))

(define-widget keychord-editor (QTableWidget)
  ())

(define-initializer (keychord-editor setup)
  (setf (q+:column-count keychord-editor) 2)
  (setf (q+:horizontal-header-labels keychord-editor) (list "Key Chord" "Command"))
  (setf (q+:stretch-last-section (q+:horizontal-header keychord-editor)) T)
  (q+:hide (q+:vertical-header keychord-editor))
  (flet ((make-table-item (content)
           (let ((item (q+:make-qtablewidgetitem content)))
             (setf (q+:flags item) (logior (q+:qt.item-is-editable)
                                           (q+:qt.item-is-enabled)))
             item)))
    (loop for i from 0
          for keychord in (keychords (keytable *main*))
          do (q+:insert-row keychord-editor i)
             (setf (q+:item keychord-editor i 0) (make-table-item (print-keychord (groups keychord) NIL)))
             (setf (q+:item keychord-editor i 1) (make-table-item (princ-to-string (action keychord)))))))

(defmethod apply-changes ((editor keychord-editor)))
