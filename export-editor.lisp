#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.studio)
(in-readtable :qtools)

(defun strip-or-null-type (type)
  (if (and (listp type) (eq 'or (first type)))
      (let ((type (remove 'null type)))
        (if (cddr type)
            type
            (second type)))
      type))

(define-widget export-editor (QWidget)
  ((profile :accessor profile)
   (slot-inputs :initform () :accessor slot-inputs)))

(defmethod initialize-instance ((editor export-editor) &key profile)
  (setf (profile editor)
        (etypecase profile
          ((or symbol class) (make-instance profile))
          (export-profile profile)))
  (call-next-method))

(define-subwidget (export-editor layout) (q+:make-qformlayout export-editor)
  (let* ((class (class-of profile))
         (properties (profile-properties profile)))
    (loop for slot-name in properties
          for slot = (or (find slot-name (c2mop:class-slots class) :key #'c2mop:slot-definition-name)
                         (error "No slot found for property ~s in class ~a" slot-name class))
          for input = (make-input-for-slot export-editor slot profile)
          do (q+:add-row layout (string-capitalize slot-name) input)
             (push (cons slot-name input) slot-inputs))))

(defun universal-to-unix-msecs (universal)
  (* (- universal (encode-universal-time 0 0 0 1 1 1970 0)) 1000))

(defun unix-msecs-to-universal (unix)
  (+ (/ unix 1000) (encode-universal-time 0 0 0 1 1 1970 0)))

(defun make-input-for-slot (parent slot instance)
  (let ((value (slot-value instance (c2mop:slot-definition-name slot)))
        (type (strip-or-null-type (c2mop:slot-definition-type slot)))
        (description (documentation slot T)))
    (let ((input (cond ((and (listp type) (eql (first type) 'date))
                        (q+:make-qdatetimeedit (if value
                                                   (q+:qdatetime-from-msecs-since-epoch
                                                    (universal-to-unix-msecs value))
                                                   (q+:qdatetime-current-date-time))
                                               parent))
                       ((and (listp type) (eql (first type) 'file))
                        (make-instance 'file-input :direction (second type)
                                                   :file-type (cons (third type)
                                                                    (fourth type))))
                       ((subtypep type 'pathname)
                        (make-instance 'file-input))
                       ((subtypep type 'string)
                        (q+:make-qlineedit (or value "") parent))
                       ((subtypep type 'integer)
                        (let ((input (q+:make-qspinbox parent)))
                          (setf (q+:value input) (or value 0))
                          input))
                       (T
                        (warn "No specific widget for type ~s, falling back to string." type)
                        (q+:make-qlineedit (princ-to-string (or value "")) parent)))))
      (setf (q+:tool-tip input) description)
      input)))

(defun coerce-to-lisp (value)
  (typecase value
    (qobject
     (qtypecase value
       ("QDateTime"
        (unix-msecs-to-universal (q+:to-msecs-since-epoch value)))
       (T
        (error "Don't know how to coerce ~a to a lisp value." value))))
    (string
     (when (string/= "" value)
       value))
    (T value)))

(defmethod update-profile ((editor export-editor))
  (loop for (slot . input) in (slot-inputs editor)
        for value = (value input)
        do (setf (slot-value (profile editor) slot)
                 (coerce-to-lisp value))))

(define-widget export-dialog (QDialog)
  ())

(define-subwidget (export-dialog ok) (q+:make-qpushbutton "Ok" export-dialog))

(define-subwidget (export-dialog cancel) (q+:make-qpushbutton "Cancel" export-dialog))

(define-subwidget (export-dialog profiles) (q+:make-qlistwidget export-dialog)
  (setf (q+:size-policy profiles) (values (q+:qsizepolicy.maximum) (q+:qsizepolicy.minimum)))
  (setf (q+:minimum-width profiles) 100)
  (setf (q+:maximum-width profiles) 150)
  (dolist (profile (list-export-profiles))
    (let ((item (q+:make-qlistwidgetitem profiles)))
      (setf (q+:tool-tip item) (documentation profile 'type))
      (setf (q+:text item) (label (allocate-instance profile)))
      (q+:add-item profiles item))))

(define-subwidget (export-dialog scroll) (q+:make-qscrollarea export-dialog)
  (setf (q+:widget-resizable scroll) T))

(define-subwidget (export-dialog layout) (q+:make-qgridlayout export-dialog)
  (q+:add-widget layout profiles 0 0 4 1)
  (q+:add-widget layout scroll   0 1 4 4)
  (q+:add-widget layout ok       5 3 1 1)
  (q+:add-widget layout cancel   5 4 1 1))

(define-slot (export-dialog ok) ()
  (declare (connected ok (clicked)))
  (unless (null-qobject-p (q+:widget scroll))
    (update-profile (q+:widget scroll))
    (q+:accept export-dialog)))

(define-slot (export-dialog cancel) ()
  (declare (connected cancel (clicked)))
  (q+:reject export-dialog))

(define-slot (export-dialog select) ((row int))
  (declare (connected profiles (current-row-changed int)))
  (let* ((string (q+:text (q+:item profiles row)))
         (class (or (find string (list-export-profiles)
                          :key (lambda (class) (label (allocate-instance class)))
                          :test #'string-equal)
                    (error "WTF? ~s ~s" string (list-export-profiles)))))
    (finalize (q+:widget scroll))
    (setf (q+:widget scroll) (make-instance 'export-editor :profile class))))

(defun open-export-profile ()
  (let ((dialog (make-instance 'export-dialog)))
    (when (< 0 (q+:exec dialog))
      (let ((widget (q+:widget (slot-value dialog 'scroll))))
        (unless (null-qobject-p widget)
          (profile widget))))))
