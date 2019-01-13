#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.studio)
(in-readtable :qtools)

(define-widget status (QStatusBar)
  ((action :initform NIL :accessor action)))

(define-initializer (status setup)
  (setf (q+:size-grip-enabled status) NIL))

(define-subwidget (status input) (make-instance 'status-input)
  (q+:hide input))

(define-slot (status input-done) ((string string))
  (declare (connected input (input-complete string)))
  (let ((action (action status)))
    (when action
      (setf (action status) NIL)
      (funcall action string))))

(define-slot (status input-quit) ()
  (declare (connected input (input-aborted)))
  (setf (action status) NIL)
  (message status "Quit."))

(defmethod (setf action) :after ((null null) (status status))
  (let ((input (slot-value status 'input)))
    (q+:remove-widget status input)
    (q+:hide input)))

(defmethod (setf action) :after (action (status status))
  (let ((input (slot-value status 'input)))
    (q+:add-widget status input 1)
    (q+:show input)))

(defmethod message ((status status) control &rest args)
  (q+:show-message status (apply #'format NIL control args)))

(defmethod prompt ((status status) action control &rest args)
  (q+:clear-message status)
  (setf (action status) action)
  (setf (label (slot-value status 'input)) (apply #'format NIL control args))
  (focus (slot-value status 'input)))

(defmacro with-prompt ((result status control &rest args) &body body)
  `(prompt ,status (lambda (,result) ,@body) ,control ,@args))

(defmethod quit ((status status))
  (quit (slot-value status 'input)))

(define-widget status-input (QWidget)
  ())

(defmethod (setf label) ((string string) (status-input status-input))
  (setf (q+:text (slot-value status-input 'label)) string))

(defmethod quit ((status-input status-input))
  (q+:clear (slot-value status-input 'input))
  (signal! status-input (input-aborted)))

(defmethod focus ((status-input status-input))
  (q+:set-focus (slot-value status-input 'input)))

(define-subwidget (status-input label) (q+:make-qlabel status-input))

(define-subwidget (status-input input) (q+:make-qlineedit status-input)
  (setf (q+:focus-policy input) (q+:qt.strong-focus)))

(define-subwidget (status-input layout) (q+:make-qhboxlayout status-input)
  (setf (q+:contents-margins layout) (values 3 0 3 0))
  (setf (q+:spacing layout) 0)
  (q+:add-widget layout label)
  (q+:add-widget layout input))

(define-signal (status-input input-complete) (string))

(define-signal (status-input input-aborted) ())

(define-slot (status-input input-done) ()
  (declare (connected input (return-pressed)))
  (let ((text (q+:text input)))
    (q+:clear input)
    (signal! status-input (input-complete string) text)))
