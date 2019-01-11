#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.studio)
(in-readtable :qtools)

(define-widget status (QStatusBar)
  ((action :initform NIL :accessor action)))

(define-subwidget (status input) (q+:make-qlineedit status)
  (q+:hide input))

(define-slot (status input-done) ()
  (declare (connected input (return-pressed)))
  (let ((text (q+:text input)))
    (q+:clear input)
    (when action
      (funcall action text)
      (setf (action status) NIL))))

(defmethod (setf action) ((null null) (status status))
  (q+:clear (slot-value status 'input))
  (q+:remove-widget status (slot-value status 'input)))

(defmethod (setf action) :after (action (status status))
  (q+:add-widget status (slot-value status 'input) 1))

(defmethod message ((status status) control &rest args)
  (q+:show-message status (apply #'format NIL control args)))

(defmethod prompt ((status status) action control &rest args)
  (setf (action status) action))
