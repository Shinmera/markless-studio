(in-package #:org.shirakumo.markless.studio)
(in-readtable :qtools)

(define-widget font-input (QPushButton)
  ((font :initarg :font :initform (q+:make-qfont) :accessor font)))

(define-initializer (font-input setup)
  (setf (font font-input) (font font-input)))

(defmethod (setf font) :after (font (font-input font-input))
  (setf (q+:text font-input) (format NIL "~a ~a" (q+:family font) (q+:point-size font))))

(define-slot (font-input button-pushed) ()
  (declare (connected font-input (clicked)))
  (with-finalizing ((dialog (q+:make-qfontdialog font font-input)))
    (when (< 0 (q+:exec dialog))
      (setf (font font-input) (q+:selected-font dialog)))))

(defmethod value ((font-input font-input))
  (font font-input))

(define-widget file-input (QWidget)
  ((direction :initarg :direction :initform :output :accessor direction)
   (file-type :initarg :file-type :initform NIL :accessor file-type)))

(defmethod initialize-instance :after ((file-input file-input) &key value)
  (when value
    (setf (value file-input) value)))

(define-subwidget (file-input path) (q+:make-qlineedit file-input))

(define-subwidget (file-input button) (q+:make-qpushbutton "..." file-input)
  (with-finalizing ((metrics (q+:make-qfontmetrics (q+:font button))))
    (setf (q+:fixed-width button) (+ 5 (q+:width metrics (q+:text button))))
    (setf (q+:flat button) T)))

(define-subwidget (file-input layout) (q+:make-qhboxlayout file-input)
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 0)
  (q+:add-widget layout path)
  (q+:add-widget layout button))

(define-slot (file-input button-pushed) ()
  (declare (connected button (clicked)))
  (let ((value (open-file direction :file-type file-type)))
    (when value (setf (value file-input) value))))

(defmethod value ((file-input file-input))
  (let ((text (q+:text (slot-value file-input 'path))))
    (when (string/= "" text)
      (uiop:parse-native-namestring text))))

(defmethod (setf value) (value (file-input file-input))
  (setf (q+:text (slot-value file-input 'path)) (uiop:native-namestring value)))
