#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.studio)
(in-readtable :qtools)

(defvar *main*)

(defun start ()
  (with-main-window (*main* 'main :name "Markless Studio")))

(define-widget main (QMainWindow)
  ((keytable :initform (make-instance 'keychord-table) :accessor keytable)
   (source-file :initarg :source-file :initform NIL :accessor source-file)
   (export-profile :initarg :export-profile :initform NIL :accessor export-profile)))

(defmethod (setf source-file) :after (file (main main))
  (setf (q+:window-title main) (format NIL "Markless Studio~@[ - ~a~]" file)))

(define-initializer (main setup)
  (setf *main* main)
  (q+:install-event-filter *qapplication* main)
  (make-emacs-keytable main (keytable main)))

(define-finalizer (main teardown)
  (q+:remove-event-filter *qapplication* main)
  (makunbound '*main*))

(define-subwidget (main split) (q+:make-qsplitter (q+:qt.horizontal) main)
  (setf (q+:central-widget main) split)
  (setf (q+:children-collapsible split) T))

(define-subwidget (main editor) (make-instance 'editor)
  (q+:add-widget split editor))

(define-subwidget (main viewer) (make-instance 'viewer)
  (q+:add-widget split viewer))

(define-subwidget (main status) (make-instance 'status)
  (setf (q+:status-bar main) status))

(define-slot (main update) ()
  (declare (connected editor (text-changed)))
  (multiple-value-bind (ast conditions) (parse-safely (q+:to-plain-text editor))
    (clear-conditions editor)
    (when ast
      (setf (content viewer) ast))
    (dolist (condition conditions)
      (markup-condition editor condition))))

(define-condition quit () ())

(define-override (main event-filter) (_ ev)
  (declare (ignore _))
  (let ((dir (qtenumcase (q+:type ev)
               ((q+:qevent.key-press) :dn)
               ((q+:qevent.key-release) :up))))
    (when dir
      (handler-bind ((quit (lambda (_) (quit status))))
        (let ((ev (cast "QKeyEvent" ev)))
          (unless (q+:is-auto-repeat ev)
            (update keytable (qt-key->key (q+:key ev) (q+:modifiers ev)) dir)))))))

(defun parse-safely (text)
  (let ((conditions ()))
    (handler-case
        (handler-bind ((warning (lambda (condition)
                                  (push condition conditions))))
          (values (cl-markless:parse text T)
                  (nreverse conditions)))
      (error (condition)
        (push condition conditions)
        (values NIL (nreverse conditions))))))

(defmethod open-mess ((main main) (target (eql NIL)))
  (let ((file (open-file :input)))
    (when file (open-mess main file))))

(defmethod open-mess ((main main) (target (eql T)))
  (open-mess main (source-file main)))

(defmethod open-mess ((main main) (pathname pathname))
  (let ((output (make-string-output-stream))
        (buffer (make-string 4096)))
    (with-open-file (input pathname :direction :input
                                    :element-type 'character
                                    :external-format :utf-8)
      (loop for read = (read-sequence buffer input)
            while (< 0 read)
            do (write-sequence buffer output :end read)))
    (setf (q+:plain-text (slot-value main 'editor))
          (get-output-stream-string output))
    (setf (source-file main) pathname)))

(defmethod open-mess ((main main) (target (eql :new)))
  (setf (source-file main) NIL)
  (q+:clear (slot-value main 'editor)))

(defmethod save-mess ((main main) (target (eql NIL)))
  (let ((file (open-file :output)))
    (when file (open-mess main file))))

(defmethod save-mess ((main main) (target (eql T)))
  (save-mess main (source-file main)))

(defmethod save-mess ((main main) (pathname pathname))
  (with-open-file (output pathname :direction :output
                                   :element-type 'character
                                   :external-format :utf-8
                                   :if-exists :supersede)
    (write-string (q+:to-plain-text (slot-value main 'editor))
                  output))
  (setf (source-file main) pathname))


;; FIXME: this
(defmethod export-mess ((main main) (profile (eql NIL))))

(defmethod export-mess ((main main) (profile (eql T)))
  (export-mess main (export-profile main)))

;;(defmethod export ((main main) (profile profile))
;;  (setf (export-profile main) profile))

(define-menu (main file "&File")
  (:item "&New"
    (open-mess main :new))
  (:item "&Open..."
    (open-mess main NIL))
  (:item "&Save"
    (save-mess main T))
  (:item "Save &As..."
    (save-mess main NIL))
  (:separator)
  (:item "&Export"
    (export-mess main T))
  (:item "Export As..."
    (export-mess main NIL))
  (:separator)
  (:item "&Quit"
    (q+:close main)))

(define-menu (main edit "&Edit")
  (:item "&Undo"
    (q+:undo editor))
  (:item "&Redo"
    (q+:redo editor))
  (:separator)
  (:item "&Copy"
    (q+:copy editor))
  (:item "Cu&t"
    (q+:cut editor))
  (:item "&Paste"
    (q+:paste editor))
  (:separator)
  (:item "&Settings"))

(define-menu (main help "&Help")
  (:item "&About"
    (let ((studio (asdf:find-system :markless-studio))
          (implementation (asdf:find-system :cl-markless)))
      (with-finalizing ((box (q+:make-qmessagebox main)))
        (setf (q+:window-title box) "About Markless-Studio")
        (setf (q+:text box) (format NIL "~a<br />
<br />
The source code is openly available and licensed under ~a.<br />
<br />
Homepage: <a href=\"~a~:*\">~a</a><br />
Author: ~a<br />
Version: ~a<br />
<br />
CL-Markless Version: ~a<br />
Lisp Implementation: ~a ~a"
                                    (asdf:system-description studio)
                                    (asdf:system-license studio)
                                    (asdf:system-homepage studio)
                                    (asdf:system-author studio)
                                    (asdf:component-version studio)
                                    (asdf:component-version implementation)
                                    (lisp-implementation-type)
                                    (lisp-implementation-version)))
        (#_exec box)))))

(defun read-safely (string)
  (with-standard-io-syntax
    (let ((*package* #.*package*)
          (*read-eval* NIL))
      (read-from-string string))))

(defun make-emacs-keytable (main &optional (table (make-instance 'keychord-table)))
  (with-slots-bound (main main)
    (macrolet ((def (chord &body body)
                 (cond ((and (listp (first body))
                             (find (car (first body)) '(function quote)))
                        `(install (make-keychord ,chord ,(first body)) table))
                       (T
                        `(install (make-keychord ,chord (lambda () ,@body)) table)))))
      (def "C-g" (signal 'quit))
      (def "C-x C-f" (open-mess main NIL))
      (def "C-x C-\\s" (save-mess main T))
      (def "C-x C-w" (save-mess main NIL))
      (def "C-x C-c" (q+:close main))
      (def "C-x h" (q+:select-all editor))
      (def "C-x n" (open-mess main :new))
      (def "C-_" (q+:undo editor))
      (def "M-x" (prompt status (lambda (string)
                                  (funcall (read-safely string) main))
                         "Call:"))
      (def "M-:" (prompt status (lambda (string)
                                  (eval (read-safely string)))
                         "Eval:"))
      (def "C-y" (q+:paste editor))
      (def "C-w" (q+:cut editor))
      (def "M-w" (q+:copy editor)))))
