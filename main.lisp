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
  ((keytable :initform (make-instance 'keychord-table) :accessor keytable)))

(define-initializer (main setup)
  (q+:install-event-filter *qapplication* main)
  (make-emacs-keytable main (keytable main)))

(define-finalizer (main teardown)
  (q+:remove-event-filter *qapplication* main))

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

(define-override (main event-filter) (_ ev)
  (declare (ignore _))
  (qtenumcase (q+:type ev)
    ((q+:qevent.key-press)
     (let ((ev (cast "QKeyEvent" ev)))
       (unless (q+:is-auto-repeat ev)
         (update keytable (qt-key->key (q+:key ev) (q+:modifiers ev)) :dn))))
    ((q+:qevent.key-release)
     (let ((ev (cast "QKeyEvent" ev)))
       (unless (q+:is-auto-repeat ev)
         (update keytable (qt-key->key (q+:key ev) (q+:modifiers ev)) :up)))))
  NIL)

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

;; FIXME: this

(defmethod open-mess ((main main) (target (eql NIL))))

(defmethod open-mess ((main main) (target (eql T))))

(defmethod open-mess ((main main) (pathname pathname)))

(defmethod save-mess ((main main) (target (eql NIL))))

(defmethod save-mess ((main main) (target (eql T))))

(defmethod save-mess ((main main) (pathname pathname)))

(defmethod export-mess ((main main) (profile (eql NIL))))

(defmethod export-mess ((main main) (profile (eql T))))

;;(defmethod export ((main main) (profile profile)))

(define-menu (main file "&File")
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

(defun make-emacs-keytable (main &optional (table (make-instance 'keychord-table)))
  (with-slots-bound (main main)
    (macrolet ((def (chord &body body)
                 `(install (make-keychord ,chord (lambda () ,@body)) table)))
      (def "C-g" (message status "Abort."))
      (def "C-x C-f" (open-mess main NIL))
      (def "C-x C-\\s" (save-mess main T))
      (def "C-x C-w" (save-mess main NIL))
      (def "C-x C-c" (q+:close main))
      (def "C-_" (q+:undo editor))
      (def "M-:" (prompt status (lambda (string)
                                  (eval (read-from-string string)))
                         "Eval:"))
      (def "C-y" (q+:paste editor))
      (def "C-w" (q+:cut editor))
      (def "M-w" (q+:copy editor)))))
