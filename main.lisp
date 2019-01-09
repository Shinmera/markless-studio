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
  (multiple-value-bind (ast conditions) (parse-safely (q+:to-plain-text editor))
    (clear-conditions editor)
    (when ast
      (setf (content viewer) ast))
    (dolist (condition conditions)
      (markup-condition editor condition))))

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
  (:item ("&About" (ctrl h))
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
