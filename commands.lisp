#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.studio)
(in-readtable :qtools)

(defun read-safely (string &optional (package #.*package*))
  (with-standard-io-syntax
    (let ((*package* (etypecase package
                       ((or symbol string) (find-package package))
                       (package package)))
          (*read-eval* NIL))
      (read-from-string string))))

(defun command-name (name)
  (intern (string name) '#:org.shirakumo.markless.studio.commands))

(defmacro define-editor-command (name (main &rest slots) &body body)
  (let ((name (command-name name)))
    `(progn
       (export ',name '#:org.shirakumo.markless.studio.commands)
       (defun ,name (,main)
         ,@(when (stringp (first body)) (list (first body)))
         (with-slots ,slots ,main
           ,@body)))))

(define-editor-command quit (main)
  "Abort the current command."
  (signal 'quit))

(define-editor-command open-file (main)
  "Open a new file."
  (open-mess main NIL))

(define-editor-command new-file (main editor)
  "Clear the editor and start a new file."
  (setf (source-file main) NIL)
  (q+:clear editor))

(define-editor-command save-file (main)
  "Save the current file."
  (save-mess main T))

(define-editor-command save-file-as (main)
  "Save the current file in a new place."
  (save-mess main NIL))

(define-editor-command export (main)
  "Export the document."
  (export-mess main T))

(define-editor-command export-as (main)
  "Export the document to another format."
  (export-mess main NIL))

(define-editor-command exit (main)
  "Exit markless-studio."
  (q+:close main))

(define-editor-command select-all (main editor)
  "Select all source code in the editor."
  (q+:select-all editor))

(define-editor-command undo (main editor)
  "Undo the last edit."
  (q+:undo editor))

(define-editor-command paste (main editor)
  "Paste the clipboard into the editor."
  (q+:paste editor))

(define-editor-command cut (main editor)
  "Cut the current selection from the editor."
  (q+:cut editor))

(define-editor-command copy (main editor)
  "Copy the current selection from the editor."
  (q+:copy editor))

(define-editor-command call (main status)
  "Run an editor command."
  (prompt status (lambda (string)
                   (funcall (read-safely string '#:org.shirakumo.markless.studio.commands) main))
          "Call:"))

(define-editor-command eval (main status)
  (prompt status (lambda (string)
                   (eval (read-safely string)))
          "Eval:"))
