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

(defmacro define-editor-command (name (&optional (main (gensym "MAIN")) &rest slots) &body body)
  (let ((name (command-name name)))
    `(progn
       (export ',name '#:org.shirakumo.markless.studio.commands)
       (defun ,name ()
         ,@(when (stringp (first body)) (list (first body)))
         (let ((,main *main*))
           (with-slots ,slots ,main
             ,@body))))))

(define-editor-command quit ()
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

(define-editor-command start-selection (main editor)
  "Start a text selection at point."
  (setf (in-selection editor) T))

(define-editor-command call (main status)
  "Run an editor command."
  (with-prompt (string status "Call:")
    (funcall (read-safely string '#:org.shirakumo.markless.studio.commands))))

(define-editor-command eval (main status)
  "Evaluate a lisp form."
  (with-prompt (string status "Eval:")
    (eval (read-safely string))))

(define-editor-command show-about ()
  "Show the about dialog."
  (about))

(define-editor-command show-help ()
  "Show the help document."
  (help))

(define-editor-command describe-key (main status keytable)
  "Show information about a keybinding."
  (with-prompt (string status "Keychord:")
    (let ((keychord (find-keychord string keytable)))
      (if keychord
          (describe-key keychord)
          (message status "The keychord ~a is not bound." string)))))

(define-editor-command describe-command (main status)
  "Show information about an editor command."
  (with-prompt (string status "Command:")
    (let ((command (read-safely string '#:org.shirakumo.markless.studio.commands)))
      (if (fboundp command)
          (describe-command command)
          (message status "The command ~a is not defined." string)))))
