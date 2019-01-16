#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.studio)
(in-readtable :qtools)

(defun config-directory ()
  #+(or windows win32 mswindows)
  (merge-pathnames (make-pathname :directory '(:relative "AppData" "Local" "markless-studio"))
                   (user-homedir-pathname))
  #-(or windows win32 mswindows)
  (merge-pathnames (make-pathname :directory '(:relative ".config" "markless-studio"))
                   (user-homedir-pathname)))

(defun config-file (&optional name type)
  (make-pathname :name name :type type :defaults (config-directory)))

(defmacro define-key (chord &body body)
  (cond ((symbolp (first body))
         `(install (make-keychord ,chord ',(command-name (first body))) (keytable *main*)))
        (T
         `(install (make-keychord ,chord (lambda () ,@body)) (keytable *main*)))))

(defun remove-key (chord)
  (uninstall chord (keytable *main*)))

(defmacro define-key-map (&body maps)
  `(setf (keymap (keytable *main*))
         (lambda (key)
           (case key
             ,@maps
             (T key)))))

(defun set-font (description)
  (let ((font (q+:make-qfont)))
    (q+:from-string font description)
    (setf (q+:font (slot-value *main* 'editor)) font)))

(defun load-config (main &optional (file (config-file "config" "lisp")))
  (with-open-file (stream file :if-does-not-exist NIL)
    (if stream
        (with-standard-io-syntax
          (loop with *package* = #.*package*
                with *main* = main
                for form = (read stream NIL '#1=(make-symbol "EOF"))
                until (eq form '#1#)
                do (eval form)))
        (warn "Config file ~s does not exist." file))))

(defparameter *config-separator* ";;; ~~~")

(defun read-user-config-lines (file)
  (let ((start ())
        (end ()))
    (with-standard-io-syntax
      (with-open-file (stream file :direction :input
                                   :if-does-not-exist :create)
        (loop for line = (read-line stream NIL)
              until (or (null line) (starts-with *config-separator* line))
              do (push line start))
        (loop for line = (read-line stream NIL)
              until (or (null line) (starts-with *config-separator* line)))
        (loop for line = (read-line stream NIL)
              until (null line)
              do (push line end))))
    (values start end)))

(defun write-config-lines (file start settings end)
  (with-standard-io-syntax
    (with-open-file (stream file :direction :output
                                 :if-exists :supersede)
      (loop for line in start
            do (write-line line stream))
      (format stream "~&~a Begin Auto Generated Settings~%" *config-separator*)
      (loop with *package* = #.*package*
            for setting in settings
            do (fresh-line stream)
               (write setting :stream stream
                              :case :downcase
                              :readably T)
               (terpri stream))
      (format stream "~&~a End Auto Generated Settings~%" *config-separator*)
      (loop for line in end
            do (write-line line stream)))))

(defun compile-config-settings (main)
  (list
   `(set-font ,(q+:to-string (q+:font (slot-value main 'editor))))))

(defun save-config (main &optional (file (config-file "config" "lisp")))
  (multiple-value-bind (start end) (read-user-config-lines file)
    (write-config-lines file start (compile-config-settings main) end)))
