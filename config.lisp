#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.studio)

(defun config-file ()
  #+(or windows win32 mswindows)
  (merge-pathnames (make-pathname :name "config" :type "lisp" :directory '(:relative "AppData" "Local" "markless-studio"))
                   (user-homedir-pathname))
  #-(or windows win32 mswindows)
  (merge-pathnames (make-pathname :name "config" :type "lisp" :directory '(:relative ".config" "markless-studio"))
                   (user-homedir-pathname)))

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

(defun load-config (main &optional (file (config-file)))
  (with-open-file (stream file :if-does-not-exist NIL)
    (if stream
        (with-standard-io-syntax
          (loop with *package* = #.*package*
                with *main* = main
                for form = (read stream NIL #1=(make-symbol "EOF"))
                until (eq form #1#)
                do (eval form)))
        (warn "Config file ~s does not exist." file))))
