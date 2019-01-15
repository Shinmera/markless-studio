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
   (keytable-suppressed-p :initform NIL :accessor keytable-suppressed-p)
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
  (when (and (not (keytable-suppressed-p main))
             (or (enum-equal (q+:type ev) (q+:qevent.key-press))
                 (enum-equal (q+:type ev) (q+:qevent.key-release))))
    (handle-keychord-events main (cast "QKeyEvent" ev))))

(defun find-any (items sequence)
  (loop for item in sequence
        thereis (find item items)))

(defmethod handle-keychord-events ((main main) ev)
  (let ((dir (qtenumcase (q+:type ev)
               ((q+:qevent.key-press) :dn)
               ((q+:qevent.key-release) :up)
               (T (error "Wtf"))))
        (status (slot-value main 'status)))
    (handler-bind ((quit (lambda (_) (quit status))))
      (unless (q+:is-auto-repeat ev)
        (prog1 (find-any '(:control :alt :hyper :meta :super) (pressed (keytable main)))
          (let* ((key (qt-key->key (q+:key ev) (q+:modifiers ev)))
                 (matched (update-keychords (keytable main) key dir)))
            (loop for keychord in matched
                  thereis (maybe-invoke keychord))))))))

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

(defmethod export-mess ((main main) (profile (eql NIL)))
  (let ((profile (open-export-profile)))
    (when profile (export-mess main profile))))

(defmethod export-mess ((main main) (profile (eql T)))
  (export-mess main (export-profile main)))

(defmethod export-mess ((main main) (profile export-profile))
  (setf (export-profile main) profile)
  (export-mess (q+:to-plain-text (slot-value main 'editor)) profile))

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
  (:item "E&xport As..."
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
  (:item "&About" (about))
  (:item "&Help" (help)))

(defun help ()
  (q+:qdesktopservices-open-url (q+:make-qurl "https://shinmera.github.io/markless-studio")))

(defun about ()
  (let ((studio (asdf:find-system :markless-studio))
        (implementation (asdf:find-system :cl-markless)))
    (with-finalizing ((box (q+:make-qmessagebox)))
      (setf (q+:window-title box) "About Markless-Studio")
      (setf (q+:text box) (cl-who:with-html-output-to-string (_)
                            (:h1 "Markless Studio")
                            (:p (cl-who:str (asdf:system-description studio)))
                            (:p "The source code is openly available and licensed under the "
                                (cl-who:str (asdf:system-license studio))
                                " licence.")
                            (:p "Homepage: " (cl-who:str (asdf:system-homepage studio)) (:br)
                                "Author: " (cl-who:str (asdf:system-author studio)) (:br)
                                "Version: " (cl-who:str (asdf:component-version studio)) (:br)
                                "Cl-Markless: " (cl-who:str (asdf:component-version implementation)) (:br)
                                "Lisp: " (cl-who:str (lisp-implementation-type))
                                " " (cl-who:str (lisp-implementation-version)))))
      (q+:exec box))))

(defun describe-key (key)
  (with-finalizing ((box (q+:make-qmessagebox)))
    (setf (q+:window-title box) (with-output-to-string (o)
                                  (print-keychord (keychord key) o)))
    (setf (q+:text box) (cl-who:with-html-output-to-string (o)
                          (:h1 (print-keychord (keychord key) o))
                          (:p "Is bound to " (cl-who:str (action key)) ".")
                          (:p (cl-who:str (documentation (action key) 'cl:function)))))
    (q+:exec box)))

(defun describe-command (command)
  (with-finalizing ((box (q+:make-qmessagebox)))
    (setf (q+:window-title box) (princ-to-string command))
    (setf (q+:text box) (cl-who:with-html-output-to-string (_)
                          (:h1 (cl-who:str command))
                          (:p (cl-who:str (documentation command 'cl:function)))))
    (q+:exec box)))

(defun make-emacs-keytable (main &optional (table (make-instance 'keychord-table)))
  (with-slots-bound (main main)
    (macrolet ((def (chord &body body)
                 (cond ((symbolp (first body))
                        `(install (make-keychord ,chord ',(command-name (first body))) table))
                       (T
                        `(install (make-keychord ,chord (lambda () ,@body)) table)))))
      (def "C-g" quit)
      (def "C-x C-f" open-file)
      (def "C-x C-\\s" save-file)
      (def "C-x C-w" save-file-as)
      (def "C-x C-c" exit)
      (def "C-x C-e" export)
      (def "C-x C-M-e" export-as)
      (def "C-x h" select-all)
      (def "C-<space>" start-selection)
      (def "C-_" undo)
      (def "M-x" call)
      (def "M-:" eval)
      (def "C-y" paste)
      (def "C-w" cut)
      (def "M-w" copy)
      (def "C-h a" show-about)
      (def "C-h h" show-help)
      (def "C-h k" describe-key)
      (def "C-h c" describe-command))))

;; FIXME: key listing
;; FIXME: user config file

(defun config-file ()
  #+(or windows win32 mswindows)
  (merge-pathnames (make-pathname :name "config" :type "lisp" :directory '(:relative "AppData" "Local" "markless-studio"))
                   (user-homedir-pathname))
  #-(or windows win32 mswindows)
  (merge-pathnames (make-pathname :name "config" :type "lisp" :directory '(:relative ".config" "markless-studio"))
                   (user-homedir-pathname)))
