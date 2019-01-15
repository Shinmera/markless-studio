#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.studio)

(defun map-key (key)
  (case key
    (:alt :meta)
    (:meta :control)
    (#\< :meta)
    (T key)))

(defun parse-key (key)
  (etypecase key
    (character
     (case key
       (#\C :control)
       (#\M :meta)
       (#\S :shift)
       (#\s :super)
       (#\A :alt)
       (#\H :hyper)
       (T key)))
    (string
     (cond ((string-equal key "ctrl") :control)
           ((string-equal key "meta") :meta)
           ((string-equal key "shift") :shift)
           ((string-equal key "alt") :alt)
           ((string-equal key "hyper") :hyper)
           ((string-equal key "super") :super)
           ((string-equal key "spc") #\ )
           ((string-equal key "space") #\ )
           ((string-equal key "pgup") :page-up)
           ((string-equal key "pgdn") :page-down)
           ((string-equal key "tab") :tab)
           ((string-equal key "caps") :caps-lock)
           ((string-equal key "capslk") :caps-lock)
           ((string-equal key "esc") :escape)
           ((string-equal key "escape") :escape)
           ((string-equal key "ret") :return)
           ((string-equal key "return") :return)
           ((string-equal key "enter") :return)
           ((= 1 (length key)) (char key 0))
           (T (restart-case (progn (warn "Unknown key sequence ~s" key) :?)
                (use-value (value)
                  :report "Supply a key value."
                  :interactive (lambda () (read *query-io*))
                  value)))))))

(defun print-key (key stream)
  (case key
    (:control (write-char #\C stream))
    (:meta (write-char #\M stream))
    (:shift (write-char #\S stream))
    (:super (write-char #\s stream))
    (:alt (write-char #\A stream))
    (:hyper (write-char #\H stream))
    (#\C (write-string "\\C" stream))
    (#\M (write-string "\\M" stream))
    (#\S (write-string "\\S" stream))
    (#\A (write-string "\\A" stream))
    (#\H (write-string "\\H" stream))
    (#\s (write-string "\\s" stream))
    (#\  (write-string "<spc>" stream))
    (T (etypecase key
         (keyword (format stream "<~(~a~)>" key))
         (character (write-char key stream))))))

(defun parse-keychord (chord)
  (with-input-from-string (stream chord)
    (let ((ast (make-array 0 :adjustable T :fill-pointer T))
          (group ())
          (buffer (make-string-output-stream)))
      (flet ((commit-key (string)
               (push (parse-key string) group))
             (commit-group ()
               (when group
                 (vector-push-extend (nreverse group) ast)
                 (setf group NIL)))
             (parse-next (char)
               (case char
                 (#\<
                  (loop for char = (read-char stream)
                        until (char= char #\>)
                        do (write-char char buffer))
                  (get-output-stream-string buffer))
                 (#\\
                  (string (read-char stream)))
                 (T
                  char))))
        (loop for char = (read-char stream NIL)
              while char
              do (case char
                   (#\ 
                    (commit-group))
                   (#\-
                    (commit-key (parse-next (read-char stream))))
                   (T
                    (commit-group)
                    (commit-key (parse-next char)))))
        (commit-group)
        ast))))

(defun print-keychord (keychord stream)
  (etypecase stream
    (stream
     (loop for i from 0 below (length keychord)
           do (loop for (key . rest) on (aref keychord i)
                    do (print-key key stream)
                       (when rest (write-char #\- stream)))
              (when (< (1+ i) (length keychord))
                (write-char #\  stream))))
    ((eql NIL)
     (with-output-to-string (stream)
       (print-keychord keychord stream)))
    ((eql T)
     (print-keychord keychord *standard-output*))))

(defmethod keychord= ((a vector) (b vector))
  (and (= (length a) (length b))
       (loop for ai across a
             for bi across b
             always (equal ai bi))))

(defclass keychord ()
  ((index :initform 0 :accessor index)
   (groups :initform #() :reader groups)
   (action :initarg :action :reader action)))

(defmethod initialize-instance :after ((keychord keychord) &key chord)
  (setf (slot-value keychord 'groups) (etypecase chord
                                        (string (parse-keychord chord))
                                        (vector chord))))

(defmethod print-object ((keychord keychord) stream)
  (print-unreadable-object (keychord stream :type T)
    (print-keychord (groups keychord) stream)))

(defun make-keychord (chord action)
  (make-instance 'keychord :chord chord :action action))

(defmethod update-keychord ((keychord keychord) pressed key dir)
  (let* ((index (index keychord))
         (group (aref (groups keychord) index)))
    (setf (index keychord)
          (if (rest group)
              (ecase dir
                (:dn (if (find key group)
                         (1+ index)
                         0))
                (:up (if (loop for key in group
                               always (find key pressed))
                         (1+ index)
                         0)))
              (ecase dir
                (:dn (if (find (first group) pressed)
                         (1+ index)
                         0))
                (:up index))))
    (< 0 (index keychord))))

(defmethod complete-p ((keychord keychord))
  (<= (length (groups keychord)) (index keychord)))

(defmethod invoke ((keychord keychord) &rest args)
  (apply (action keychord) args))

(defmethod maybe-invoke ((keychord keychord) &rest args)
  (when (complete-p keychord)
    (apply #'invoke keychord args)
    (reset keychord)))

(defmethod reset ((keychord keychord))
  (setf (index keychord) 0)
  keychord)

(defclass keychord-table ()
  ((keychords :initarg :keychords :initform () :accessor keychords)
   (pressed :initform () :accessor pressed)))

(defmethod update-keychords ((table keychord-table) key dir)
  (let ((key (map-key key)))
    (loop initially (when (eq dir :dn)
                      (pushnew key (pressed table)))
          for keychord in (keychords table)
          for matched = (update-keychord (pressed table) key dir)
          when matched collect keychord
          finally (when (eq dir :up)
                    (setf (pressed table) (delete key (pressed table)))))))

(defmethod reset ((table keychord-table))
  (mapc #'reset (keychords table))
  table)

(defmethod install ((keychord keychord) (table keychord-table))
  ;; FIXME: check for collisions
  (push keychord (keychords table)))

(defmethod uninstall ((keychord keychord) (table keychord-table))
  (setf (keychords table) (delete keychord (keychords table))))

(defmethod find-keychord ((string string) (table keychord-table))
  (find-keychord (parse-keychord string) table))

(defmethod find-keychord ((vector vector) (table keychord-table))
  (loop for keychord in (keychords table)
        when (keychord= vector (keychord keychord))
        return keychord))
