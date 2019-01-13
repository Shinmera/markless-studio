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
           (T (restart-case (progn (warn "Unknown key sequence ~s" key) :?)
                (use-value (value)
                  :report "Supply a key value."
                  :interactive (lambda () (read *query-io*))
                  value)))))))

(defun print-key (key stream)
  (case key
    (:ctrl (write-char #\C stream))
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
  (loop for i from 0 below (length keychord)
        do (loop for (key . rest) on (aref keychord i)
                 do (print-key key stream)
                    (when rest (write-char #\- stream)))
           (when (< (1+ i) (length keychord))
             (write-char #\  stream))))

(defclass keychord ()
  ((keychord :initform #() :reader keychord)
   (action :initarg :action :reader action)))

(defmethod initialize-instance :after ((keychord keychord) &key chord)
  (setf (slot-value keychord 'keychord) (etypecase chord
                                          (string (parse-keychord chord))
                                          (vector chord))))

(defmethod print-object ((keychord keychord) stream)
  (print-unreadable-object (keychord stream :type T)
    (print-keychord (keychord keychord) stream)))

(defun make-keychord (chord action)
  (make-instance 'keychord :chord chord :action action))

(defmethod process ((keychord keychord) pressed index key dir)
  (let* ((groups (keychord keychord))
         (group (aref groups index)))
    (flet ((advance ()
             (cond ((<= (length groups) (1+ index))
                    (funcall (action keychord))
                    (values 0 T))
                   (T
                    (values (1+ index) T)))))
      (if (rest group)
          (ecase dir
            (:dn (if (find key group)
                     (values index T)
                     0))
            (:up (if (loop for key in group
                           always (find key pressed))
                     (advance)
                     0)))
          (ecase dir
            (:dn (if (find (first group) pressed)
                     (advance)
                     0))
            (:up index))))))

(defclass keychord-table ()
  ((keychords :initarg :keychords :initform () :accessor keychords)
   (pressed :initform () :accessor pressed)))

(defmethod update ((table keychord-table) key dir)
  (let ((key (map-key key))
        (found-match NIL))
    (when (eq dir :dn)
      (pushnew key (pressed table)))
    (loop for cons in (keychords table)
          for (index . keychord) = cons
          for (next-index matched) = (multiple-value-list
                                      (process keychord (pressed table) index key dir))
          do (setf (car cons) next-index)
             (when matched (setf found-match T)))
    (when (eq dir :up)
      (setf (pressed table) (delete key (pressed table))))
    found-match))

(defmethod install ((keychord keychord) (table keychord-table))
  ;; FIXME: check for collisions
  (push (cons 0 keychord) (keychords table)))

(defmethod uninstall ((keychord keychord) (table keychord-table))
  (setf (keychords table) (delete keychord (keychords table) :key #'cdr)))
