(in-package #:org.shirakumo.markless.studio)

(defvar *key-name-map* (make-hash-table :test 'equalp))

(defun set-key-names (keysyms)
  (loop for (name keysym) in keysyms
        do (setf (gethash name *key-name-map*) keysym))
  *key-name-map*)

(set-key-names '(("ctrl" :control)
                 ("meta" :meta)
                 ("shift" :shift)
                 ("alt" :alt)
                 ("altgr" :altgr)
                 ("hyper" :hyper)
                 ("super" :super)
                 ("caps-lock" :caps-lock)
                 ("caps" :caps-lock)
                 ("tab" :tab)
                 ("esc" :escape)
                 ("escape" :escape)
                 ("f1" :f1)
                 ("f2" :f2)
                 ("f3" :f3)
                 ("f4" :f4)
                 ("f5" :f5)
                 ("f6" :f6)
                 ("f7" :f7)
                 ("f8" :f8)
                 ("f9" :f9)
                 ("f10" :f10)
                 ("f11" :f11)
                 ("f12" :f12)
                 ("print-screen" :print-screen)
                 ("print" :print-screen)
                 ("scroll-lock" :scroll-lock)
                 ("pause" :pause)
                 ("num-lock" :num-lock)
                 ("insert" :insert)
                 ("home" :home)
                 ("pgup" :page-up)
                 ("delete" :delete)
                 ("end" :end)
                 ("pgdn" :page-down)
                 ("backspace" :backspace)
                 ("ret" :return)
                 ("return" :return)
                 ("enter" :return)
                 ("left" :left)
                 ("right" :right)
                 ("up" :up)
                 ("down" :down)
                 ("spc" #\ )
                 ("space" #\ )))

(defun modifier-key-p (key)
  (find key '(:shift :control :alt :meta :hyper :super)))

(defun default-key-map (key)
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
     (cond ((gethash key *key-name-map*)
            (gethash key *key-name-map*))
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
       (keychord-prefix-p a b)))

(defun keychord-prefix-p (a b)
  (loop for ai across a
        for bi across b
        always (and (subsetp ai bi) (subsetp bi ai))))

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
  (let ((index (index keychord)))
    (when (< index (length (groups keychord)))
      (let ((group (aref (groups keychord) index)))
        (setf (index keychord)
              (if (rest group)
                  (ecase dir
                    (:dn (if (find key group)
                             index
                             0))
                    (:up (if (loop for key in group
                                   always (find key pressed))
                             (1+ index)
                             0)))
                  (ecase dir
                    (:dn (if (find (first group) pressed)
                             (1+ index)
                             (if (modifier-key-p key)
                                 index
                                 0)))
                    (:up index))))))
    (< 0 (index keychord))))

(defmethod complete-p ((keychord keychord))
  (<= (length (groups keychord)) (index keychord)))

(defmethod invoke ((keychord keychord) &rest args)
  (apply (action keychord) args))

(defmethod maybe-invoke ((keychord keychord) &rest args)
  (when (complete-p keychord)
    (reset keychord)
    (apply #'invoke keychord args)
    keychord))

(defmethod reset ((keychord keychord))
  (setf (index keychord) 0)
  keychord)

(defclass keychord-table ()
  ((keychords :initarg :keychords :initform () :accessor keychords)
   (pressed :initform () :accessor pressed)
   (keymap :initform #'default-key-map :accessor keymap)))

(defmethod update-keychords ((table keychord-table) key dir)
  (let ((key (funcall (keymap table) key)))
    (loop initially (when (eq dir :dn)
                      (pushnew key (pressed table)))
          for keychord in (keychords table)
          for matched = (update-keychord keychord (pressed table) key dir)
          when matched collect keychord
          finally (when (eq dir :up)
                    (setf (pressed table) (delete key (pressed table)))))))

(defmethod reset ((table keychord-table))
  (mapc #'reset (keychords table))
  (setf (pressed table) ())
  table)

(defmethod install ((keychord keychord) (table keychord-table))
  (loop for other in (keychords table)
        when (keychord-prefix-p (groups other) (groups keychord))
        do (error "Cannot install ~a as it would clobber ~a."
                  keychord other))
  (reset keychord)
  (push keychord (keychords table)))

(defmethod uninstall ((keychord keychord) (table keychord-table))
  (setf (keychords table) (delete keychord (keychords table))))

(defmethod find-keychord ((string string) (table keychord-table))
  (find-keychord (parse-keychord string) table))

(defmethod find-keychord ((vector vector) (table keychord-table))
  (loop for keychord in (keychords table)
        when (keychord= vector (groups keychord))
        return keychord))

(defmethod clear ((table keychord-table))
  (setf (keychords table) ())
  (setf (pressed table) ()))
