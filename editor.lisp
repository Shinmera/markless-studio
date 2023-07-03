(in-package #:org.shirakumo.markless.studio)
(in-readtable :qtools)

(define-widget highlighter (QSyntaxHighlighter)
  ((editor :initarg :editor :accessor editor)))

(defmethod construct ((highlighter highlighter))
  (new highlighter (editor highlighter)))

(define-subwidget (highlighter keywords) (q+:make-qtextcharformat)
  (setf (q+:foreground keywords) (q+:brush (q+:palette editor) (q+:qpalette.disabled) (q+:qpalette.text))))

(define-subwidget (highlighter instruction) (q+:make-qtextcharformat)
  (setf (q+:foreground instruction) (q+:make-qbrush (q+:make-qcolor 200 0 0))))

(define-subwidget (highlighter embed) (q+:make-qtextcharformat)
  (setf (q+:foreground embed) (q+:make-qbrush (q+:make-qcolor 0 150 255))))

(define-subwidget (highlighter code) (q+:make-qtextcharformat)
  (setf (q+:foreground code) (q+:make-qbrush (q+:make-qcolor 250 160 40))))

(define-subwidget (highlighter url) (q+:make-qtextcharformat)
  (setf (q+:font-underline url) T))

(define-override (highlighter highlight-block) (text)
  (cond ((< (q+:previous-block-state highlighter) 2)
         (cl-ppcre:do-scans (s e rs re "(^(\\[ |- |\\d+\\.|#+ |;+ ))|(\\*\\*|//|__|<-|->|\\\\)" text)
           (setf (q+:format highlighter) (values s (- e s) keywords)))
         (cl-ppcre:do-scans (s e rs re "[v^]\\(.*\\)" text)
           (setf (q+:format highlighter) (values s 2 keywords))
           (setf (q+:format highlighter) (values (- e 1) 1 keywords)))
         (cl-ppcre:do-scans (s e rs re "``.*``" text)
           (setf (q+:format highlighter) (values s 2 keywords))
           (setf (q+:format highlighter) (values (- e 2) 2 keywords))
           (setf (q+:format highlighter) (values (+ s 2) (- e s 4) code)))
         (cl-ppcre:do-scans (s e rs re "^(\\! ).*" text)
           (setf (q+:format highlighter) (values s (- e s) instruction)))
         (cl-ppcre:do-scans (s e rs re "^(\\[ ).*" text)
           (setf (q+:format highlighter) (values s (- e s) embed)))
         (cl-ppcre:do-scans (s e rs re "^(::+).*" text)
           (setf (q+:format highlighter) (values s (- e s) keywords))
           (setf (q+:current-block-state highlighter) (- (aref re 0) (aref rs 0))))
         (cl-ppcre:do-scans (s e rs re "\\w[\\d\\w+\\-.]*://[\\d\\w$\\-_.+!*'()&,/:;=?@%#\\\\]+" text)
           (setf (q+:format highlighter) (values s (- e s) url))))
        ((and (= (q+:previous-block-state highlighter) (length text))
              (every (lambda (c) (char= #\: c)) text))
         (setf (q+:format highlighter) (values 0 (length text) keywords))
         (setf (q+:current-block-state highlighter) 0))
        (T
         (setf (q+:format highlighter) (values 0 (length text) code))
         (setf (q+:current-block-state highlighter) (q+:previous-block-state highlighter)))))

(define-widget line-number-area (QWidget)
  ((editor :initarg :editor :accessor editor)))

(defmethod construct ((line-number-area line-number-area))
  (new line-number-area (editor line-number-area)))

(define-override (line-number-area size-hint) ()
  (q+:make-qsize (margin editor) 0))

(define-override (line-number-area paint-event) (ev)
  (with-finalizing ((painter (q+:make-qpainter line-number-area)))
    (let ((palette (q+:palette line-number-area)))
      (q+:fill-rect painter (q+:rect ev) (q+:color palette (q+:qpalette.disabled) (q+:qpalette.alternate-base)))
      (setf (q+:pen painter) (q+:color palette (q+:qpalette.disabled) (q+:qpalette.text)))
      (loop for block = (q+:first-visible-block editor) then (q+:next block)
            for top = (q+:top (q+:translated (q+:block-bounding-geometry editor block)
                                             (q+:content-offset editor)))
            for bottom = (+ top (q+:height (q+:block-bounding-rect editor block)))
            while (and (q+:is-valid block) (<= top (q+:bottom (q+:rect ev))))
            do (when (and (q+:is-visible block) (< (q+:top (q+:rect ev)) bottom))
                 (q+:draw-text painter 0 (ceiling top) (- (q+:width line-number-area) 2) (q+:height (q+:font-metrics editor))
                               (q+:qt.align-right) (princ-to-string (1+ (q+:block-number block)))))))))

(define-widget editor (QPlainTextEdit)
  ((in-selection :initform NIL :accessor in-selection)))

(define-initializer (editor setup)
  (setf (q+:font editor) (q+:make-qfont "monospace")))

(define-subwidget (editor error-format) (q+:make-qtextcharformat)
  (setf (q+:underline-style error-format) (q+:qtextcharformat.wave-underline))
  (setf (q+:underline-color error-format) (q+:make-qcolor 255 0 0)))

(define-subwidget (editor line-number-area) (make-instance 'line-number-area :editor editor))

(define-subwidget (editor highlighter) (make-instance 'highlighter :editor editor)
  (setf (q+:document highlighter) (q+:document editor)))

(defmethod margin ((editor editor))
  (+ (* (ceiling (log (+ 10 (q+:block-count editor)) 10))
        (q+:width (q+:font-metrics editor) "9"))
     5 2))

(defmethod clear-conditions ((editor editor))
  ;;(mapcar #'finalize (q+:extra-selections editor))
  (setf (q+:extra-selections editor) NIL))

(defmethod markup-condition ((editor editor) condition))

(defmethod markup-condition ((editor editor) (condition cl-markless:parser-condition))
  (let ((cursor (q+:make-qtextcursor (q+:document editor)))
        (selection (q+:make-qtextedit-extraselection)))
    (q+:move-position cursor (q+:qtextcursor.next-block) (q+:qtextcursor.move-anchor) (cl-markless:line condition))
    (cond ((= 0 (cl-markless:cursor condition))
           (q+:select cursor (q+:qtextcursor.line-under-cursor)))
          (T
           (q+:move-position cursor (q+:qtextcursor.next-character) (q+:qtextcursor.move-anchor) (cl-markless:cursor condition))
           (q+:select cursor (q+:qtextcursor.word-under-cursor))))
    (setf (q+:cursor selection) cursor)
    (setf (q+:format selection) (slot-value editor 'error-format))
    (setf (q+:extra-selections editor) (list* selection (q+:extra-selections editor)))))

(define-slot (editor new-block) ((blocks int))
  (declare (connected editor (block-count-changed int)))
  (setf (q+:viewport-margins editor) (values (margin editor) 0 0 0)))

(define-slot (editor update-line-number-area) ((rect "QRect") (dy int))
  (declare (connected editor (update-request "QRect" int)))
  (if (= 0 dy)
      (q+:update line-number-area 0 (q+:y rect) (q+:width line-number-area) (q+:height rect))
      (q+:scroll line-number-area 0 dy))
  (when (q+:contains rect (q+:rect (q+:viewport editor)))
    (%editor-slot-new-block editor 0)))

(define-override (editor resize-event) (ev)
  (call-next-qmethod)
  (let ((r (q+:contents-rect editor)))
    (setf (q+:geometry line-number-area) (q+:make-qrect (q+:left r) (q+:top r) (round (margin editor)) (q+:height r)))))

;; FIXME: add methods for cursor movements that respect in-selection

(define-override (editor key-press-event) (ev)
  (cond (in-selection
         (let ((cursor (q+:text-cursor editor)))
           (qtenumcase (q+:key ev)
             ((q+:qt.key_left)
              (q+:move-position cursor (q+:qtextcursor.left) (q+:qtextcursor.keep-anchor))
              (setf (q+:text-cursor editor) cursor))
             ((q+:qt.key_right)
              (q+:move-position cursor (q+:qtextcursor.right) (q+:qtextcursor.keep-anchor))
              (setf (q+:text-cursor editor) cursor))
             ((q+:qt.key_up)
              (q+:move-position cursor (q+:qtextcursor.up) (q+:qtextcursor.keep-anchor))
              (setf (q+:text-cursor editor) cursor))
             ((q+:qt.key_down)
              (q+:move-position cursor (q+:qtextcursor.down) (q+:qtextcursor.keep-anchor))
              (setf (q+:text-cursor editor) cursor))
             (T
              (setf in-selection NIL)
              (stop-overriding)))))
        (T (stop-overriding))))
