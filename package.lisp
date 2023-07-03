(in-package #:cl-user)
(defpackage #:markless-studio
  (:nicknames #:org.shirakumo.markless.studio)
  (:use #:cl+qt)
  ;; main.lisp
  (:export
   #:start))

(unless (find-package '#:org.shirakumo.markless.studio.commands)
  (make-package '#:markless-studio-commands
                :nicknames '(#:org.shirakumo.markless.studio.commands)
                :use ()))
