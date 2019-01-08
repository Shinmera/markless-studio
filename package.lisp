#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:markless-studio
  (:nicknames #:org.shirakumo.markless.studio)
  (:use #:cl+qt)
  ;; main.lisp
  (:export
   #:start))
