#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem markless-studio
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An application for writing and publishing Markless documents."
  :homepage "https://Shinmera.github.io/markless-studio/"
  :bug-tracker "https://github.com/Shinmera/markless-studio/issues"
  :source-control (:git "https://github.com/Shinmera/markless-studio.git")
  :serial T
  :components ((:file "package")
               (:file "keychords")
               (:file "viewer")
               (:file "editor")
               (:file "status")
               (:file "main"))
  :depends-on (:qtools
               :qtcore
               :qtgui
               :cl-markless-plump
               :cl-markless-epub
               :cl-markless-markdown
               :cl-ppcre)
  :build-operation "qt-program-op"
  :build-pathname "markless-studio"
  :entry-point "markless-studio:start")
