#|
 This file is a part of markless-studio
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem markless-studio
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An application for writing and publishing Markless documents."
  :homepage "https://shinmera.github.io/markless-studio/"
  :bug-tracker "https://github.com/Shinmera/markless-studio/issues"
  :source-control (:git "https://github.com/Shinmera/markless-studio.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "keychords")
               (:file "key-table")
               (:file "file-dialog")
               (:file "input-widgets")
               (:file "export")
               (:file "export-editor")
               (:file "viewer")
               (:file "editor")
               (:file "status")
               (:file "commands")
               (:file "config")
               (:file "settings")
               (:file "main"))
  :depends-on (:qtools
               :qtcore
               :qtgui
               :clip
               :cl-markless-plump
               :cl-markless-epub
               :cl-markless-markdown
               :cl-ppcre
               :cl-who
               :closer-mop
               :qtools-ui-bytearray)
  :defsystem-depends-on (:qtools)
  :build-operation "qt-program-op"
  :build-pathname "markless-studio"
  :entry-point "markless-studio:start")
