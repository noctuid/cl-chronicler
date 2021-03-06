(in-package #:cl-user)
(defpackage #:chronicler-asd
  (:use #:cl #:asdf))
(in-package #:chronicler-asd)

(asdf:defsystem :chronicler
  :description "Track and analyze writing statistics for emacs org files."
  :author "Fox Kiester <noct@openmailbox.org>"
  :homepage "https://github.com/noctuid/cl-chronicler"
  :license "GPL3"
  :version "0.1"
  :depends-on (:alexandria
               :cl-ppcre
               :unix-opts
               :cl-store
               :local-time
               :cl-arrows
               :metabang-bind)
  :serial t
  :components ((:file "package")
               (:file "option-defaults")
               (:file "util")
               (:file "counts")
               (:file "markup")
               (:file "org-parse")
               (:file "generic-parse")
               (:file "store-and-query")
               (:file "cli-parse")))
