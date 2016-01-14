(asdf:defsystem :chronicler
  :description "Track and analyze writing statistics for emacs org files."
  :author "Lit Wakefield <noct@openmailbox.org>"
  :homepage "https://github.com/noctuid/cl-chronicler"
  :license "GPL3"
  :version "0.1"
  :depends-on (:cl-ppcre :unix-opts :cl-store :local-time)
  :serial t
  :components ((:file "package")
               (:file "option-defaults")
               (:file "util")
               (:file "counts")
               (:file "org-parse")
               (:file "store-and-query")
               (:file "cli-parse")))
