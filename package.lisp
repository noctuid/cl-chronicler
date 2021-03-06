(in-package #:cl-user)

(defpackage #:chronicler
  (:use #:cl)
  (:import-from #:alexandria
                #:eswitch
                #:emptyp
   :read-file-into-string)
  (:import-from #:cl-ppcre
                #:regex-replace
                #:regex-replace-all
                #:scan
                #:scan-to-strings
                #:do-matches-as-strings
                #:split)
  (:import-from #:local-time
                #:format-timestring
                #:timestamp-
                #:now)
  (:import-from #:uiop/filesystem
                #:directory-files)
  (:import-from #:cl-store
                #:store
                #:restore)
  (:import-from #:unix-opts
                #:define-opts
                #:option
                #:skip-option
                #:unknown-option
                #:get-opts
                #:missing-arg
                #:arg-parser-failed
                #:raw-arg)
  (:import-from #:cl-arrows
                #:->>
                #:-<>>)
  (:import-from #:metabang-bind
                #:bind)
  (:shadowing-import-from #:unix-opts
                          #:describe)
  (:export #:main))
