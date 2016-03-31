(in-package #:cl-user)
(defpackage #:chronicler-tests-asd
  (:use #:cl #:asdf))
(in-package #:chronicler-tests-asd)

(defsystem :chronicler-tests
  :license "GPL3"
  :depends-on (:chronicler :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:test-file "tests"))
  :description "Test system for chronicler"
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
