(in-package :chronicler)

(defun parse-markup-file (file)
  "Determine FILE's extension and dispatch to the appropriate parsing function."
  (eswitch ((pathname-type file) :test #'equal)
           ("org" (parse-org-file file))))
