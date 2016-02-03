(in-package :chronicler)

;;; Counting Functions 
(defun count-words (text)
  "Return the number of words in TEXT."
  (length (remove-if #'emptyp
                     (cl-ppcre:split "\\s+" text))))
