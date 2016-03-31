(in-package #:chronicler)

;;; Counting Functions
(defun count-words (text)
  "Return the number of words in TEXT."
  (->> text
       (cl-ppcre:split "\\s+")
       (remove-if #'emptyp)
       (length)))
