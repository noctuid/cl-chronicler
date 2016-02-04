(in-package :chronicler)

;;; Generic Heading Class and Methods
;; https://stackoverflow.com/questions/3210177/in-common-lisp-how-to-define-a-generic-data-type-specifier-like-list-of-intege
(defun elements-are-of-type (seq type)
  (every #'(lambda (x) (typep x type)) seq))

(deftype list-of-type (type)
  (let ((predicate (gensym)))
    (setf (symbol-function predicate)
          #'(lambda (seq) (elements-are-of-type seq type)) )
    `(and list (satisfies ,predicate))))

(defclass heading ()
  ((id
    :type (or symbol null)
    :initarg :id
    :initform nil
    :documentation "Heading id or name.")
   (word-count
    :type integer
    :initarg :word-count
    :initform 0
    :accessor word-count
    :documentation "Number of words directly under the heading.")
   (sub-headings
    :type (or (list-of-type heading) null)
    :accessor sub-headings
    :initform nil
    :documentation "List of heading objects that are sub-headings.")
   (ignored
    :type boolean
    :initarg :ignored
    :initform *default-ignore-behavior*
    :accessor ignored
    :documentation "Whether to ignore the content directly under the heading.")
   (ignored-recursively
    :type boolean
    :initarg :ignored-recursively
    :initform *default-ignore-behavior*
    :accessor ignored-recursively
    :documentation
    "Whether to ignore the content under all sub-headings by default.")))

(defgeneric total-word-count (heading)
  (:documentation
   "Get the total word count of a heading including sub-heading content."))
(defmethod total-word-count ((heading heading))
  (let ((wc (word-count heading))
        (sub-headings (sub-headings heading)))
    (dolist (sub-heading sub-headings)
      (let ((additional-wc (total-word-count sub-heading)))
        (unless (ignored sub-heading)
          (incf wc additional-wc))))
    wc))
