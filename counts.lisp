(in-package :chronicler)

;; TODO: attempt to make faster; test with sb-profile

;;; Org Heading Class and Methods
(defclass org-heading ()
  ((id
    :type (or string null)
    :initarg :id
    :initform nil
    :documentation "Org heading id.")
   (word-count
    :type integer
    :initarg :word-count
    :initform 0
    :accessor word-count
    :documentation "Number of words directly under the heading.")
   (sub-headings
    :type (or (list org-heading) null)
    :accessor sub-headings
    :initform nil
    :documentation "List of org-heading objects that are sub-headings.")
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
(defmethod total-word-count ((heading org-heading))
  (let ((wc (word-count heading))
        (sub-headings (sub-headings heading)))
    (dolist (sub-heading sub-headings)
      (let ((additional-wc (total-word-count sub-heading)))
        (unless (ignored sub-heading)
          (incf wc additional-wc))))
    wc))

;;; Counting and Comparison Functions 
(defun count-words (text)
  "Return the number of words in TEXT."
  (length (remove-if #'empty-string-p
                     (cl-ppcre:split "\\s+" text))))

(defun count-org-words (text)
  "Return the number of words in TEXT ignoring special org syntax."
  (count-words (remove-org-ignored-text text)))

(defun compare-days (accessor day1 day2)
  "Return the difference between DAY1 and DAY2 values as obtained with
ACCESSOR."
  (-
   (funcall accessor day1)
   (funcall accessor day2)))

;;; Getting and Persisting Information
(defun update-today-info (file)
  "Store and return org heading information obtained from FILE."
  (let* ((info (parse-org-file file))
         (store-dir (get-and-create-store-dir file))
         (today-file  (concatenate 'string store-dir (get-date-stamp 0)))
         (yesterday-file (concatenate 'string store-dir (get-date-stamp 1))))
    (unless (or (probe-file today-file)
                (probe-file yesterday-file)
                ;; TODO: use function that stops if even one file is found
                ;; if this could potentially take a significant amount of time
                (directory-files store-dir))
      ;; the first time a file is analyzed, put initial counts in a file
      ;; for yesterday as well so that total counts start at 0 for current day
      (store info yesterday-file))
    (store info today-file)))

(defun get-today-info (file &optional force-update)
  "Return org heading information obtained from the stored data for FILE.
The FILE will only be examined if FORCE-UPDATE is non-nil or if data does not
yet exist for today."
  (let* ((store-dir (get-and-create-store-dir file))
         (today-file (concatenate 'string store-dir (get-date-stamp 0)))
         (today-info
           (if (and (probe-file today-file)
                    (not force-update))
               (restore today-file)
               (update-today-info file)))
         ;; last time data stored may not be yesterday
         (previous-file (get-previous-date-file store-dir))
         (previous-info (when previous-file (restore previous-file))))
    (values today-info previous-info)))

(defun get-today-word-count (today-info &optional previous-info)
  "Return the number of words written today as obtained from TODAY-INFO.
If PREVIOUS-DAY-INFO is non-nil the word count will be calculated as
the total number of words existing today minus the total number of words
that existed yesterday. Otherwise, the total number of words existing will be
returned."
  (if previous-info
      (compare-days
       #'total-word-count
       (gethash 'root today-info)
       (gethash 'root previous-info))
      (total-word-count (gethash 'root today-info))))
