(in-package :chronicler)

(defun empty-string-p (string)
  "Return nil if STRING has 0 length."
  (= (length string) 0))

(defun get-file-as-string (file)
  "Return a string corresponding to the full contents of FILE."
  (with-open-file (stream file)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun replace-/-with-! (path)
  "Replace all '/' characters in PATH with '!' characters."
  (regex-replace-all "/" path "!"))

(defun ensure-trailing-/ (path)
  "Ensure that the string PATH ends with '/'."
  (if (scan "/$" path)
      path
      (when path (concatenate 'string path "/"))))

(defun get-date-stamp (days-ago)
  "Return a string corresponding to the date DAYS-AGO.
*day-start-time* (default 4) determines when a day is considered to start."
  (format-timestring
   nil
   (timestamp- (now) (+ *day-start-time* (* days-ago 24)) :hour)
   :format '((:year 4) #\- (:month 2) #\- (:day 2))))

(defun get-and-create-store-dir (file)
  "Get the data storage directory for FILE and ensure it exists."
  (ensure-directories-exist
   (concatenate 'string
                *base-store-dir*
                (replace-/-with-! (namestring (truename file)))
                "/")))

;; TODO: store last date if this takes a significant amount of time
(defun get-previous-date-file (dir)
  "Get the name of the file in DIR corresponding to the last day information was
saved. Assumes that a file for today already exists."
  (let ((file-path (cadr (nreverse (directory-files dir)))))
    (when file-path
      (namestring file-path))))
