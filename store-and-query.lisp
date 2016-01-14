;;; Persisting and Getting Information for Entire File
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

(defun get-days-ago-info (file days-ago)
  "Return the stored information for FILE as of DAYS-AGO days.
As a special case, specifying -1 for DAYS-AGO will get the information for the
last date before today that information was stored."
  (let* ((store-dir (get-and-create-store-dir file))
         (day-file (if (= days-ago -1)
                       (get-previous-date-file store-dir)
                       (concatenate 'string store-dir
                                    (get-date-stamp days-ago)))))
    (if (probe-file day-file)
        (restore file)
        nil)))

(defun get-previous-info (file)
  "Return the information for FILE from the most recent day before today."
  (get-days-ago-info file -1))

(defun get-today-info (file)
  "Return the information for FILE from today."
  (get-days-ago-info file 0))

;;; Comparison Functions
(defun compare-days (accessor day1 day2)
  "Return the difference between DAY1 and DAY2 values as obtained with
ACCESSOR."
  (-
   (funcall accessor day1)
   (funcall accessor day2)))

(defun compare-word-count (today-info earlier-info)
  "Return the difference in total word count between TODAY-INFO and
EARLIER-INFO."
  (compare-days
   #'total-word-count
   (gethash 'root today-info)
   (gethash 'root earlier-info)))
