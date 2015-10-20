(in-package :chronicler)

;; TODO: figure out why emacs is indenting these wrong
(define-opts
    (:name :help
     :description "Print this help text."
     :short #\h
     :long "help")
    ;; (:name :total
    ;;  :description "Print total counts instead of comparative"
    ;;  :short #\T
    ;; :long "total")
    (:name :update
     :description "Update all count information for the specified file."
     :short #\u
     :long "update")
  (:name :report
   :description "Report count information."
   :short #\r
   :long "report")
  (:name :directory
   :description "The directory to use for storing count information.
                 Defaults to '.chronicler/' in the current directory."
   :short #\d
   :long "directory"
   :arg-parser #'identity
   :meta-var "DIR")
  (:name :day-start-time
   :description
   "The time that counts will be reset (0-23). Defaults to 4am."
   :short #\t
   :long "day-start-time"
   :arg-parser #'parse-integer
   :meta-var "TIME"))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (option condition))
  (invoke-restart 'skip-option))

(defun print-help ()
  "Print command line help text."
  (describe
   :prefix
   "chronicler —  Track and analyze writing statistics for emacs org files."
   :usage-of "chronicler"
   :args     "[file name]"))

(defun main (argv)
  "Main function for handling command line options."
  (multiple-value-bind (options free-args)
      (handler-case
          (handler-bind ((unknown-option #'unknown-option))
            (get-opts argv))
        (missing-arg (condition)
          (format t "fatal: option ~s needs an argument!~%"
                  (option condition)))
        (arg-parser-failed (condition)
          (format t "fatal: cannot parse ~s as argument of ~s~%"
                  (raw-arg condition)
                  (option condition))))
    (cond ((getf options :help)
           (print-help))
          ((not (= (length free-args) 1))
           (print-help)
           (format
            t
            "Exactly one positional argument <file name> is required.~%")
           (format t "The following arguments were given:~%")
           (dolist (arg free-args)
             (format t "~a~%" arg)))
          ((not (probe-file (car free-args)))
           (print-help)
           (format t "The specified file does not exist.~%"))
          ((cl-ppcre:scan "!" (namestring (truename (car free-args))))
           (format t "The file path may not contain the character '!'.~%"))
          (t
           (let* ((*day-start-time* (or (getf options :day-start-time)
                                        *day-start-time*))
                  (*base-store-dir* (or (ensure-trailing-/
                                         (getf options :directory))
                                        *base-store-dir*))
                  (file (car free-args)))
             (cond ((getf options :report)
                    (format t "word count:~a~%"
                            (multiple-value-bind (today-info previous-info)
                                (get-today-info file (getf options :update))
                              (get-today-word-count today-info previous-info))))
                   ((getf options :update)
                    (update-today-info file))))))))
