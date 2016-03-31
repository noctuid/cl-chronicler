;; TODO: make better sexp regexp wrapper for cl-ppcre or try cl-irregsexp
(in-package #:chronicler)

;;; Org Heading Class and Methods
(defclass org-heading (heading)
  ())

;;; Org Regex
(defparameter +org-heading-regexp+
  "(?m)^\\*+ .*$"
  "Regexp that matches an org heading.")

(defparameter +org-heading-with-contents-regexp+
  "(?ms)^\\*+ .+?(?=^\\*+ |\\Z)"
  "Regexp that matches an org heading's contents not including sub-headings.")

(defparameter +org-comment-regexp+
  "(?m)^[ \\t]*# .*$"
  "Regexp that matches an org comment.")

(defparameter +org-block-regexp+
  "(?ms)^[ \\t]*#\\+begin.*?$.*?#\\+end.*?$"
  "Regexp that matches an org block.")

(defparameter +org-drawer-regexp+
  "(?m)^[ \\t]*:(\\w|-)+:.*$"
  "Regexp that matches an org drawer.")

(defparameter +org-planning-regexp+
  "(?m)^[ \\t]*(CLOSED|DEADLINE|SCHEDULED):.*$"
  "Regexp that matches an org planning line.")

;; (defun regex-or (expression-list)
;;   (concatenate 'string
;;                "("
;;                (reduce
;;                 #'(lambda (x y) (concatenate 'string y "|" x))
;;                 expression-list)
;;                ")"))

;; (defparameter *org-remove-regexp*
;;   (regex-or *org-syntax-regexps*)
;;   "Combination of org syntax to ignore.")

(defparameter *org-syntax-regexps*
  (list +org-heading-regexp+ 
        +org-comment-regexp+
        +org-block-regexp+
        +org-drawer-regexp+
        +org-planning-regexp+))

;; TODO: don't loop through the text a bunch of times
(defun remove-org-ignored-text (text)
  (dolist (regex *org-syntax-regexps* text)
    (setf text (regex-replace-all regex text ""))))

(defun count-org-words (text)
  "Return the number of words in TEXT ignoring special org syntax."
  (count-words (remove-org-ignored-text text)))

(defparameter +org-property-drawer-regexp+
  "(?ms)^:PROPERTIES:$.*?:END:$"
  "Regexp that matches an org property drawer.")

(defparameter +count-property+
  "(?mi)^[ \\t]*:count:$"
  "Regexp that matches the count property.")

(defparameter +count-recursive-property+
  "(?mi)^[ \\t]*:count-recursive:$"
  "Regexp that matches the recursive count property.")

(defparameter +no-count-property+
  "(?mi)^[ \\t]*:no-count:$"
  "Regexp that matches the ignore property.")

(defparameter +no-count-recursive-property+
  "(?mi)^[ \\t]*:no-count-recursive:$"
  "Regexp that matches the recursive ignore property.")

(defparameter +id-property-regexp+
  "(?mi)^[ \\t]*:id:.*$"
  "Regexp that matches the id property.")

(defun heading-ignored-p (heading-text parent-ignored-recursively-p)
  "Return a list containing whether HEADING-TEXT is ignored and whether
any sub-headings will be ignored by default. PARENT-IGNORED-RECURSIVELY-P
determines the default recursive setting for the heading. Properties that
specify that the heading should be counted take precedence in the case that
the heading has both ignore and count properties."
  (let* ((property-drawer
           (scan-to-strings +org-property-drawer-regexp+ heading-text))
         (ignored-recursively
           (cond ((scan-to-strings +count-recursive-property+ property-drawer)
                  nil)
                 ((scan-to-strings +no-count-recursive-property+ property-drawer)
                  t)
                 (t
                  parent-ignored-recursively-p)))
         (ignored
           (cond ((scan-to-strings +count-property+ property-drawer)
                  nil)
                 ((scan-to-strings +no-count-property+ property-drawer)
                  t)
                 (t
                  ignored-recursively))))
    (list ignored ignored-recursively)))

(defun get-heading-id (heading-text)
  "Return the id of HEADING-TEXT or nil if there is not one."
  (-<>> heading-text
        (scan-to-strings +org-property-drawer-regexp+)
        (scan-to-strings +id-property-regexp+)
        (regex-replace "(?i)^[ \\t]*:id:\\s*" <> "")
        (string-upcase)
        (intern)))

(defun get-heading-depth (heading)
  "Return the level of the org heading."
  (length (scan-to-strings "\\A\\*+" heading)))

;; faster way than do-matches?
(defun parse-org-file (file)
  "Return a hash table representing the org headings in FILE.
The keys correspond to the ids of the org headings (if they have ids).
The heading object at the 'root' key represents the entire org file and will
contain all the top level headings in its 'sub-headings' instance data."
  (let ((org-text (read-file-into-string file))
        (root-heading (make-instance 'org-heading
                                     :ignored *default-ignore-behavior*))
        (headings (make-hash-table))
        (heading-tracker (make-array 30 :adjustable t)))
    (setf (gethash 'root headings) root-heading)
    (setf (aref heading-tracker 0) root-heading)
    (do-matches-as-strings
        (heading-text +org-heading-with-contents-regexp+ org-text)
      (bind ((current-depth (get-heading-depth heading-text))
             (parent-heading (aref heading-tracker (1- current-depth)))
             (heading-id (get-heading-id heading-text))
             ((ignored ignored-recursively)
              (heading-ignored-p
               heading-text
               (ignored-recursively parent-heading)))
             (heading (make-instance
                       'org-heading
                       :id heading-id
                       :ignored ignored
                       :ignored-recursively ignored-recursively
                       :word-count (if ignored
                                       0
                                       (count-org-words heading-text)))))
        (setf (sub-headings parent-heading)
              (cons heading (sub-headings parent-heading)))
        (when (>= current-depth (length heading-tracker))
          (setf heading-tracker (adjust-array heading-tracker
                                              (+ current-depth 50))))
        (setf (aref heading-tracker current-depth) heading)
        (setf (gethash heading-id headings) heading)))
    headings))
