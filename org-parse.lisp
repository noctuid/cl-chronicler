;; TODO: make better sexp regexp wrapper for cl-ppcre or try cl-irregsexp
(in-package :chronicler)

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

(defun heading-ignored-p (heading-text parent-ignored-recursively)
  "Return a list containing whether HEADING-TEXT is ignored and whether
any sub-headings will be ignored by default. PARENT-IGNORED-RECURSIVELY
determines the default recursive setting for the heading."
  (let ((property-drawer
          (scan-to-strings +org-property-drawer-regexp+ heading-text))
        count
        count-recursive
        no-count
        no-count-recursive
        ignored
        ignored-recursively)
    (when (scan-to-strings +count-property+ property-drawer)
      (setf count t))
    (when (scan-to-strings +count-recursive-property+ property-drawer)
      (setf count-recursive t))
    (when (scan-to-strings +no-count-property+ property-drawer)
      (setf no-count t))
    (when (scan-to-strings +no-count-recursive-property+ property-drawer)
      (setf no-count-recursive t))
    ;; do count properties take precedence
    (cond (count-recursive
           (setf ignored-recursively nil))
          (no-count-recursive
           (setf ignored-recursively t))
          (t
           (setf ignored-recursively parent-ignored-recursively)))
    (cond (count
           (setf ignored nil))
          (no-count
           (setf ignored t))
          (t
           (setf ignored ignored-recursively)))
    (list ignored ignored-recursively)))

(defun get-heading-id (heading-text)
  "Return the id of HEADING-TEXT or nil if there is not one."
  (let* ((property-drawer
           (scan-to-strings +org-property-drawer-regexp+ heading-text))
         (id-section (scan-to-strings +id-property-regexp+ property-drawer)))
    (intern
     (string-upcase (regex-replace "(?i)^[ \\t]*:id:\\s*" id-section "")))))

(defun get-heading-depth (heading)
  "Return the level of the org heading."
  (length (scan-to-strings "\\A\\*+" heading)))

;; faster way than do-matches?
(defun parse-org-file (file)
  "Return a hash table representing the org headings in FILE.
The keys correspond to the ids of the org headings (if they have ids).
The heading object at the 'root' key represents the entire org file and will
contain all the top level headings in its 'sub-headings' instance data."
  (let ((org-text (get-file-as-string file))
        (root-heading (make-instance 'org-heading
                                     :ignored *default-ignore-behavior*))
        (heading-hash-table (make-hash-table))
        (heading-tracker (make-array 30 :adjustable t)))
    (setf (gethash 'root heading-hash-table) root-heading)
    (setf (elt heading-tracker 0) root-heading)
    (do-matches-as-strings
        (heading-text +org-heading-with-contents-regexp+ org-text)
      (let* ((current-depth (get-heading-depth heading-text))
             (parent-heading (elt heading-tracker (1- current-depth)))
             (new-settings (heading-ignored-p
                            heading-text
                            (ignored-recursively parent-heading)))
             (ignored (car new-settings))
             (ignored-recursively (cadr new-settings))
             (heading-id (get-heading-id heading-text))
             (heading-object (make-instance
                              'org-heading
                              :ignored ignored
                              :ignored-recursively ignored-recursively
                              :id heading-id
                              :word-count (if ignored
                                              0
                                              (count-org-words heading-text)))))
        (setf (sub-headings parent-heading)
              (cons heading-object (sub-headings parent-heading)))
        (when (>= current-depth (length heading-tracker))
          (setf heading-tracker (adjust-array heading-tracker
                                              (+ current-depth 50))))
        (setf (elt heading-tracker current-depth) heading-object)
        (setf (gethash heading-id heading-hash-table) heading-object)))
    heading-hash-table))
