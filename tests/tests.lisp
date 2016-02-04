;; in the same package (but not included in main system)
;; allows for access to internals without exporting everything
(in-package :chronicler)
;; I don't need prove in the main package; only import its symbols here
(use-package :prove)

;; don't plan a specific number of tests to run
(plan nil)

;; check that ignored syntax is not counted
(is (count-org-words (read-file-into-string "tests/ignore-syntax.org"))
    14)

;; check that headings and subheadings are counted properly
(let* ((headings (parse-markup-file "tests/depth.org"))
       (root-heading (gethash 'root headings))
       (level1 (gethash
                (intern (string-upcase "ea242d4f-c79b-4f06-95d2-5e197071a048"))
                headings)))
  (subtest "Testing individual heading accuracy."
    (is (total-word-count root-heading)
        10)
    (is (word-count root-heading)
        0)
    (is (total-word-count level1)
        8)
    (is (word-count level1)
        2)))

(finalize)
