;;; NIL is the empty list for the list-printing functions (CLHS 22.2.1.3).

(deftest pprint-empty-list-printers.fill-colon
  (with-output-to-string (s) (pprint-fill s nil t))
  "()")

(deftest pprint-empty-list-printers.fill-no-colon
  (with-output-to-string (s) (pprint-fill s nil nil))
  "")

(deftest pprint-empty-list-printers.linear-colon
  (with-output-to-string (s) (pprint-linear s nil t))
  "()")

(deftest pprint-empty-list-printers.tabular-colon
  (with-output-to-string (s) (pprint-tabular s nil t))
  "()")
