;;; PPRINT-TABULAR tabs from the current section and fills after the tab.
;;; Expected values are SBCL 2.6.8's output.

(deftest pprint-tabular-section-layout.wide
  (let ((*print-right-margin* 80) (*print-pretty* t) (*print-miser-width* nil))
    (with-output-to-string (s)
      (pprint-tabular s '(alpha beta gamma delta epsilon zeta eta theta))))
  #.(format nil "(ALPHA           BETA            GAMMA           DELTA           EPSILON~% ZETA            ETA             THETA)"))

(deftest pprint-tabular-section-layout.narrow
  (let ((*print-right-margin* 24) (*print-pretty* t) (*print-miser-width* nil))
    (with-output-to-string (s)
      (pprint-tabular s '(1 2 3 4 5 6) nil nil 20)))
  #.(format nil "1                   2~%3                   4~%5                   6"))
