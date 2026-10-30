;;; A logical block's suffix is part of the fit test (CLHS 22.2.1.1).

(deftest pprint-logical-block-suffix-fit.fill
  (let ((*print-right-margin* 12) (*print-pretty* t) (*print-miser-width* nil))
    (with-output-to-string (s)
      (pprint-logical-block (s '(alpha beta) :prefix "(" :suffix ")")
        (loop (pprint-exit-if-list-exhausted)
              (write (pprint-pop) :stream s)
              (write-string " " s)
              (pprint-newline :fill s)))))
  #.(format nil "(ALPHA BETA~% )"))

(deftest pprint-logical-block-suffix-fit.per-line-prefix
  (let ((*print-right-margin* 24) (*print-pretty* t) (*print-miser-width* 40))
    (with-output-to-string (s)
      (pprint-logical-block (s nil :per-line-prefix "x " :suffix " end")
        (write-string "alpha beta" s)
        (pprint-newline :fill s)
        (write-string "gamma delta" s))))
  #.(format nil "x alpha beta~%x gamma delta end"))
