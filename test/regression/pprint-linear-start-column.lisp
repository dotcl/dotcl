;;; PPRINT-LINEAR measures its fit from the column of the stream it writes.
;;; Expected value is SBCL 2.6.8's output.

(deftest pprint-linear-start-column.after-previous-call
  (let ((*print-right-margin* 30) (*print-pretty* t) (*print-miser-width* nil))
    (list (with-output-to-string (s)
            (pprint-linear s '(alpha beta gamma delta) t))
          (with-output-to-string (s)
            (pprint-linear s '(a (b c) d) t))))
  #.(list "(ALPHA BETA GAMMA DELTA)" "(A (B C) D)"))
