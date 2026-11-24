;;; ~A of a list inside a FORMAT logical block counts the block as one level for *print-level*,
;;; as ~S and ~W do.

(defun %fbpl (lev control &rest args)
  (let ((*print-level* lev) (*print-pretty* t))
    (apply #'format nil control args)))

(deftest format-block-print-level-aesthetic.one-block
  (list (%fbpl 1 "~@<~A~:>" '(a (b)))
        (%fbpl 2 "~@<~A~:>" '(a (b)))
        (%fbpl 3 "~@<~A~:>" '(a (b)))
        (%fbpl 1 "~<~A~:>" '((a b))))
  ("#" "(A #)" "(A (B))" "#"))

(deftest format-block-print-level-aesthetic.matches-s-and-w
  (list (string= (%fbpl 2 "~@<~A~:>" '(a (b))) (%fbpl 2 "~@<~S~:>" '(a (b))))
        (string= (%fbpl 2 "~@<~A~:>" '(a (b))) (%fbpl 2 "~@<~W~:>" '(a (b)))))
  (t t))

(deftest format-block-print-level-aesthetic.nested
  (list (%fbpl 2 "~@<#~;~@<#~;~A~:>~:>" '(a b))
        (%fbpl 3 "~@<#~;~@<#~;~A~:>~:>" '(a b)))
  ("###" "##(A B)"))
