;;; *PRINT-LEVEL* counts the logical block itself as one level (CLHS
;;; PPRINT-LOGICAL-BLOCK). Expected value is SBCL 2.6.8's output.

(deftest pprint-print-level-logical-block.relative
  (let ((*print-level* 2) (*print-pretty* t))
    (with-output-to-string (s)
      (pprint-logical-block (s '(a (b (c d)) e) :prefix "(" :suffix ")")
        (loop (pprint-exit-if-list-exhausted)
              (write (pprint-pop) :stream s)
              (write-string " " s)))))
  "(A (B #) E )")
