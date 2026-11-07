;;; ~@{ ... ~} inside ~< ... ~:> stops at *PRINT-LENGTH* with "...", as PPRINT-POP does.

(defun %fbl-run (length control &rest args)
  (let ((*print-pretty* t) (*print-length* length) (*print-right-margin* 80))
    (with-output-to-string (s) (apply #'format s control args))))

(deftest format-block-length.iteration
  (list (%fbl-run 1 "~<(~;~@{~S~^ ~:_~}~;)~:>" '(a b c))
        (%fbl-run 2 "~<(~;~@{~S~^ ~:_~}~;)~:>" '(a b c))
        (%fbl-run 3 "~<(~;~@{~S~^ ~:_~}~;)~:>" '(a b c))
        (%fbl-run nil "~<(~;~@{~S~^ ~:_~}~;)~:>" '(a b c)))
  ("(A ...)" "(A B ...)" "(A B C)" "(A B C)"))

(deftest format-block-length.counts-leading-directives
  (%fbl-run 2 "~<(~;~S ~:I~@{~S~^ ~_~}~;)~:>" '(a b c d))
  "(A B ...)")

(deftest format-block-length.outside-a-block-unchanged
  (%fbl-run 1 "~@{~S~^ ~}" 'a 'b 'c)
  "A B C")
