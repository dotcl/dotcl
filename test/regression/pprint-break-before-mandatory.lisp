;;; A conditional newline that breaks and is at once followed by a mandatory newline still
;;; starts a line that holds the block's prefix and indentation; the second line end then
;;; takes the blanks off. So a per-line prefix shows on the line between the two line ends,
;;; and an empty indentation leaves an empty line. The expected values are SBCL's.

(defun %pbm-outer (m miser thunk)
  (let ((*print-pretty* t) (*print-right-margin* m) (*print-miser-width* miser) (*print-lines* nil))
    (with-output-to-string (s)
      (pprint-logical-block (s nil :prefix "{" :suffix "}") (funcall thunk s)))))

(defun %pbm-plp-block (s kind1 suffix)
  (pprint-logical-block (s nil :per-line-prefix "> " :suffix suffix)
    (pprint-newline kind1 s)
    (pprint-newline :mandatory s)))

(deftest pprint-break-before-mandatory.layouts
  (list
   (%pbm-outer 10 nil (lambda (s)
                        (pprint-newline :fill s)
                        (pprint-logical-block (s nil) (pprint-newline :mandatory s))))
   (%pbm-outer 11 10 (lambda (s) (%pbm-plp-block s :fill "")))
   (%pbm-outer 12 10 (lambda (s) (%pbm-plp-block s :fill ")")))
   (%pbm-outer 17 nil (lambda (s) (%pbm-plp-block s :linear "")))
   (%pbm-outer 10 nil (lambda (s)
                        (write-string "x" s)
                        (pprint-newline :fill s)
                        (pprint-logical-block (s nil) (pprint-newline :mandatory s))))
   (%pbm-outer 11 3 (lambda (s)
                      (pprint-newline :linear s)
                      (pprint-indent :current 2 s)
                      (pprint-newline :mandatory s))))
  ("{

 }"
   "{>
 >
 > }"
   "{>
 >
 > )}"
   "{>
 >
 > }"
   "{x

 }"
   "{

   }"))
