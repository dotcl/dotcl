;;; A fill newline breaks when the section after it does not fit: the section includes the
;;; spaces a section tab makes and the block's suffix. Expected strings are SBCL's.

(defun %pft (tabp margin)
  (with-output-to-string (s)
    (let ((*print-pretty* t) (*print-right-margin* margin))
      (pprint-logical-block (s '("a") :prefix "" :suffix ":END")
        (write (pprint-pop) :stream s)
        (write-char #\Space s)
        (pprint-newline :fill s)
        (if tabp (pprint-tab :section 10 1 s) (write-string "bbbbbb" s))))))

(deftest pprint-fill-before-tab.breaks-when-tab-and-suffix-do-not-fit
  (list (%pft t 10) (%pft t 14))
  ("\"a\"
          :END"
   "\"a\"
          :END"))

(deftest pprint-fill-before-tab.stays-when-it-fits
  (list (%pft t 18) (%pft t 19))
  ("\"a\"           :END" "\"a\"           :END"))

(deftest pprint-fill-before-tab.without-tab
  (list (%pft nil 10) (%pft nil 14) (%pft nil 15))
  ("\"a\"
bbbbbb:END" "\"a\" bbbbbb:END" "\"a\" bbbbbb:END"))
