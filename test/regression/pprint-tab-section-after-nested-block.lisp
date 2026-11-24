;;; A :SECTION or :SECTION-RELATIVE tab of a block, after a block nested in it has closed,
;;; measures from where the closed block's text began, as SBCL does (the section that was open
;;; inside the nested block stays the current one).

(defun %ptsb (prefix kind colnum colinc)
  (let ((*print-pretty* t))
    (substitute #\_ #\Space
                (with-output-to-string (o)
                  (write-string prefix o)
                  (pprint-logical-block (o nil :prefix "{" :suffix "}")
                    (pprint-logical-block (o nil :prefix "(")
                      (princ "CCC 345" o))
                    (write-char #\Space o)
                    (pprint-tab kind colnum colinc o)
                    (princ "Z" o))))))

(deftest pprint-tab-section-after-nested-block.section
  (list (%ptsb "" :section 3 2) (%ptsb "" :section 3 4) (%ptsb "" :section 0 3) (%ptsb "xx" :section 3 2))
  ("{(CCC_345__Z}" "{(CCC_345____Z}" "{(CCC_345__Z}" "xx{(CCC_345__Z}"))

(deftest pprint-tab-section-after-nested-block.section-relative
  (list (%ptsb "" :section-relative 3 2) (%ptsb "" :section-relative 3 4) (%ptsb "" :section-relative 0 3)
        (%ptsb "xx" :section-relative 3 2))
  ("{(CCC_345_____Z}" "{(CCC_345_____Z}" "{(CCC_345__Z}" "xx{(CCC_345_____Z}"))

(deftest pprint-tab-section-after-nested-block.line-unchanged
  (list (%ptsb "" :line 3 2) (%ptsb "" :line-relative 3 2))
  ("{(CCC_345__Z}" "{(CCC_345_____Z}"))
