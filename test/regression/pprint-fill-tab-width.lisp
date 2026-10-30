;;; A block whose expanded tabs fit on one line must not break at fill newlines.
;;; Expected values are SBCL 2.6.8's output.

(defun pftw-run (colnum colinc margin)
  (let ((*print-right-margin* margin) (*print-pretty* t) (*print-miser-width* nil))
    (with-output-to-string (s)
      (pprint-logical-block (s '(a bb ccc dddd eeeee) :prefix "(" :suffix ")")
        (loop (pprint-exit-if-list-exhausted)
              (write (pprint-pop) :stream s :pretty t)
              (pprint-tab :section colnum colinc s)
              (pprint-newline :fill s))))))

(deftest pprint-fill-tab-width.flat
  (pftw-run 8 0 24)
  "(A       BBCCCDDDDEEEEE)")

(deftest pprint-fill-tab-width.flat-colinc
  (pftw-run 8 2 30)
  "(A       BB  CCC DDDD  EEEEE )")

(deftest pprint-fill-tab-width.break
  (pftw-run 1 2 20)
  "(A  BB  CCC DDDD
 EEEEE  )")

(deftest pprint-fill-tab-width.break-colnum
  (pftw-run 3 2 20)
  "(A  BB  CCC DDDD
 EEEEE  )")

(deftest pprint-fill-tab-width.break-narrow
  (pftw-run 5 0 16)
  "(A    BBCCCDDDD
 EEEEE)")

(deftest pprint-fill-tab-width.break-colinc
  (pftw-run 8 2 24)
  "(A       BB  CCC DDDD
 EEEEE   )")

(deftest pprint-fill-tab-width.tabular-nested-origin
  (let ((*print-right-margin* 100) (*print-pretty* t) (*print-miser-width* nil))
    (format nil "~<~4:/pprint-tabular/~:>" '((m m))))
  "(M   M)")

(deftest pprint-fill-tab-width.tabular-nested-default
  (let ((*print-right-margin* 100) (*print-pretty* t) (*print-miser-width* nil))
    (format nil "~<~v:/pprint-tabular/~:>" '(nil (m m))))
  "(M               M)")

(deftest pprint-fill-tab-width.tabular-nested-narrow
  (let ((*print-right-margin* 100) (*print-pretty* t) (*print-miser-width* nil))
    (format nil "~<~v:/pprint-tabular/~:>" '(3 (m m))))
  "(M  M)")
