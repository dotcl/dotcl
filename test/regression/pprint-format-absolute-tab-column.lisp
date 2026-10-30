;;; FORMAT ~T takes the column after a preceding object is laid out.
;;; Expected values are SBCL 2.6.8's output.

(deftest pprint-format-absolute-tab.multi-line
  (let ((*print-right-margin* 5) (*print-pretty* t) (*print-miser-width* 0))
    (format nil "~@<~a~10T~a~:>"
            '(x (y z) (w (v (u))) t)
            '("string one" "string two" "string three")))
  "(X
 (Y
  Z)
 (W
  (V
   (U)))
 T)       (string one
           string two
           string three)")

(deftest pprint-format-absolute-tab.colinc
  (let ((*print-right-margin* 20) (*print-pretty* t) (*print-miser-width* 10))
    (format nil "~@<~a~5,3T~a~:>"
            '(x (y z) (w (v (u))) t) '(a b c) '(a)))
  "(X (Y Z)
 (W (V (U))) T)  (A
                  B
                  C)")

(deftest pprint-format-absolute-tab.after-indent
  (let ((*print-right-margin* 12) (*print-pretty* t) (*print-miser-width* 80))
    (format nil "~@<~a~10I~a~10T~a~:>"
            '(#\a #\b #\c) '((a b) (c d) (e f))
            '(1 2 3 4 5 6 7 8 9 10 11 12)))
  "(a
 b
 c)((A B)
    (C D)
    (E
     F))  (1
           2
           3
           4
           5
           6
           7
           8
           9
           10
           11
           12)")

(deftest pprint-format-absolute-tab.narrow
  (let ((*print-right-margin* 8) (*print-pretty* t) (*print-miser-width* nil))
    (format nil "~@<~a~5,3T~a~:>"
            '("string one" "string two" "string three")
            '("string one" "string two" "string three")
            '(this-is-a-long-symbol another-long-symbol and-a-third-long-symbol)))
  "(string one
 string two
 string three)   (string one
                  string two
                  string three)")

(deftest pprint-format-absolute-tab.single-line
  (let ((*print-right-margin* 40) (*print-pretty* t))
    (format nil "~@<~a~10T~a~:>" 'a 'b))
  "A         B")

(deftest pprint-format-absolute-tab.relative-single-line
  (let ((*print-right-margin* 40) (*print-pretty* t))
    (format nil "~@<~a~5,3@T~a~:>" 'a 'b))
  "A     B")
