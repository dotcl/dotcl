;;; The dotted tail follows a conditional newline, as PPRINT-POP leaves it.
;;; Expected values are SBCL 2.6.8's output.

(deftest pprint-dotted-tail-layout.fill
  (let ((*print-right-margin* 8) (*print-pretty* t) (*print-miser-width* nil))
    (list (with-output-to-string (s) (pprint-fill s '(a b . tail) t))
          (with-output-to-string (s) (pprint-fill s '(a b . tail) nil))))
  (#.(format nil "(A B~% . TAIL)") #.(format nil "A B~%. TAIL")))

(deftest pprint-dotted-tail-layout.linear
  (let ((*print-right-margin* 8) (*print-pretty* t) (*print-miser-width* nil))
    (list (with-output-to-string (s) (pprint-linear s '(a b . tail) t))
          (with-output-to-string (s) (pprint-linear s '(a b . tail) nil))))
  (#.(format nil "(A~% B~% . TAIL)") #.(format nil "A~%B~%. TAIL")))

(deftest pprint-dotted-tail-layout.tabular
  (let ((*print-right-margin* 8) (*print-pretty* t) (*print-miser-width* nil))
    (with-output-to-string (s) (pprint-tabular s '(a b . tail) t nil 16)))
  #.(format nil "(A~% B~% . TAIL)"))
