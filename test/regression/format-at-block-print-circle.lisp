;;; ~@<...~:> takes the remaining FORMAT arguments as its list, so *PRINT-CIRCLE*
;;; finds structure those arguments share, as it does for the list of ~<...~:>.
;;; Expected values are what SBCL returns.

(deftest format-at-block-print-circle.shared-args
  (let ((*print-circle* t) (*print-pretty* t) (x (list 1)) (y (list 2)))
    (list (format nil "~@<~S ~S~:>" x x)
          (format nil "~@<~A ~A~:>" x x)
          (format nil "~@<~S ~S ~S~:>" x y x)
          (format nil "~@<(~;~S ~S~;)~:>" x x)))
  ("#1=(1) #1#" "#1=(1) #1#" "#1=(1) (2) #1#" "(#1=(1) #1#)"))

(deftest format-at-block-print-circle.nothing-shared
  (let ((*print-circle* t) (*print-pretty* t) (x (list 1)) (y (list 2)))
    (list (format nil "~@<~S ~S~:>" x y)
          (let ((*print-circle* nil)) (format nil "~@<~S ~S~:>" x x))
          (let ((*print-pretty* nil)) (format nil "~@<~S ~S~:>" x y))))
  ("(1) (2)" "(1) (1)" "(1) (2)"))

(deftest format-at-block-print-circle.list-form-unchanged
  (let ((*print-circle* t) (*print-pretty* t) (x (list 1)))
    (format nil "~<~S ~S~:>" (list x x)))
  "#1=(1) #1#")
