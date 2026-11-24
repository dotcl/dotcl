;;; The rows of a multi-dimensional array break together when they do not fit on a line (as SBCL
;;; prints them); only the elements of the innermost dimension fill.

(defun %pmar (array)
  (let ((*print-right-margin* 40) (*print-pretty* t))
    (prin1-to-string array)))

(deftest pprint-multidim-array-rows-linear.rows-break-together
  (list (%pmar (make-array '(4 3) :initial-element 100))
        (%pmar (make-array '(2 2 3) :initial-element 777))
        (%pmar (make-array '(5 2) :initial-element 'a)))
  ("#2A((100 100 100)
    (100 100 100)
    (100 100 100)
    (100 100 100))"
   "#3A(((777 777 777) (777 777 777))
    ((777 777 777) (777 777 777)))"
   "#2A((A A) (A A) (A A) (A A) (A A))"))

(deftest pprint-multidim-array-rows-linear.innermost-fills
  (%pmar (make-array '(2 9) :initial-element 123456))
  "#2A((123456 123456 123456 123456 123456
     123456 123456 123456 123456)
    (123456 123456 123456 123456 123456
     123456 123456 123456 123456))")
