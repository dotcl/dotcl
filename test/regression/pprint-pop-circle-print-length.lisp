;;; PPRINT-POP detects a circular list under *PRINT-CIRCLE* also when *PRINT-LENGTH* is set,
;;; as long as the length lets the output reach the back-reference. It used to count the
;;; elements up to the length and ignore the circularity.

(defun %ppcl-ring (n)
  (let ((l (loop for i below n collect (nth i '(a b c d)))))
    (setf (cdr (last l)) l)
    l))

(defun %ppcl-walk (l)
  (with-output-to-string (o)
    (pprint-logical-block (o l :prefix "(" :suffix ")")
      (loop (pprint-exit-if-list-exhausted)
            (write (pprint-pop) :stream o)
            (pprint-exit-if-list-exhausted)
            (write-string " " o)))))

(deftest pprint-pop-circle-print-length.large-length
  (let ((*print-circle* t))
    (list (let ((*print-length* nil)) (%ppcl-walk (%ppcl-ring 2)))
          (let ((*print-length* 100)) (%ppcl-walk (%ppcl-ring 2)))
          (let ((*print-length* 100)) (%ppcl-walk (%ppcl-ring 3)))
          (let ((*print-length* 5)) (%ppcl-walk (%ppcl-ring 3)))))
  ("(A . #1=(B A . #1#))" "(A . #1=(B A . #1#))" "(A . #1=(B C A . #1#))" "(A . #1=(B C A . #1#))"))

(deftest pprint-pop-circle-print-length.short-length-truncates
  (let ((*print-circle* t) (*print-length* 3))
    (%ppcl-walk (%ppcl-ring 3)))
  "(A B C ...)")

(deftest pprint-pop-circle-print-length.not-circular
  (let ((*print-circle* t) (*print-length* 100))
    (%ppcl-walk (list 1 2 3)))
  "(1 2 3)")
