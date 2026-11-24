;;; ~D, ~B, ~O and ~X of a non-integer print it as ~A does, so a list under *print-pretty*
;;; breaks at *print-right-margin* from the column the directive starts at.

(defun %frn (control arg)
  (let ((*print-pretty* t) (*print-right-margin* 20))
    (substitute #\Space #\Newline (format nil control arg))))

(deftest format-radix-non-integer-pretty-column.breaks-like-a
  (let ((a (%frn "0123456789012345~A" '(1 2 3))))
    (list (string= a (%frn "0123456789012345~D" '(1 2 3)))
          (string= a (%frn "0123456789012345~X" '(1 2 3)))
          (string= (%frn "0123456789012345~A" '(1 2 3)) (%frn "0123456789012345~O" '(1 2 3)))
          (and (search "(1 " a) (not (search "(1 2 3)" a)) t)))
  (t t t t))

(deftest format-radix-non-integer-pretty-column.fits
  (let ((*print-pretty* t) (*print-right-margin* 80))
    (list (format nil "x~D" '(1 2 3)) (format nil "x~B" '(1 2 3))))
  ("x(1 2 3)" "x(1 10 11)"))
