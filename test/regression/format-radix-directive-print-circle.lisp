;;; ~D ~B ~O ~X of an argument that is not an integer print it as ~A does, so *print-circle*
;;; applies to it. Expected strings are SBCL's.

(defun %frc-circular ()
  (let ((x (list 1 2)))
    (setf (cdr (cdr x)) x)
    x))

(deftest format-radix-print-circle.circular-argument
  (let ((*print-circle* t) (x (%frc-circular)))
    (list (format nil "~D" x) (format nil "~A" x) (format nil "~X" x)))
  ("#1=(1 2 . #1#)" "#1=(1 2 . #1#)" "#1=(1 2 . #1#)"))

(deftest format-radix-print-circle.shared-argument
  (let* ((*print-circle* t) (s (list 'a)) (v (list s s)))
    (list (format nil "~D" v) (format nil "~O" v)
          (with-output-to-string (o) (format o "~D" v))))
  ("(#1=(A) #1#)" "(#1=(A) #1#)" "(#1=(A) #1#)"))

(deftest format-radix-print-circle.unchanged-neighbours
  (let ((*print-circle* t))
    (list (format nil "~D" '(1 2))
          (format nil "~D" 255)
          (format nil "~X" 255)
          (format nil "~X" '(10 11))
          (format nil "~D" 'sym)
          (let ((*print-circle* nil) (s (list 'a))) (format nil "~D" (list s s)))))
  ("(1 2)" "255" "FF" "(A B)" "SYM" "((A) (A))"))
