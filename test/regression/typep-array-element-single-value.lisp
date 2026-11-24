;;; TYPEP returns one value. For an array type whose element type had to be
;;; upgraded through SUBTYPEP ((vector (or bit character)), (vector (member 0 1)))
;;; the second value of that SUBTYPEP leaked out as a second value of TYPEP.

(deftest typep-array-element-single-value.or-element
  (list (multiple-value-list (typep "abc" '(vector (or bit character))))
        (multiple-value-list (typep #*101 '(vector (or bit character))))
        (multiple-value-list (typep (make-array 3) '(array (or null integer)))))
  ((nil) (nil) (t)))

(deftest typep-array-element-single-value.member-element
  (list (multiple-value-list (typep #*101 '(vector (member 0 1))))
        (multiple-value-list (typep "abc" '(vector (member 0 1)))))
  ((t) (nil)))

(deftest typep-array-element-single-value.in-compiled-code
  (let ((f (lambda (x) (typep x '(array (and integer (satisfies evenp)))))))
    (multiple-value-list (funcall f "abc")))
  (nil))
