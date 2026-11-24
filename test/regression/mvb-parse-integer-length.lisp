;;; MULTIPLE-VALUE-BIND of two variables on PARSE-INTEGER without :JUNK-ALLOWED, :START or :END:
;;; the second value is the length of the string. Expected values from SBCL.

(defun mpl-1 (s) (multiple-value-list (multiple-value-bind (v p) (parse-integer s) (values v p))))
(defun mpl-2 (s r) (multiple-value-list (multiple-value-bind (v p) (parse-integer s :radix r) (values v p))))
(defun mpl-3 (s) (multiple-value-bind (v p) (parse-integer s) (declare (ignore p)) v))
(defun mpl-4 (s) (multiple-value-bind (v p) (parse-integer s) (declare (ignore v)) p))
(defun mpl-5 (s) (multiple-value-bind (v p x) (parse-integer s) (list v p x)))
(defun mpl-6 (s)
  (let ((n 0))
    (multiple-value-bind (v p) (parse-integer (progn (incf n) s) :radix (progn (incf n 10) 10))
      (list v p n))))
(defun mpl-7 (s) (multiple-value-bind (v p) (parse-integer s :junk-allowed t) (list v p)))
(defun mpl-8 (s) (multiple-value-bind (v p) (parse-integer s :start 1) (list v p)))

(deftest mvb-parse-integer-length.plain
  (list (mpl-1 "123") (mpl-1 " 12 ") (mpl-1 "-5") (mpl-1 "+7"))
  ((123 3) (12 4) (-5 2) (7 2)))

(deftest mvb-parse-integer-length.radix
  (list (mpl-2 "ff" 16) (mpl-2 "  ff  " 16))
  ((255 2) (255 6)))

(deftest mvb-parse-integer-length.ignore-and-third
  (list (mpl-3 "42") (mpl-4 "42") (mpl-4 " 42  ") (mpl-5 "9"))
  (42 2 5 (9 1 nil)))

(deftest mvb-parse-integer-length.argument-order
  (mpl-6 "33")
  (33 2 11))

(deftest mvb-parse-integer-length.other-keywords-unchanged
  (list (mpl-7 "12ab") (mpl-8 "a12"))
  ((12 2) (12 3)))

(deftest mvb-parse-integer-length.errors
  (list (handler-case (mpl-1 "12x") (error () :err))
        (handler-case (mpl-1 "") (error () :err))
        (handler-case (mpl-1 5) (error () :err))
        (handler-case (mpl-2 "12" 99) (error () :err)))
  (:err :err :err :err))

(deftest mvb-parse-integer-length.string-kinds
  (list (mpl-1 (make-array 3 :element-type 'character :initial-contents "123" :fill-pointer 2))
        (mpl-1 (coerce "77" 'simple-base-string)))
  ((12 2) (77 2)))
