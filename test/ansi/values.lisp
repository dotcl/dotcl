;;; values.lisp — Tests for multiple-value-list, multiple-value-bind, values

(deftest values.1
  (multiple-value-list (values))
  nil)

(deftest values.2
  (multiple-value-list (values 1))
  (1))

(deftest values.3
  (multiple-value-list (values 1 2 3))
  (1 2 3))

(deftest mvl-non-values
  (multiple-value-list (+ 1 2))
  (3))

(deftest mvl-cons
  (multiple-value-list (cons 'a 'b))
  ((a . b)))

(deftest mvb.1
  (multiple-value-bind (a b c) (values 1 2 3)
    (list a b c))
  (1 2 3))

(deftest mvb.2
  (multiple-value-bind (a) (+ 10 20)
    a)
  30)

(deftest mvb.3
  (multiple-value-bind (a b) (values 1 2)
    (+ a b))
  3)

(do-tests-summary)
