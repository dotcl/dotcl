;;; A value that OR, AND or a test-only COND arm returns from a form that is not
;;; its last is that form's primary value alone.

(defun %oasv-two () (values 5 6))
(defun %oasv-nil-two () (values nil 6))
(defun %oasv-or (x) (or (%oasv-two) x))
(defun %oasv-or-var (x) (%oasv-two) (or x 0))
(defun %oasv-and (x) (and (%oasv-nil-two) x))
(defun %oasv-cond (x) (cond ((%oasv-two)) (t x)))
(defun %oasv-and-cmp (x) (and (< x 0) (%oasv-two)))

(deftest or-and-single-value.non-final
  (list (multiple-value-list (%oasv-or 1))
        (multiple-value-list (%oasv-or-var 3))
        (multiple-value-list (%oasv-and 1))
        (multiple-value-list (%oasv-cond 1))
        (multiple-value-list (%oasv-and-cmp 1))
        (multiple-value-list (or (values 1 2) 3))
        (multiple-value-list (and (values nil 2) 3))
        (multiple-value-list (cond ((values 1 2)) (t 3))))
  ((5) (3) (nil) (5) (nil) (1) (nil) (1)))

(deftest or-and-single-value.final-keeps-values
  (list (multiple-value-list (or nil (%oasv-two)))
        (multiple-value-list (and t (%oasv-two)))
        (multiple-value-list (%oasv-and-cmp -1)))
  ((5 6) (5 6) (5 6)))

(deftest or-and-single-value.bind
  (list (multiple-value-bind (a b) (or (%oasv-two) 0) (list a b))
        (multiple-value-call #'list (or (%oasv-two) 0) (and (%oasv-nil-two) 1)))
  ((5 nil) (5 nil)))

(defun %oasv-pred-or (x) (or (null x) (< x 0) (consp x) (%oasv-two)))
(defun %oasv-pred-and (x) (and (not (null x)) (numberp x) (> x 0) (%oasv-two)))

(deftest or-and-single-value.predicate-clauses
  (list (multiple-value-list (%oasv-pred-or nil))
        (multiple-value-list (%oasv-pred-or -1))
        (multiple-value-list (%oasv-pred-or 1))
        (multiple-value-list (%oasv-pred-and nil))
        (multiple-value-list (%oasv-pred-and -1))
        (multiple-value-list (%oasv-pred-and 1))
        (let ((x 3)) (list (or (eq x 3) :no) (or (not x) :no) (and (not (null x)) :yes))))
  ((t) (t) (5 6) (nil) (nil) (5 6) (t :no :yes)))
