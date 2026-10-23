;;; CONSP / ATOM / LISTP of a variable take the variable's value as it is; the
;;; other values of a VALUES form it was bound from are already gone.

(defun %pv-test (x) (list (consp x) (atom x) (listp x)
                          (typep x 'cons) (typep x 'atom) (typep x 'list)))

(deftest predicate-variable.from-values
  (list (let ((x (values (list 1) 2))) (%pv-test x))
        (let ((x (values 1 '(2)))) (%pv-test x))
        (let ((x (values nil '(2)))) (%pv-test x))
        (%pv-test (values '(a) 2))
        (let ((x (multiple-value-prog1 (values '(1) 2) nil))) (%pv-test x))
        (let ((x 0)) (setq x (values nil '(1))) (%pv-test x)))
  ((t nil t t nil t) (nil t nil nil t nil) (nil t t nil t t)
   (t nil t t nil t) (t nil t t nil t) (nil t t nil t t)))

(deftest predicate-variable.walk
  (labels ((walk (x) (if (consp x) (+ (walk (car x)) (walk (cdr x))) (if (atom x) (if x 1 0) 99))))
    (list (walk '((a b) (c (d e)) f)) (walk nil) (walk 'a)))
  (6 0 1))
