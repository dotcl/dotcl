;;; (setf (values) form) has no place to store into: the form is evaluated and no
;;; value is returned (CLHS 5.1.2.3: as many values as there are places).

(defvar *svnp-count* 0)

(deftest setf-values-no-places.returns-nothing
  (list (multiple-value-list (setf (values) (values 1 2 3)))
        (multiple-value-list (setf (values) 1))
        (multiple-value-list (let ((x 0)) (setf (values) (incf x)) (values))))
  (nil nil nil))

(deftest setf-values-no-places.form-still-evaluated
  (progn (setf (values) (incf *svnp-count*)) (setf (values) (incf *svnp-count*)) *svnp-count*)
  2)

(deftest setf-values-no-places.places-unchanged
  (let (a b)
    (list (multiple-value-list (setf (values a b) (values 1 2 3))) a b))
  ((1 2) 1 2))
