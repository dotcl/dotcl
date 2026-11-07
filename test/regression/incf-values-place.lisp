;;; INCF / DECF on a (VALUES a b) place store the new value in the first place
;;; and NIL in the others, and return the new value. The extra store variable of
;;; the setf expansion used to be left unbound.

(deftest incf-values-place.basic
  (let ((a 1) (b 2)) (incf (values a b)) (list a b))
  (2 nil))

(deftest incf-values-place.result-and-delta
  (let ((a 1) (b 2)) (list (incf (values a b) 5) a b))
  (6 6 nil))

(deftest incf-values-place.decf
  (let ((a 10) (b 2)) (list (decf (values a b) 3) a b))
  (7 7 nil))

(deftest incf-values-place.three-places
  (let ((a 1) (b 2) (c 3)) (incf (values a b c)) (list a b c))
  (2 nil nil))

(deftest incf-values-place.single-place-unchanged
  (let ((a 1)) (list (incf (values a)) a))
  (2 2))

(deftest incf-values-place.ordinary-places-unchanged
  (let ((pl (list :a 1)) (v (vector 1 2)))
    (list (incf (getf pl :a) 10) (decf (aref v 0)) (incf (car (list 1)))))
  (11 0 2))
