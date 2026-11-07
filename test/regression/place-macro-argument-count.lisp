;;; incf / decf / push / pop signal a PROGRAM-ERROR for a wrong number of arguments instead
;;; of ignoring the extra ones ((incf a 2 3) was 3) or expanding (incf) to (setq nil ...).

(defmacro %pmac-program-error-p (form)
  `(handler-case (progn (eval ',form) nil) (program-error () t)))

(deftest place-macro-argument-count.extra-arguments
  (list (%pmac-program-error-p (let ((a 1)) (incf a 2 3)))
        (%pmac-program-error-p (let ((a 1)) (decf a 2 3)))
        (%pmac-program-error-p (let ((a nil)) (push 1 a 2)))
        (%pmac-program-error-p (let ((a (list 1))) (pop a 1))))
  (t t t t))

(deftest place-macro-argument-count.too-few
  (list (%pmac-program-error-p (incf))
        (%pmac-program-error-p (decf))
        (%pmac-program-error-p (push 1))
        (%pmac-program-error-p (pop)))
  (t t t t))

(deftest place-macro-argument-count.valid-forms
  (let ((a 1) (l (list 1 2)) (v (vector 1 2)))
    (list (incf a) (incf a 5) (decf a) (decf a 2)
          (push 0 l) (pop l) (incf (aref v 0) 10) (pop l)))
  (2 7 6 4 (0 1 2) 0 11 1))
