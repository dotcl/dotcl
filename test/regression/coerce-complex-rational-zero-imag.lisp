;;; COERCE to a COMPLEX type with a rational part type: a zero imaginary part
;;; makes the result the real number itself, as COMPLEX does.

(deftest coerce-complex-rational.integer
  (list (coerce 1 '(complex real)) (coerce 1 '(complex integer))
        (coerce 1/2 '(complex rational)) (coerce 1 '(complex (integer 0 5))))
  (1 1 1/2 1))

(deftest coerce-complex-rational.result-is-real
  (list (typep (coerce 1 '(complex rational)) 'integer)
        (complexp (coerce 1 '(complex rational))))
  (t nil))

(deftest coerce-complex-rational.float-part-stays-complex
  (list (coerce 1 '(complex float)) (coerce 1.5 '(complex single-float)))
  (#c(1.0 0.0) #c(1.5 0.0)))
