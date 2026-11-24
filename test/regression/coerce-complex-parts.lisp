;;; COERCE to a complex type builds parts of one float format.

(deftest coerce-complex-parts.real-part-type
  (list (coerce 1.5 '(complex real))
        (imagpart (coerce 1.5 '(complex real)))
        (typep (coerce 1.5 '(complex real)) '(complex single-float))
        (coerce -2.5d0 '(complex real))
        (typep (coerce -2.5d0 '(complex real)) '(complex double-float)))
  (#C(1.5 0.0) 0.0 t #C(-2.5d0 0.0d0) t))

(deftest coerce-complex-parts.float-part-type
  (list (typep (coerce -2.5d0 '(complex float)) '(complex double-float))
        (coerce 1 '(complex float))
        (coerce 5 '(complex real))
        (coerce 1.5 '(complex double-float)))
  (t #C(1.0 0.0) 5 #C(1.5d0 0.0d0)))
