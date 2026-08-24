;;; A complex never has one float part and one rational part.
;;;
;;; Adding a real to a complex sends the real parts through the operation and
;;; carries the imaginary part across untouched, so contagion has to be applied
;;; to the pair when the complex is rebuilt, not only to the part the operation
;;; produced. Without that, (+ 3.5d0 #c(1 2)) answered #C(4.5d0 2): a value with
;;; a double real part and an integer imaginary part, which CLHS 12.1.4.4 and
;;; 12.1.5.3 do not allow and which no other implementation produces.
;;;
;;; Every expected value here was read off SBCL.

(deftest complex-float-contagion.real-plus-complex-rational
  (list (+ 3.5d0 #c(1 2)) (- 3.5d0 #c(1 2))
        (+ #c(1 2) 3.5d0) (- #c(1 2) 3.5d0)
        (+ 1.5 #c(1 2)) (+ #c(1/2 3/4) 1.0))
  (#c(4.5d0 2.0d0) #c(2.5d0 -2.0d0)
   #c(4.5d0 2.0d0) #c(-2.5d0 2.0d0)
   #c(2.5 2.0) #c(1.5 0.75)))

;;; Mixed float widths widen the same way: the wider format wins for both parts.
(deftest complex-float-contagion.mixed-float-widths
  (list (+ 3.5d0 #c(1.5 -2.25)) (- #c(1.5 -2.25) 3.5d0))
  (#c(5.0d0 -2.25d0) #c(-2.0d0 -2.25d0)))

;;; Both parts of every result are the same type, which is the property that was
;;; being broken.
(deftest complex-float-contagion.parts-agree
  (mapcar (lambda (z) (eq (type-of (realpart z)) (type-of (imagpart z))))
          (list (+ 3.5d0 #c(1 2)) (- 3.5d0 #c(1 2)) (+ 1.5 #c(1 2))
                (+ 3.5d0 #c(1.5 -2.25)) (* 3.5d0 #c(1 2)) (/ #c(1 2) 2.0d0)))
  (t t t t t t))

;;; Exact arithmetic is untouched, and an exact zero imaginary part still
;;; collapses the complex to a real.
(deftest complex-float-contagion.exact-is-unchanged
  (list (+ 1 #c(1 2)) (- #c(1 2) #c(1 2)) (* #c(1 2) #c(1 2))
        (complexp (- #c(1 2) #c(1 2))))
  (#c(2 2) 0 #c(-3 4) nil))

;;; A float zero imaginary part does not collapse: the value stays complex.
(deftest complex-float-contagion.float-zero-stays-complex
  (list (complexp (- #c(1.0d0 2.0d0) #c(0.0d0 2.0d0)))
        (- #c(1.0d0 2.0d0) #c(0.0d0 2.0d0))
        (complexp (+ #c(1 2) #c(0.0d0 -2.0d0))))
  (t #c(1.0d0 0.0d0) t))

;;; ABS of a complex is its magnitude, (sqrt (+ (* r r) (* i i))). SQRT of a
;;; rational answers in the default float format, so the magnitude of a complex
;;; rational is a single float -- ABS used to be the one place that said double
;;; where SQRT of the same number said single. Expected values from SBCL.
(deftest complex-float-contagion.abs-format
  (list (abs #c(1 2)) (abs #c(3 4)) (abs #c(1/2 3/4))
        (abs #c(1.5 -2.25)) (abs #c(1.5d0 -2.25d0)))
  (2.236068 5.0 0.9013878 2.7041636 2.704163456597992d0))

(deftest complex-float-contagion.abs-agrees-with-sqrt
  (list (eq (type-of (abs #c(1 2))) (type-of (sqrt 5)))
        (eq (type-of (abs #c(1/2 3/4))) (type-of (sqrt 5/2)))
        (eq (type-of (abs #c(1.5d0 0.0d0))) (type-of (sqrt 2.25d0))))
  (t t t))
