;;; A LET* binding declared SPECIAL hides the outer lexical variable of the same
;;; name for the later init forms of the same LET*.

(deftest let-star-special.later-init-sees-dynamic
  (let ((x 5))
    (let* ((x (1+ x)) (z (1+ x)))
      (declare (special x))
      z))
  7)

(deftest let-star-special.first-init-sees-outer
  (let ((x 5))
    (let* ((x (1+ x)) (z (1+ x)))
      (declare (special x))
      (list x z (symbol-value 'x))))
  (6 7 6))

(deftest let-star-special.body-and-function-parameter
  ((lambda (x)
     (let* ((x (* 2 x)) (z (+ x 1)))
       (declare (special x))
       (list z (symbol-value 'x))))
   5)
  (11 10))
