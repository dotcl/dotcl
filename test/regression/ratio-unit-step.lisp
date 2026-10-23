;;; 1+, 1- and the reciprocal of a ratio, and integer +/- ratio, build the
;;; result straight from the reduced parts. The answers must stay in lowest
;;; terms, keep the sign on the numerator, and collapse to an integer when the
;;; denominator does. Expected values from SBCL.

(defvar *rus-big* (expt 2 63))
(defvar *rus-max* (1- (expt 2 63)))
(defvar *rus-min* (- (expt 2 63)))

(deftest ratio-unit-step.increment
  (mapcar #'1+ (list 1/2 -1/2 -3/2 7/3 -7/3
                     (/ *rus-max* 2) (/ *rus-min* 3)
                     (/ *rus-big* 3) (/ (- *rus-big*) 3)))
  (3/2 1/2 -1/2 10/3 -4/3
   9223372036854775809/2 -9223372036854775805/3
   9223372036854775811/3 -9223372036854775805/3))

(deftest ratio-unit-step.decrement
  (mapcar #'1- (list 1/2 -1/2 3/2 7/3
                     (/ *rus-min* 3) (/ (- *rus-big*) 5)))
  (-1/2 -3/2 1/2 4/3 -9223372036854775811/3 -9223372036854775813/5))

(defun rus-inc (x) (1+ x))
(defun rus-dec (x) (1- x))
(defun rus-recip (x) (/ x))

(deftest ratio-unit-step.compiled
  (list (rus-inc 1/2) (rus-dec -1/2) (rus-recip 1/2) (rus-recip -1/2)
        (rus-recip 2/3) (rus-recip -2/3) (rus-recip (/ 3 *rus-big*))
        (rus-recip (/ -1 *rus-big*)) (rus-recip (/ *rus-big* -7)))
  (3/2 -3/2 2 -2 3/2 -3/2 9223372036854775808/3
   -9223372036854775808 -7/9223372036854775808))

(deftest ratio-unit-step.reciprocal-n-ary
  (list (apply #'/ (list 5/7)) (apply #'/ (list -5/7)) (funcall #'/ -1/9)
        (/ 1 -1/9) (/ 1 (/ 2 *rus-big*)))
  (7/5 -7/5 -9 -9 4611686018427387904))

(deftest ratio-unit-step.integer-plus-ratio
  (list (+ 3 1/2) (+ 1/2 3) (- 3 1/2) (- 1/2 3) (+ -1 1/3) (- -1 -1/3)
        (+ *rus-big* 1/2) (- 1/2 *rus-big*) (+ (expt 10 30) -7/3)
        (* 2/3 1) (* 1 -2/3) (* 3 2/3) (* (expt 10 20) 1/10)
        (* 3/4 (expt 2 70)) (* -3/4 (/ 1 (expt 2 70))))
  (7/2 7/2 5/2 -5/2 -2/3 -2/3
   18446744073709551617/2 -18446744073709551615/2
   2999999999999999999999999999993/3
   2/3 -2/3 2 10000000000000000000 885443715538058477568
   -3/4722366482869645213696))

(deftest ratio-unit-step.continued-fraction-150
  (let ((x 1)) (dotimes (i 149) (setq x (1+ (/ x))))
    (list (numerator x) (denominator x)))
  (16130531424904581415797907386349 9969216677189303386214405760200))

(deftest ratio-unit-step.reciprocal-zero-signals
  (handler-case (progn (rus-recip 0) :no-error)
    (division-by-zero () :division-by-zero))
  :division-by-zero)
