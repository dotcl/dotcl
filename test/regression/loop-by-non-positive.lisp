;;; A literal BY of zero or less in an arithmetic LOOP clause is an error, like a
;;; BY value computed at run time that is not positive (CLHS 6.1.2.1.1: BY is a
;;; positive number). A loop with `by -1` and an upper limit used to count down for
;;; ever, and `by 0` repeated the first value for ever.

(defun %lbnp (thunk)
  (handler-case (funcall thunk) (type-error () :type-error)))

(deftest loop-by-non-positive.literal
  (list (%lbnp (lambda () (loop for x from 1 to 2 by -1 collect x)))
        (%lbnp (lambda () (loop for x from 1 to 2 by 0 collect x)))
        (%lbnp (lambda () (loop for x from 5 downto 1 by -1 collect x)))
        (%lbnp (lambda () (loop for x from 1 to 2 by -0.5 collect x))))
  (:type-error :type-error :type-error :type-error))

(deftest loop-by-non-positive.variable
  (let ((s -1) (z 0))
    (list (%lbnp (lambda () (loop for x from 1 to 3 by s collect x)))
          (%lbnp (lambda () (loop for x from 1 to 3 by z collect x)))))
  (:type-error :type-error))

(deftest loop-by-non-positive.positive-unchanged
  (list (loop for x from 1 to 5 by 2 collect x)
        (loop for x from 5 downto 1 by 2 collect x)
        (loop for x below 5 by 1.5 collect x)
        (loop for x from 1 to 3 by 1/2 collect x)
        (loop for x in '(1 2 3 4 5) by #'cddr collect x))
  ((1 3 5) (5 3 1) (0 1.5 3.0 4.5) (1 3/2 2 5/2 3) (1 3 5)))
