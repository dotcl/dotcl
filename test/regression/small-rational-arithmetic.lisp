;;; + - * / on two rationals, at least one a ratio, whose parts are small enough to be computed
;;; in a long. The answer must be the one the integer formulas give: (n1 d2 + n2 d1) / (d1 d2)
;;; and so on, each divided out by / on integers (which does not go through the ratio paths).

(defun %sra-rational (n d) (/ n d))

(defun %sra-oracle (op n1 d1 n2 d2)
  (ecase op
    (+ (/ (+ (* n1 d2) (* n2 d1)) (* d1 d2)))
    (- (/ (- (* n1 d2) (* n2 d1)) (* d1 d2)))
    (* (/ (* n1 n2) (* d1 d2)))
    (/ (/ (* n1 d2) (* d1 n2)))))

(defun %sra-ops (a b op)
  (ecase op (+ (+ a b)) (- (- a b)) (* (* a b)) (/ (/ a b))))

(defun %sra-run (values-list)
  (let ((bad 0) (count 0))
    (dolist (n1 values-list)
      (dolist (d1 '(1 2 3 7 1000 2147483647))
        (dolist (n2 values-list)
          (dolist (d2 '(1 3 5 11 65536 2147483647))
            (dolist (op '(+ - * /))
              (unless (and (eq op '/) (zerop n2))
                (incf count)
                (let ((a (%sra-rational n1 d1)) (b (%sra-rational n2 d2)))
                  (unless (eql (%sra-ops a b op) (%sra-oracle op n1 d1 n2 d2))
                    (incf bad)))))))))
    (list bad (plusp count))))

(deftest small-rational-arithmetic.matches-integer-formulas
  (%sra-run '(0 1 -1 2 -3 5 -7 100 -255 65535 2147483646 -2147483647 2147483647 4294967297 -4294967296))
  (0 t))

(deftest small-rational-arithmetic.results
  (list (+ 1/3 1/6) (- 1/2 1/2) (* 2/3 3/2) (/ 1/3 1/6) (/ 3 1/2) (+ 1/2 1) (- 1 1/3)
        (* -2/3 -3/2) (/ -1/2 1/4) (+ 2147483647/3 1/3) (* 2147483647/2 2/2147483647))
  (1/2 0 1 2 6 3/2 2/3 1 -2 2147483648/3 1))

(deftest small-rational-arithmetic.division-by-zero
  (list (handler-case (/ 1/2 0) (division-by-zero () :division-by-zero))
        (handler-case (/ 1/2 (- 1/2 1/2)) (division-by-zero () :division-by-zero)))
  (:division-by-zero :division-by-zero))

(deftest small-rational-arithmetic.ratio-with-float
  (list (* 1/3 0.5d0) (/ 1/4 0.5d0) (+ 1/2 0.25d0) (- 0.75d0 1/4) (/ 2.0d0 1/4))
  (0.16666666666666666d0 0.5d0 0.75d0 0.5d0 8.0d0))
