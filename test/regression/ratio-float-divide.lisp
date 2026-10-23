;;; Division of fixnums that leaves a ratio, a ratio converted to a double, and
;;; a double divided by a rational (cl-bench mandelbrot/dfloat: (/ I N 0.25d0)).
;;; The operands come in as arguments, so nothing is folded at compile time.
;;; Values were checked against SBCL.

(defun %rfd-div (a b) (/ a b))
(defun %rfd-div3 (a b c) (/ a b c))
(defun %rfd-float (r) (float r 1d0))

(deftest ratio-float-divide.fixnum-ratio
  (list (%rfd-div 7 21) (%rfd-div -6 4) (%rfd-div 6 -4) (%rfd-div -6 -4)
        (%rfd-div 3 9223372036854775807) (%rfd-div -9223372036854775807 2)
        (%rfd-div 9223372036854775806 4) (%rfd-div 1 -9223372036854775807)
        (%rfd-div -9223372036854775808 3) (%rfd-div 3 -9223372036854775808)
        (%rfd-div 0 5) (%rfd-div 10 -5)
        (handler-case (%rfd-div 1 0) (division-by-zero () :dbz)))
  (1/3 -3/2 -3/2 3/2 3/9223372036854775807 -9223372036854775807/2
   4611686018427387903/2 -1/9223372036854775807
   -9223372036854775808/3 -3/9223372036854775808 0 -2 :dbz))

(deftest ratio-float-divide.ratio-to-double
  (list (%rfd-float 1/3) (%rfd-float 2/3) (%rfd-float -7/3) (%rfd-float 1/10)
        (%rfd-float 9007199254740993/9007199254740992)
        (%rfd-float 9007199254740991/9007199254740992)
        (%rfd-float 1/9007199254740993)
        (%rfd-float 9007199254740993/3) (%rfd-float -9007199254740992/3)
        (%rfd-div 7/3 0.5d0) (%rfd-div 0.5d0 7/3) (%rfd-div3 2 3 4d0)
        (%rfd-div 3 0.25d0) (%rfd-div 0.25d0 3) (%rfd-div 1/3 2.0)
        (%rfd-div3 7 100 0.25d0) (%rfd-div3 99 100 0.25d0)
        (%rfd-div 1 0d0) (%rfd-div -1/3 0d0)
        (%rfd-div 0.5d0 #c(1 1)) (%rfd-div 0.5d0 #c(1d0 1d0)) (%rfd-div #c(1d0 1d0) 2))
  (0.3333333333333333d0 0.6666666666666666d0 -2.3333333333333335d0 0.1d0 1.0d0
   0.9999999999999999d0 1.1102230246251564d-16 3.002399751580331d15
   -3.0023997515803305d15 4.666666666666667d0 0.21428571428571427d0
   0.16666666666666666d0 12.0d0 0.08333333333333333d0 0.16666667 0.28d0 3.96d0
   #.dotcl:double-float-positive-infinity #.dotcl:double-float-negative-infinity
   #c(0.25d0 -0.25d0) #c(0.25d0 -0.25d0) #c(0.5d0 0.5d0)))

(deftest ratio-float-divide.sum
  (let ((s 0))
    (loop for i from 1 to 300
          do (loop for j from 1 to 40 do (setq s (+ s (%rfd-div3 i j 0.25d0)))))
    s)
  772704.872831909d0)
