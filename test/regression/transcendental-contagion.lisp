;;; LOG, EXPT and the transcendental functions: contagion, complex bases and branch cuts.

(defun transcendental-close-p (actual expected)
  (if (and (complexp actual) (complexp expected))
      (and (transcendental-close-p (realpart actual) (realpart expected))
           (transcendental-close-p (imagpart actual) (imagpart expected)))
      (and (numberp actual)
           (<= (abs (- (coerce actual 'double-float)
                       (coerce expected 'double-float)))
               (* 1d-6 (max 1d0 (abs (coerce expected 'double-float))))))))

(deftest log-complex-base
  (mapcar (lambda (pair) (transcendental-close-p (first pair) (second pair)))
          (list (list (log 2 #C(0 1)) #C(0.0 -0.4412712))
                (list (log 1.0d0 #C(1 1)) #C(0.0d0 0.0d0))))
  (t t))

(deftest log-negative-base
  (mapcar (lambda (pair) (transcendental-close-p (first pair) (second pair)))
          (list (list (log 2.0 -2) #C(0.046420325 -0.21039362))
                (list (log 2 -2) #C(0.046420325 -0.21039362))
                (list (log 2.0d0 -2) #C(0.04642032354540812d0 -0.21039362420793017d0))
                (list (log 1/2 -2) #C(-0.046420325 0.21039362))))
  (t t t t))

(deftest log-zero-base
  (list (log 2 0) (log 2.0d0 0) (log 1/2 0) (log #C(1 1) 0) (log 0 0)
        (type-of (log 2 0)) (type-of (log 2.0d0 0)))
  (0.0 0.0d0 0.0 0.0 0.0 single-float double-float))

(deftest log-float-contagion
  (list (mapcar (lambda (x) (type-of (realpart x)))
                (list (log -1) (log #C(1 1)) (log #C(1 1) 10)
                      (log 1.0 10.0d0) (log 2 2.0d0)
                      (log #C(1.0 2.0) 2.0d0)))
        (log 1.0 10.0d0) (log 2 2.0d0))
  ((single-float single-float single-float double-float double-float double-float)
   0.0d0 1.0d0))

(deftest expt-float-contagion
  (mapcar (lambda (x) (type-of (realpart x)))
          (list (expt #C(1 1) 1/2) (expt #C(1/2 1/3) 3/2)
                (expt #C(1 -2) 2.0) (expt -1 #C(1 1))
                (expt #C(1.0d0 2.0d0) 2.0)))
  (single-float single-float single-float single-float double-float))

(deftest expt-zero-power-complex-type
  (list (expt #C(1/2 1/3) 0.0d0) (expt #C(-2.0 0.0) 0.0d0)
        (expt #C(1/2 1/3) 0.0) (expt #C(1/2 1/3) 0))
  (#C(1.0d0 0.0d0) #C(1.0d0 0.0d0) #C(1.0 0.0) 1))

(deftest transcendental-complex-rational-width
  (mapcar (lambda (x) (type-of (realpart x)))
          (list (asin 2) (acosh -2) (atanh 2)
                (sin #C(1 2)) (exp #C(1 1)) (sqrt #C(1 2))
                (cos #C(1 2)) (tan #C(1 2))
                (sinh #C(1 2)) (cosh #C(1 2)) (tanh #C(1 2))
                (asinh #C(1 2)) (atan #C(1 2))))
  (single-float single-float single-float single-float single-float single-float
   single-float single-float single-float single-float single-float
   single-float single-float))

(deftest transcendental-branch-cuts
  (list (sqrt #C(-1.0d0 -0.0d0))
        (sqrt #C(-1.0d0 0.0d0))
        (transcendental-close-p (asin #C(2.0 -0.0)) #C(1.5707964 -1.3169578))
        (transcendental-close-p (asin #C(2.0d0 0.0d0))
                                #C(1.5707963267948966d0 -1.3169578969248166d0))
        (transcendental-close-p (acos #C(-2.0 0.0)) #C(3.1415927 -1.3169578))
        (transcendental-close-p (acos #C(-2.0d0 -0.0d0))
                                #C(3.141592653589793d0 -1.3169578969248166d0)))
  (#C(0.0d0 -1.0d0) #C(0.0d0 1.0d0) t t t t))

(deftest transcendental-large-real
  (mapcar (lambda (pair) (transcendental-close-p (first pair) (second pair)))
          (list (list (asin -1.0d10) #C(-1.5707963267948966d0 23.7189981105004d0))
                (list (asin -1.0e10) #C(-1.5707964 23.718998))
                (list (acos 1.0d10) #C(0.0d0 23.7189981105004d0))
                (list (acos 1.0e10) #C(0.0 23.718998))
                (list (acos 1.0d300) #C(0.0d0 691.4686750787736d0))))
  (t t t t t))

(deftest transcendental-large-real-width
  (list (type-of (realpart (asin -1.0e10)))
        (type-of (realpart (acos 1.0d10))))
  (single-float double-float))
