;;; = between a float and a fixnum compares the values exactly (CLHS 12.1.4.1)
;;; without turning the float into a rational first. ZEROP on a float went
;;; through that conversion and allocated on every call.

(deftest float-fixnum-numeric-equal.values
  (list (= 2.0 2) (= 2 2.0) (= 2.5 2) (= 2 2.5)
        (= -0.0 0) (= 0 -0.0) (= 0.0d0 0)
        (= -3.0d0 -3) (= -3.0d0 3)
        (= 1.0e10 10000000000) (= 1.0e10 10000000001)
        (= 4.611686018427387904d18 (expt 2 62))
        (= 9.223372036854775808d18 (1- (expt 2 63)))
        (= -9.223372036854775808d18 (- (expt 2 63)))
        (= 1.0d300 (expt 10 300))
        (zerop 0.0) (zerop -0.0d0) (zerop 1.0e-30)
        (/= 2.0 2) (/= 2.5 2))
  (t t nil nil
   t t t
   t nil
   t nil
   t
   nil
   t
   nil
   t t nil
   nil t))

(defun %ffne-zerop-count (xs)
  (let ((n 0))
    (dolist (x xs n) (when (zerop x) (incf n)))))

(deftest-compiled-only float-fixnum-numeric-equal.zerop-no-alloc
  (let ((xs (list 3.0 0.0 123456.0 -7.5 1.0e9 2.0d0 1.0d12)))
    (%ffne-zerop-count xs)
    (let ((b0 (nth 4 (dotcl:gc-stats))))
      (dotimes (k 10000) (%ffne-zerop-count xs))
      (< (/ (- (nth 4 (dotcl:gc-stats)) b0) 10000) 16)))
  t)
