;;; A numeric comparison of a native int64 local with a generic operand takes the int64 raw; the
;;; answer is the one the generic comparison gives, for fixnums, bignums, ratios and floats on
;;; either side.

(defmacro %mlc-t (form) `(if ,form t nil))

(defun %mlc-native (a b)
  (declare (fixnum a))
  (progn
    (list (%mlc-t (< a b)) (%mlc-t (> a b)) (%mlc-t (<= a b)) (%mlc-t (>= a b)) (%mlc-t (= a b))
          (%mlc-t (< b a)) (%mlc-t (> b a)) (%mlc-t (<= b a)) (%mlc-t (>= b a)) (%mlc-t (= b a)))))

(defun %mlc-generic (n b)
  (let ((a n))
    (mapcar (lambda (x) (and x t))
            (list (funcall #'< a b) (funcall #'> a b) (funcall #'<= a b) (funcall #'>= a b) (funcall #'= a b)
                  (funcall #'< b a) (funcall #'> b a) (funcall #'<= b a) (funcall #'>= b a) (funcall #'= b a)))))

(deftest mixed-long-compare.matches-generic
  (let ((bad nil))
    (dolist (n '(0 1 -1 7 65535 65536 262143 262144 1000000 -1000000 4611686018427387903 -4611686018427387904))
      (dolist (b (list 0 1 -1 7 65536 262144 1000000 4611686018427387903 (expt 2 70) (- (expt 2 70))
                       1/2 -7/3 3.5d0 -2.5d0 7.0d0 1000000.0d0 (expt 2 62.0d0) 7.0f0))
        (unless (equal (%mlc-native n b) (%mlc-generic n b)) (push (list n b) bad))))
    bad)
  nil)

(deftest mixed-long-compare.bignum-either-side
  (list (%mlc-native 5 (expt 2 80)) (%mlc-native 5 (- (expt 2 80))))
  ((t nil t nil nil nil t nil t nil)
   (nil t nil t nil t nil t nil nil)))
