;;; ZEROP and = on single floats, and a float against a fixnum (cl-bench
;;; frpoly/float: (ZEROP C) on single-float coefficients). The operands come in
;;; as arguments. Values were checked against SBCL.

(defun %fze-zerop (x) (zerop x))
(defun %fze-if-zerop (x) (if (zerop x) :z :nz))
(defun %fze-= (a b) (= a b))
(defun %fze-if-= (a b) (if (= a b) :eq :ne))

(deftest float-zerop-equal.zerop
  (let ((xs (list 0.0 -0.0 1.0e-45 -1.0e-45 0.0d0 -0.0d0 1d-300 0 3 1/2 #c(0.0 0.0) #c(0.0 1.0))))
    (list (mapcar #'%fze-zerop xs) (mapcar #'%fze-if-zerop xs)))
  ((t t nil nil t t nil t nil nil t nil)
   (:z :z :nz :nz :z :z :nz :z :nz :nz :z :nz)))

(deftest float-zerop-equal.=
  (let ((pairs '((3.0 3) (3.5 3) (3 3.0) (999999984306749440 1.0e18)
                 (1000000000000000000 1.0e18) (1d18 1000000000000000000)
                 (2.0 2.0) (2.0 2.5) (-0.0 0) (0 -0.0d0) (16777217 16777216.0)
                 (16777216 16777216.0) (1.0e10 10000000000)
                 (-9.223372e18 -9223372036854775808) (9.223372e18 9223372036854775807)
                 (2.5d0 2) (-3d0 -3) (1.0 1d0) (0.1 0.1d0) (1/2 0.5))))
    (list (mapcar (lambda (p) (%fze-= (first p) (second p))) pairs)
          (mapcar (lambda (p) (%fze-if-= (second p) (first p))) pairs)))
  ((t nil t t nil t t nil t t nil t t t nil nil t t nil t)
   (:eq :ne :eq :eq :ne :eq :eq :ne :eq :eq :ne :eq :eq :eq :ne :ne :eq :eq :ne :eq)))

(deftest float-zerop-equal.non-finite
  (list (%fze-= dotcl:double-float-positive-infinity 0)
        (%fze-zerop dotcl:double-float-negative-infinity)
        (%fze-= 0 dotcl:single-float-positive-infinity))
  (nil nil nil))
