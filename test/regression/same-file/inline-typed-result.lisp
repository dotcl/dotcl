(defun %sfit-long (x) (if x 1 0))
(declaim (ftype (function (t) fixnum) %sfit-long))

(defun %sfit-double (x) (if x 1.5d0 0d0))
(declaim (ftype (function (t) double-float) %sfit-double))

(defun %sfit-long-caller (x)
  (let ((a (%sfit-long x))) (list a (> 2 a))))

(defun %sfit-double-caller (x)
  (let ((a (%sfit-double x))) (list a (> 2d0 a))))

(defun %sfit-trim (v lo)
  (declare (optimize (speed 3) (safety 0)))
  (let ((split-lo (if (eq lo :none) 0 (%sfit-long lo)))
        (split-hi (length (the simple-vector v))))
    (> split-hi split-lo)))
