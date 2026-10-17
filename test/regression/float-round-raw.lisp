;;; FLOOR, TRUNCATE, CEILING and ROUND of one float of a statically known format
;;; round the raw float; MULTIPLE-VALUE-BIND of them takes the remainder as the
;;; float minus the rounded value. The values are those of the generic entry.

(setf dotcl:*save-sil* t)

(defun %frr-s (x)
  (declare (single-float x))
  (list (multiple-value-list (floor x)) (multiple-value-list (truncate x))
        (multiple-value-list (ceiling x)) (multiple-value-list (round x))
        (multiple-value-bind (q r) (floor x) (list q r))
        (multiple-value-bind (q r) (truncate x) (list q r))
        (multiple-value-bind (q r) (ceiling x) (list q r))
        (multiple-value-bind (q r) (round x) (list q r))
        (multiple-value-bind (q) (round x) q)
        (list (floor x) (truncate x) (ceiling x) (round x))
        (list (ffloor x) (ftruncate x) (fceiling x) (fround x))))

(defun %frr-d (x)
  (declare (double-float x))
  (list (multiple-value-list (floor x)) (multiple-value-list (truncate x))
        (multiple-value-list (ceiling x)) (multiple-value-list (round x))
        (multiple-value-bind (q r) (floor x) (list q r))
        (multiple-value-bind (q r) (truncate x) (list q r))
        (multiple-value-bind (q r) (ceiling x) (list q r))
        (multiple-value-bind (q r) (round x) (list q r))
        (multiple-value-bind (q) (round x) q)
        (list (floor x) (truncate x) (ceiling x) (round x))
        (list (ffloor x) (ftruncate x) (fceiling x) (fround x))))

(defun %frr-generic (x)
  (let ((fl #'floor) (tr #'truncate) (ce #'ceiling) (ro #'round)
        (ffl #'ffloor) (ftr #'ftruncate) (fce #'fceiling) (fro #'fround))
    (list (multiple-value-list (funcall fl x)) (multiple-value-list (funcall tr x))
          (multiple-value-list (funcall ce x)) (multiple-value-list (funcall ro x))
          (multiple-value-list (funcall fl x)) (multiple-value-list (funcall tr x))
          (multiple-value-list (funcall ce x)) (multiple-value-list (funcall ro x))
          (values (funcall ro x))
          (list (values (funcall fl x)) (values (funcall tr x))
                (values (funcall ce x)) (values (funcall ro x)))
          (list (values (funcall ffl x)) (values (funcall ftr x))
                (values (funcall fce x)) (values (funcall fro x))))))

(defun %frr-same (f xs)
  (loop for x in xs
        unless (equalp (funcall f x) (%frr-generic x))
          collect x))

(defun %frr-error (f x)
  (handler-case (progn (funcall f x) :no-error)
    (arithmetic-error () :arithmetic-error)
    (error () :error)))

(defun %frr-trunc-s (x)
  (declare (single-float x))
  (multiple-value-bind (q r) (truncate x) (values q r)))
(defun %frr-trunc-d (x)
  (declare (double-float x))
  (multiple-value-bind (q r) (truncate x) (values q r)))
(defun %frr-identity (x) x)
(declaim (notinline %frr-identity))

(deftest float-round-raw.single
  (%frr-same #'%frr-s
             (list 0f0 -0f0 0.5f0 -0.5f0 1.5f0 2.5f0 -2.5f0 3.7f0 -3.7f0 0.49999997f0
                   1f7 8388609f0 1f20 -1f20 most-positive-single-float
                   least-positive-single-float))
  nil)

(deftest float-round-raw.double
  (%frr-same #'%frr-d
             (list 0d0 -0d0 0.5d0 -0.5d0 1.5d0 2.5d0 -2.5d0 3.7d0 -3.7d0
                   4503599627370497d0 9.2d18 -9.3d18 1d300 least-positive-double-float))
  nil)

(deftest float-round-raw.signed-zero-remainder
  (flet ((sign (f x) (multiple-value-bind (q r) (funcall f x) (list q (float-sign r)))))
    (list (sign #'%frr-trunc-s -0f0) (sign #'%frr-trunc-d -0d0)
          (sign #'%frr-trunc-s 0f0) (sign #'%frr-trunc-s -0.5f0)))
  ((0 -1.0) (0 -1d0) (0 1.0) (0 -1.0)))


(deftest float-round-raw.infinity
  (let ((inf (/ 1d0 (%frr-identity 0d0))))
    (list (%frr-error #'%frr-trunc-d inf) (%frr-error #'%frr-trunc-s (coerce inf 'single-float))
          (%frr-error #'%frr-generic inf)))
  (:arithmetic-error :arithmetic-error :arithmetic-error))


(deftest-compiled-only float-round-raw.no-generic
  (let ((s (%flat-text (dotcl:function-sil #'%frr-trunc-s)))
        (d (%flat-text (dotcl:function-sil #'%frr-trunc-d))))
    (list (search "TruncateOp" s) (search "BindNth" s)
          (search "TruncateOp" d) (search "BindNth" d)))
  (nil nil nil nil))

(setf dotcl:*save-sil* nil)
