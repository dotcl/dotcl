;;; ZEROP and = on bignums and ratios answer without comparing against 0.
;;; Expected values from SBCL.

(defvar *zbr-vals*
  (list 0 1 -1 most-positive-fixnum most-negative-fixnum
        (1+ most-positive-fixnum) (1- most-negative-fixnum)
        (- (1+ most-positive-fixnum) 1) (+ (1- most-negative-fixnum) 1)
        (expt 10 40) (- (expt 10 40)) 1/3 -1/3 (/ (expt 10 30) 7)
        0.0 -0.0d0 (- (expt 2 64) (expt 2 64))))

(deftest zerop-bignum-ratio.funcall
  (mapcar #'zerop *zbr-vals*)
  (t nil nil nil nil nil nil nil nil nil nil nil nil nil t t t))

(defun zbr-test (x) (if (zerop x) :zero :nonzero))

(deftest zerop-bignum-ratio.conditional
  (mapcar #'zbr-test *zbr-vals*)
  (:zero :nonzero :nonzero :nonzero :nonzero :nonzero :nonzero :nonzero
   :nonzero :nonzero :nonzero :nonzero :nonzero :nonzero :zero :zero :zero))

(deftest zerop-bignum-ratio.value-type
  (list (zerop (- (expt 10 40) (expt 10 40)))
        (zerop (* 1/3 3 0))
        (zerop (- 1/3 1/3)))
  (t t t))

;;; A bignum is never = to a fixnum; around the fixnum boundary the answer
;;; must still follow the value.
(deftest zerop-bignum-ratio.integer-equal-boundary
  (let ((mp most-positive-fixnum) (mn most-negative-fixnum))
    (list (= (1+ mp) mp) (= (1- (1+ mp)) mp) (= mp (1- (1+ mp)))
          (= (1- mn) mn) (= (1+ (1- mn)) mn)
          (= (expt 2 63) (expt 2 63)) (= (expt 2 63) (- (expt 2 63)))
          (/= (1+ mp) mp) (= 0 (expt 10 40)) (= (expt 10 40) 0)
          (eql (1- (1+ mp)) mp) (typep (1- (1+ mp)) 'fixnum)))
  (nil t t nil t t nil t nil nil t t))
