;;; (- X) of an integer whose negation is known to fit int64 is computed as a
;;; raw negation, and its range is known to the code around it (an ASH count,
;;; an index, the 64-bit lane). Values must not change, and the one integer
;;; whose negation does not fit, most-negative-fixnum, must still give a bignum.

(defun %uml-small (x)
  (declare (type (integer -1000 1000) x))
  (- x))

(defun %uml-fix (x)
  (declare (type fixnum x))
  (- x))

(defun %uml-sum (x)
  (declare (type (signed-byte 32) x))
  (+ (- x) 7))

(defun %uml-let (x)
  (declare (type (unsigned-byte 64) x))
  (let ((r 6))
    (logxor x (ash x (- r)))))

(defun %uml-shift (x n)
  (declare (type (unsigned-byte 64) x) (type (integer 0 63) n))
  (ash x (- n)))

(defun %uml-index (v i)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (type (integer -9 0) i))
  (aref v (- i)))

(deftest unary-minus-int-lane.values
  (list (%uml-small 5) (%uml-small -1000) (%uml-small 0)
        (%uml-fix 12345) (%uml-fix most-positive-fixnum)
        (%uml-sum 3) (%uml-sum -2147483648))
  (-5 1000 0 -12345 #.(- most-positive-fixnum) 4 2147483655))

(deftest unary-minus-int-lane.most-negative-fixnum
  (let ((r (%uml-fix most-negative-fixnum)))
    (list (= r (ash 1 63)) (typep r 'bignum) (typep (- (+ most-negative-fixnum 1)) 'fixnum)))
  (t t t))

(deftest unary-minus-int-lane.u64
  (list (%uml-let #xFFFFFFFFFFFFFFFF) (%uml-let #x8000000000000000) (%uml-let 64)
        (%uml-shift #xFFFFFFFFFFFFFFFF 60) (%uml-shift #x8000000000000000 0))
  (#xFC00000000000000 #x8200000000000000 65 15 #x8000000000000000))

(deftest unary-minus-int-lane.index
  (let ((v (make-array 10 :element-type '(unsigned-byte 8) :initial-contents '(0 1 2 3 4 5 6 7 8 9))))
    (list (%uml-index v 0) (%uml-index v -9) (%uml-index v -4)))
  (0 9 4))

(deftest unary-minus-int-lane.not-a-number
  (handler-case (progn (%uml-small (the t (identity 1.5))) :no-error)
    (type-error () :type-error))
  :type-error)
