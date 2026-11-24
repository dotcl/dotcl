;;; SXHASH of integers mixes all the bits: -1, 0 and most-positive-fixnum, and
;;; numbers whose two 32-bit halves are equal, no longer share one value. Equal
;;; objects still hash alike (CLHS 18.2.3.2), and SBCL's values for general
;;; vectors (one value for all of them) are not asked for.

(defun %sfm-distinct (list)
  (length (remove-duplicates (mapcar #'sxhash list))))

(deftest sxhash-fixnum-mix.special-values
  (list (= (length (remove-duplicates
                    (list (sxhash 0) (sxhash -1) (sxhash 1) (sxhash most-positive-fixnum)
                          (sxhash most-negative-fixnum))))
           5)
        (= (sxhash 0) (sxhash 0))
        (= (sxhash most-positive-fixnum) (sxhash (1- (1+ most-positive-fixnum)))))
  (t t t))

(deftest sxhash-fixnum-mix.consecutive-fixnums
  ;; 1000 consecutive fixnums, from negative to positive: at most one collision
  ;; in the 30-bit result.
  (>= (%sfm-distinct (loop for i from -500 below 500 collect i)) 999)
  t)

(deftest sxhash-fixnum-mix.equal-halves
  ;; k * (2^32 + 1) has equal halves: the xor of the halves was 0 for all of them.
  (>= (%sfm-distinct (loop for k from 1 to 1000 collect (* k (1+ (expt 2 32))))) 999)
  t)

(deftest sxhash-fixnum-mix.powers-of-two
  (>= (%sfm-distinct (loop for i from 0 below 63 collect (ash 1 i))) 62)
  t)

(deftest sxhash-fixnum-mix.equal-implies-same-hash
  (every (lambda (x) (= (sxhash x) (sxhash (+ x 0))))
         (list -1 0 1 255 65536 (expt 2 31) (expt 2 32) (1- (expt 2 62)) (- (expt 2 62))))
  t)

(deftest sxhash-fixnum-mix.numbers-by-value
  (list (= (sxhash 1.5d0) (sxhash (+ 1.0d0 0.5d0)))
        (= (sxhash 3/4) (sxhash (/ 6 8)))
        (/= (sxhash 1.5d0) (sxhash 2.5d0)))
  (t t t))

(deftest sxhash-fixnum-mix.user-hash-table
  ;; A table keyed with :hash-function #'sxhash over keys whose halves are equal.
  (let ((h (make-hash-table :test 'eql :hash-function #'sxhash)))
    (loop for k from 1 to 2000 do (setf (gethash (* k (1+ (expt 2 32))) h) k))
    (list (hash-table-count h)
          (gethash (* 1500 (1+ (expt 2 32))) h)))
  (2000 1500))

(deftest sxhash-fixnum-mix.signed-zero
  ;; 0.0 and -0.0 are similar, so they hash alike (ansi-test SXHASH.17-19)
  (list (= (sxhash 0.0f0) (sxhash -0.0f0))
        (= (sxhash 0.0d0) (sxhash -0.0d0))
        (= (sxhash (complex 0.0d0 0.0d0)) (sxhash (complex -0.0d0 -0.0d0)))
        (= (sxhash (complex 0 0.0d0)) (sxhash (complex 0 -0.0d0)))
        (= (sxhash (complex 0.0f0 1.0f0)) (sxhash (complex -0.0f0 1.0f0))))
  (t t t t t))
