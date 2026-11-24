;;; At (safety 3) a variable declared BIT, (UNSIGNED-BYTE 1) or a small INTEGER range signals a
;;; TYPE-ERROR for a SETQ, an initial value or an INCF that leaves the range. The same type
;;; declared through the short form (declare (bit x)) is checked too. Every expected value is what
;;; SBCL gives as well.

(defmacro %sr-run (form)
  `(handler-case (funcall (compile nil '(lambda () ,form))) (type-error () :type-error)))

(deftest-emitting-only small-range-setq-safety3.bit-setq
  (%sr-run (let ((x 0)) (declare (bit x) (optimize (safety 3))) (setq x 2) x))
  :type-error)

(deftest-emitting-only small-range-setq-safety3.type-bit-setq
  (%sr-run (let ((x 0)) (declare (type bit x) (optimize (safety 3))) (setq x 2) x))
  :type-error)

(deftest-emitting-only small-range-setq-safety3.unsigned-byte-1-setq
  (%sr-run (let ((x 0)) (declare (type (unsigned-byte 1) x) (optimize (safety 3))) (setq x 2) x))
  :type-error)

(deftest-emitting-only small-range-setq-safety3.integer-0-1-setq
  (%sr-run (let ((x 0)) (declare (type (integer 0 1) x) (optimize (safety 3))) (setq x 2) x))
  :type-error)

(deftest-emitting-only small-range-setq-safety3.integer-0-3-setq
  (%sr-run (let ((x 0)) (declare (type (integer 0 3) x) (optimize (safety 3))) (setq x 4) x))
  :type-error)

(deftest-emitting-only small-range-setq-safety3.negative
  (%sr-run (let ((x 0)) (declare (type (integer 0 3) x) (optimize (safety 3))) (setq x -1) x))
  :type-error)

(deftest-emitting-only small-range-setq-safety3.init-short-form
  (%sr-run (let ((x 2)) (declare (bit x) (optimize (safety 3))) x))
  :type-error)

(deftest-emitting-only small-range-setq-safety3.init-computed
  (%sr-run (let ((x (+ 1 1))) (declare (bit x) (optimize (safety 3))) x))
  :type-error)

(deftest-emitting-only small-range-setq-safety3.setq-computed
  (%sr-run (let ((x 0)) (declare (bit x) (optimize (safety 3))) (setq x (+ 1 1)) x))
  :type-error)

(deftest-emitting-only small-range-setq-safety3.setq-variable
  (%sr-run (let* ((x 0) (y 5)) (declare (bit x) (optimize (safety 3))) (setq x y) x))
  :type-error)

(deftest-emitting-only small-range-setq-safety3.incf-out
  (%sr-run (let ((x 1)) (declare (bit x) (optimize (safety 3))) (incf x) x))
  :type-error)

(deftest-emitting-only small-range-setq-safety3.incf-by-two
  (%sr-run (let ((x 0)) (declare (bit x) (optimize (safety 3))) (incf x 2) x))
  :type-error)

(deftest-emitting-only small-range-setq-safety3.dotimes
  (%sr-run (let ((x 0)) (declare (bit x) (optimize (safety 3))) (dotimes (i 3) (setq x i)) x))
  :type-error)

(deftest-emitting-only small-range-setq-safety3.closure-setq
  (%sr-run (let ((x 0) (f nil))
             (declare (bit x) (optimize (safety 3)))
             (setq f (lambda (v) (setq x v)))
             (funcall f 2)
             x))
  :type-error)

(deftest-emitting-only small-range-setq-safety3.in-range-ok
  (%sr-run (let ((x 0)) (declare (bit x) (optimize (safety 3))) (setq x 1) x))
  1)

(deftest-emitting-only small-range-setq-safety3.in-range-loop-ok
  (%sr-run (let ((x 0)) (declare (type (integer 0 3) x) (optimize (safety 3)))
             (dotimes (i 4) (setq x i))
             x))
  3)

(deftest-emitting-only small-range-setq-safety3.in-range-incf-ok
  (%sr-run (let ((x 0)) (declare (bit x) (optimize (safety 3))) (incf x) x))
  1)
