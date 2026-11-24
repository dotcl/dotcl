;;; At (safety 3) the variable of DOTIMES declared a narrower type than the counter is checked each
;;; time round, including the last, when it holds the count itself; and a LOOP WITH variable declared
;;; OF-TYPE is checked at its initial value. Every expected value is what SBCL gives as well.

(defmacro %dl-run (form)
  `(handler-case (funcall (compile nil '(lambda () ,form))) (type-error () :type-error)))

(deftest-emitting-only dotimes-declared-type-safety3.over
  (%dl-run (dotimes (i 5) (declare (type (integer 0 3) i) (optimize (safety 3))) i))
  :type-error)

(deftest-emitting-only dotimes-declared-type-safety3.collects-then-overflows
  (%dl-run (let ((r nil))
             (dotimes (i 4) (declare (type (integer 0 3) i) (optimize (safety 3))) (push i r))
             r))
  :type-error)

(deftest-emitting-only dotimes-declared-type-safety3.result-form
  (%dl-run (let ((r nil))
             (dotimes (i 4 i) (declare (type (integer 0 3) i) (optimize (safety 3))) (push i r))))
  :type-error)

(deftest-emitting-only dotimes-declared-type-safety3.count-itself-fits
  (%dl-run (dotimes (i 3 i) (declare (type (integer 0 3) i) (optimize (safety 3)))))
  3)

(deftest-emitting-only dotimes-declared-type-safety3.count-itself-fits-bit
  (%dl-run (dotimes (i 1 i) (declare (type bit i) (optimize (safety 3)))))
  1)

(deftest-emitting-only dotimes-declared-type-safety3.bit-over
  (%dl-run (dotimes (i 5) (declare (type bit i) (optimize (safety 3))) i))
  :type-error)

(deftest-emitting-only dotimes-declared-type-safety3.short-form
  (%dl-run (dotimes (i 3) (declare (bit i) (optimize (safety 3))) i))
  :type-error)

(deftest-emitting-only dotimes-declared-type-safety3.limit-one-past
  (%dl-run (dotimes (i 5 i) (declare (type (integer 0 4) i) (optimize (safety 3)))))
  :type-error)

(deftest-emitting-only dotimes-declared-type-safety3.exact-range
  (%dl-run (dotimes (i 5 i) (declare (type (integer 0 5) i) (optimize (safety 3)))))
  5)

(deftest-emitting-only dotimes-declared-type-safety3.zero-count
  (%dl-run (dotimes (i 0 i) (declare (type bit i) (optimize (safety 3)))))
  0)

(deftest-emitting-only dotimes-declared-type-safety3.body-sum
  (%dl-run (let ((s 0))
             (dotimes (i 4 s) (declare (type (integer 0 4) i) (optimize (safety 3))) (incf s i))))
  6)

(deftest-emitting-only loop-with-of-type-safety3.string-init
  (%dl-run (locally (declare (optimize (safety 3)))
             (loop with x of-type fixnum = "s" repeat 1 collect x)))
  :type-error)

(deftest-emitting-only loop-with-of-type-safety3.range-init
  (%dl-run (locally (declare (optimize (safety 3)))
             (loop with x of-type (integer 0 3) = 7 repeat 1 collect x)))
  :type-error)

(deftest-emitting-only loop-with-of-type-safety3.bit-init
  (%dl-run (locally (declare (optimize (safety 3)))
             (loop with x of-type bit = 2 repeat 1 collect x)))
  :type-error)

(deftest-emitting-only loop-with-of-type-safety3.in-range-init
  (%dl-run (locally (declare (optimize (safety 3)))
             (loop with x of-type fixnum = 5 repeat 1 collect x)))
  (5))

(deftest-emitting-only loop-with-of-type-safety3.setq-out
  (%dl-run (let () (declare (optimize (safety 3)))
             (loop with x of-type fixnum = 0 do (setq x "s") (return 1))))
  :type-error)
