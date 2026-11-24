;;; Assigning a constant outside the int64 range to a variable declared FIXNUM
;;; compiles, and signals a TYPE-ERROR when the code runs.

(defun %fsc-run (form)
  (handler-case (funcall (compile nil `(lambda () ,form)))
    (type-error () :type-error)))

(deftest-emitting-only fixnum-setq-out-of-range-constant.errors
  (list (%fsc-run '(let ((x 0)) (declare (fixnum x)) (setq x (expt 2 63)) x))
        (%fsc-run '(let ((x 0)) (declare (fixnum x)) (setq x (logxor (expt 2 63) 1000000)) x))
        (%fsc-run '(let ((x 0)) (declare (fixnum x)) (setq x (+ (expt 2 62) (expt 2 62))) x))
        (%fsc-run '(let ((x 0)) (declare (fixnum x)) (setq x (ash 1 70)) x))
        (%fsc-run '(let ((x 0)) (declare (fixnum x)) (setq x (- (expt 2 62) 1)) (> x 0)))
        (%fsc-run '(let ((x 0)) (declare (fixnum x)) (setq x (- (expt 2 62))) (< x 0))))
  (:type-error :type-error :type-error :type-error t t))
