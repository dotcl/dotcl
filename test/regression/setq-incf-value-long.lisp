;;; The value of (SETQ v e), (INCF v [n]) or (DECF v [n]) on a variable held in a raw int64 slot is
;;; used raw: bound to a LET variable (declared or not) it builds no Fixnum above the cache, and the
;;; answers are the same, including the type error for a value that does not fit.

(defun %siv-incf (n) (declare (fixnum n))
  (let ((i 70000) (s 0)) (declare (fixnum i s))
    (dotimes (k n) (let ((next (incf i))) (when (> next 0) (setq s (logand next 255)))))
    (list i s)))
(defun %siv-incf-declared (n) (declare (fixnum n))
  (let ((i 70000) (s 0)) (declare (fixnum i s))
    (dotimes (k n) (let ((next (incf i))) (declare (fixnum next)) (setq s (logand next 255))))
    (list i s)))
(defun %siv-decf (n) (declare (fixnum n))
  (let ((i 70000) (s 0)) (declare (fixnum i s))
    (dotimes (k n) (let ((next (decf i 3))) (setq s (logand next 255))))
    (list i s)))
(defun %siv-setq (n) (declare (fixnum n))
  (let ((i 70000) (s 0)) (declare (fixnum i s))
    (dotimes (k n) (let ((next (setq i (+ i 2)))) (setq s (logand next 255))))
    (list i s)))
(defun %siv-nested (i) (declare (fixnum i)) (logand (incf i 5) (decf i 2)))
(defun %siv-overflow () (let ((i most-positive-fixnum)) (declare (fixnum i)) (let ((next (incf i))) next)))

(deftest setq-incf-value-long.answers
  (list (%siv-incf 5) (%siv-incf-declared 3) (%siv-decf 4) (%siv-setq 6)
        (%siv-nested 100))
  ((70005 #.(logand 70005 255)) (70003 #.(logand 70003 255)) (69988 #.(logand 69988 255)) (70012 #.(logand 70012 255))
   #.(logand 105 103)))

;; A declaration the evaluator path does not check: only compiled code signals.
(deftest-compiled-only setq-incf-value-long.overflow
  (handler-case (%siv-overflow) (type-error () :type-error))
  :type-error)

(defun %siv-loop (n) (declare (fixnum n)) (%siv-incf n))

(deftest-compiled-only setq-incf-value-long.no-allocation
  (progn (%siv-loop 10)
         (< (min-allocated-bytes (lambda () (%siv-loop 100000))) 100000))
  t)
