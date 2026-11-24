;;; (setq s (logand (+ s X) MASK)) on a native integer slot does not box the masked sum.

(defvar *msb-table* (let ((h (make-hash-table))) (dotimes (i 1000) (setf (gethash i h) (* i 2))) h))

(defun %msb-get-sum ()
  (let ((h *msb-table*) (s 0))
    (dotimes (i 100000) (setq s (logand (+ s (gethash (mod i 1000) h)) #xFFFFFF)))
    s))

(deftest-emitting-only masked-sum-box-sum-allocation
  (progn
    (%msb-get-sum)
    (< (min-allocated-bytes #'%msb-get-sum) 100000))
  t)

(deftest masked-sum-box-sum-value
  (%msb-get-sum)
  16013920)
