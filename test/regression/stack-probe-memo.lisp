;;; The periodic stack check remembers the deepest point at which it proved
;;; enough headroom and answers shallower checks from that. These pin that a
;;; runaway recursion is still caught as STORAGE-CONDITION after many shallow
;;; checks have passed, and caught again on a second attempt.

(defun spm-deep (n) (if (= n 0) 0 (1+ (spm-deep (1- n)))))

(defun spm-runaway () (handler-case (spm-deep most-positive-fixnum)
                        (storage-condition () :caught)))

(deftest stack-probe-memo.after-shallow-checks
  (progn
    (dotimes (i 2000) (spm-deep 50))
    (list (spm-runaway) (spm-deep 1000) (spm-runaway)))
  (:caught 1000 :caught))
