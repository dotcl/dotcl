;;; A checked fixnum add of a literal that is not negative tests overflow with
;;; one compare. The overflow is still the same error.

(setf dotcl:*save-sil* t)

(defun %fan-inc (i) (declare (fixnum i)) (the fixnum (+ i 1)))
(defun %fan-add0 (i) (declare (fixnum i)) (the fixnum (+ i 0)))
(defun %fan-dec (i) (declare (fixnum i)) (the fixnum (+ i -1)))

(deftest fixnum-add-nonneg.values
  (list (%fan-inc 41) (%fan-inc -1) (%fan-add0 most-positive-fixnum)
        (%fan-dec 0) (%fan-dec most-positive-fixnum))
  (42 0 #.most-positive-fixnum -1 #.(1- most-positive-fixnum)))

(deftest-compiled-only fixnum-add-nonneg.overflow
  (list (handler-case (%fan-inc most-positive-fixnum) (type-error () :type-error))
        (handler-case (%fan-dec most-negative-fixnum) (type-error () :type-error)))
  (:type-error :type-error))

(deftest-compiled-only fixnum-add-nonneg.emitted
  (and (search "AddFixnumCheckedNonNeg" (%flat-text (dotcl:function-sil #'%fan-inc))) t)
  t)
