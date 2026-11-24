;;; SIMPLE-VECTOR-P in a test position is an inline type test, not a call through the symbol's
;;; function cell: same answers for every kind of object, and no call to the function.

(setf dotcl:*save-sil* t)

(defun %svp-kind (x) (if (simple-vector-p x) :simple :other))
(defun %svp-value (x) (simple-vector-p x))

(deftest simple-vector-p-inline.answers
  (list (%svp-kind (vector 1 2)) (%svp-kind "abc") (%svp-kind (make-array 2 :element-type 'bit))
        (%svp-kind (make-array 2 :adjustable t)) (%svp-kind (make-array 3 :fill-pointer 1))
        (%svp-kind (make-array '(2 2))) (%svp-kind '(1)) (%svp-kind nil) (%svp-kind 5)
        (%svp-value (vector)) (%svp-value '(1)))
  (:simple :other :other :other :other :other :other :other :other t nil))

(deftest-emitting-only simple-vector-p-inline.no-call
  (let ((sil (%flat-text (dotcl:function-sil (function %svp-kind)))))
    (and (search "IsTrueSimpleVectorp" sil) (not (search "GetFunctionBySymbol" sil))))
  t)
