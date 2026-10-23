;;; A tail self-call into a parameter held in a raw Int64 slot computes the
;;; argument as an int64 (overflow-checked), also through LET and VALUES around
;;; it, instead of boxing it and unboxing it again.

(setf dotcl:*save-sil* t)

(defun %trla-count (i n acc)
  (declare (fixnum i n acc))
  (if (>= i n)
      (list acc)
      (%trla-count (let ((a i) (b 1)) (values (+ a b))) n (+ acc i))))

(defun %trla-labels (n)
  (declare (fixnum n))
  (labels ((rec (i acc)
             (declare (fixnum i acc))
             (if (>= i n) acc (rec (let ((a i)) (values (1+ a))) (+ acc i)))))
    (rec 0 0)))

(defun %trla-overflow (i)
  (declare (fixnum i))
  (if (< i 0) i (%trla-overflow (+ i most-positive-fixnum))))

(deftest tco-raw-long-args.values
  (list (%trla-count 0 10 0) (%trla-labels 10)
        (handler-case (%trla-overflow 5) (error () :error)))
  ((45) 45 :error))

(deftest-compiled-only tco-raw-long-args.raw
  (and (search "Runtime.AddFixnum)" (%flat-text (dotcl:function-sil #'%trla-count))) t)
  nil)

(setf dotcl:*save-sil* nil)
