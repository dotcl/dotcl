;;; A FIXNUM (or bounded integer) parameter or LET variable read only as the
;;; init of another binding that is read in integer arithmetic gets a raw slot
;;; too, so the chain of bindings stays raw.

(setf dotcl:*save-sil* t)

(defun %iliu-eq (n i)
  (declare (fixnum n i))
  (let ((a i) (b n)) (if (= a b) :same :differ)))

(defun %iliu-sum (n)
  (declare (type (integer 0 1000) n))
  (let ((acc 0))
    (declare (fixnum acc))
    (dotimes (i n acc)
      (let ((a acc) (b n)) (setq acc (+ a b))))))

(deftest int-let-init-use.values
  (list (%iliu-eq 3 3) (%iliu-eq 3 4) (%iliu-sum 10)
        (handler-case (%iliu-eq 1.5 1) (type-error () :type-error)))
  (:same :differ 100 :type-error))

(deftest-compiled-only int-let-init-use.raw
  (list (and (search "Runtime.NumEqual" (%flat-text (dotcl:function-sil #'%iliu-eq))) t)
        (and (search "Int64" (%flat-text (dotcl:function-sil #'%iliu-eq))) t))
  (nil t))

(setf dotcl:*save-sil* nil)
