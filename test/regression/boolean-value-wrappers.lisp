;;; A test whose value comes through (VALUES X), (PROGN X), (THE TYPE X) or
;;; (LOCALLY (DECLARE (OPTIMIZE ..)) X) branches on X itself, so a comparison of
;;; raw integers stays a raw comparison.

(setf dotcl:*save-sil* t)

(defun %bvw-test (i n)
  (declare (fixnum i n))
  (if (the (values boolean &optional)
           (locally (declare (optimize (safety 0)))
             (let ((a i) (b n)) (values (progn (< a b))))))
      :less
      :not-less))

(defun %bvw-special ()
  (let ((x nil))
    (declare (special x))
    (let ((x t))
      (if (locally (declare (special x)) x) :true :false))))

(deftest boolean-value-wrappers.values
  (list (%bvw-test 1 2) (%bvw-test 2 1) (%bvw-special)
        (if (values nil t) :t :nil) (if (the t (values 1 nil)) :t :nil))
  (:less :not-less :false :nil :t))

(deftest-compiled-only boolean-value-wrappers.raw
  (and (search "Runtime.LessThan" (%flat-text (dotcl:function-sil #'%bvw-test))) t)
  nil)

(setf dotcl:*save-sil* nil)
