;;; A place that is a call to a MACROLET macro, under the SETF family, in the
;;; tree-walk evaluator. Its MACROLET bindings are lexical (not in the global
;;; macro table), and the SETF expanders look a place's operator up without an
;;; environment, so (setf (ref 0) 5) became a call to an undefined (SETF REF)
;;; on the emit-free build.

(defun imsp-set (v)
  (macrolet ((ref (x) `(aref v ,x)))
    (setf (ref 0) 5)
    v))

(deftest interp-macrolet-setf-place.setf
  (coerce (imsp-set (vector 1 2)) 'list)
  (5 2))

(deftest interp-macrolet-setf-place.modify-macros
  (let ((v (vector 1 2)))
    (macrolet ((ref (x) `(aref v ,x)))
      (incf (ref 1) 10)
      (push 7 (ref 0))
      (rotatef (ref 0) (ref 1))
      (coerce v 'list)))
  (12 (7 . 1)))

(deftest interp-macrolet-setf-place.nested
  (let ((v (vector 1 2)))
    (macrolet ((ref (x) `(aref v ,x)))
      (macrolet ((first-ref () `(ref 0)))
        (setf (first-ref) :a)))
    (coerce v 'list))
  (:a 2))
