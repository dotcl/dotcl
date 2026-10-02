;;; The effective slot's :TYPE is the conjunction of the types declared for the
;;; slot along the class precedence list (CLHS 7.5.3). Only the most specific
;;; declaration used to be kept, which claims values the superclass excludes.
;;; A declared type that is a supertype of another one adds nothing and is left
;;; out, so a subclass that narrows the type still reports the narrow type.

(defclass stc-a () ((s :initarg :s :type (or string symbol)) (n :type integer)))
(defclass stc-b (stc-a) ((s :initarg :s :type (or symbol integer)) (n :type (integer 0 100))))

(defun %stc-type (name)
  (dotcl-mop:finalize-inheritance (find-class 'stc-b))
  (dotcl-mop:slot-definition-type
   (find name (dotcl-mop:class-slots (find-class 'stc-b))
         :key #'dotcl-mop:slot-definition-name)))

(deftest slot-type-conjunction.overlapping-types
  (let ((type (%stc-type 's)))
    (list (subtypep 'symbol type)
          (typep 'foo type) (typep "x" type) (typep 1 type)))
  (t t nil nil))

(deftest slot-type-conjunction.narrowed-type
  (%stc-type 'n)
  (integer 0 100))
