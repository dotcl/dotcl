;;; #'(SETF DOTCL-MOP:SLOT-VALUE-USING-CLASS) names the generic function on
;;; every evaluator. DOTCL-MOP's symbols take the function of the same-named
;;; runtime symbol when the package is set up, and the setf function was not
;;; taken with it: compiled code still found it through a fallback, but the
;;; emit-free build (FDEFINITION) reported it undefined, so
;;; (remove-method #'(setf dotcl-mop:slot-value-using-class) m) failed there.

(defclass mopsfn-c () ((a :initarg :a)))

(deftest mop-setf-function-names.svuc
  (let ((f (fdefinition '(setf dotcl-mop:slot-value-using-class))))
    (list (fboundp '(setf dotcl-mop:slot-value-using-class))
          (typep f 'generic-function)
          (eq f #'(setf dotcl-mop:slot-value-using-class))))
  (t t t))

(deftest mop-setf-function-names.remove-method
  (let* ((gf #'(setf dotcl-mop:slot-value-using-class))
         (m (eval '(defmethod (setf dotcl-mop:slot-value-using-class) :after
                     (v (c standard-class) (o mopsfn-c) s)
                     (declare (ignore v c o s))
                     nil))))
    (list (not (null (member m (dotcl-mop:generic-function-methods gf))))
          (progn (remove-method #'(setf dotcl-mop:slot-value-using-class) m)
                 (not (null (member m (dotcl-mop:generic-function-methods gf)))))
          (let ((o (make-instance 'mopsfn-c :a 1)))
            (setf (slot-value o 'a) 2)
            (slot-value o 'a))))
  (t nil 2))
