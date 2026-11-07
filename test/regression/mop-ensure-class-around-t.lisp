;;; An :AROUND method on ENSURE-CLASS-USING-CLASS that is not specialised (its
;;; arguments are T) takes the plain DEFCLASS too, so it can choose the metaclass.
;;; A method specialised on T used to be skipped as if it were the default one.

(defclass %meca-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c %meca-meta) (s standard-class)) t)

(defvar *meca-names* nil)

(deftest mop-ensure-class-around-t.chooses-metaclass
  (let ((m (eval '(defmethod dotcl-mop:ensure-class-using-class :around (class name &rest keys)
                    (when (member name '(%meca-indirect))
                      (setf (getf keys :metaclass) '%meca-meta))
                    (apply #'call-next-method class name keys)))))
    (unwind-protect
         (progn (eval '(defclass %meca-indirect () ()))
                (eval '(defclass %meca-other () ()))
                (list (class-name (class-of (find-class '%meca-indirect)))
                      (class-name (class-of (find-class '%meca-other)))))
      (remove-method #'dotcl-mop:ensure-class-using-class m)))
  (%meca-meta standard-class))

(deftest mop-ensure-class-around-t.before-observes
  (let ((m (eval '(defmethod dotcl-mop:ensure-class-using-class :before (class name &rest keys)
                    (declare (ignore class keys))
                    (push name *meca-names*)))))
    (setf *meca-names* nil)
    (unwind-protect
         (progn (eval '(defclass %meca-observed () ()))
                (list (reverse *meca-names*)
                      (class-name (class-of (find-class '%meca-observed)))))
      (remove-method #'dotcl-mop:ensure-class-using-class m)))
  ((%meca-observed) standard-class))

(deftest mop-ensure-class-around-t.no-method-plain-defclass
  (progn (eval '(defclass %meca-plain () ((a :initarg :a))))
         (list (class-name (class-of (find-class '%meca-plain)))
               (slot-boundp (make-instance '%meca-plain :a 1) 'a)))
  (standard-class t))
