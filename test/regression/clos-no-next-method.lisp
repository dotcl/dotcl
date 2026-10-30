;;; CALL-NEXT-METHOD with no next method must run NO-NEXT-METHOD (CLHS 7.6.6.2).

(defgeneric nnm-user (x))
(defmethod nnm-user ((x integer)) (list :method (call-next-method)))
(defmethod no-next-method ((gf (eql #'nnm-user)) (m standard-method) &rest args)
  (list :no-next (eq gf #'nnm-user) (typep m 'standard-method) args))

(deftest clos-no-next-method.user-method
  (nnm-user 5)
  (:method (:no-next t t (5))))

(defgeneric nnm-values (x))
(defmethod nnm-values ((x integer)) (call-next-method))
(defmethod no-next-method ((gf (eql #'nnm-values)) (m standard-method) &rest args)
  (declare (ignore m args))
  (values 1 2 3))

(deftest clos-no-next-method.returns-values
  (nnm-values 5)
  1 2 3)

(defgeneric nnm-default (x))
(defmethod nnm-default ((x integer)) (call-next-method))

(deftest clos-no-next-method.default-error
  (handler-case (progn (nnm-default 5) :no-error)
    (error () :error))
  :error)

(defgeneric nnm-args (x))
(defmethod nnm-args ((x integer)) (list :method (call-next-method 42)))
(defmethod no-next-method ((gf (eql #'nnm-args)) (m standard-method) &rest args)
  (declare (ignore m))
  (list :no-next args))

(deftest clos-no-next-method.call-next-method-with-arguments
  (nnm-args 5)
  (:method (:no-next (42))))

(defvar *nnm-trace* nil)
(defgeneric nnm-aux (x))
(defmethod no-next-method ((gf (eql #'nnm-aux)) (m standard-method) &rest args)
  (list :no-next (method-qualifiers m) args))
(defmethod nnm-aux ((x integer))
  (push (list :primary (call-next-method)) *nnm-trace*)
  :primary)
(defmethod nnm-aux :before ((x integer))
  (push (list :before (call-next-method)) *nnm-trace*))
(defmethod nnm-aux :after ((x integer))
  (push (list :after (call-next-method)) *nnm-trace*))
(defmethod nnm-aux :around ((x integer))
  (push :around *nnm-trace*)
  (prog1 (call-next-method) (push :around-out *nnm-trace*)))

(deftest clos-no-next-method.auxiliary-methods
  (let ((*nnm-trace* nil))
    (list (nnm-aux 7) (reverse *nnm-trace*)))
  (:primary (:around
             (:before (:no-next (:before) (7)))
             (:primary (:no-next nil (7)))
             (:after (:no-next (:after) (7)))
             :around-out)))

(defgeneric nnm-eql (x))
(defmethod nnm-eql ((x (eql :only))) (list :only (call-next-method)))
(defmethod no-next-method ((gf (eql #'nnm-eql)) (m standard-method) &rest args)
  (declare (ignore m))
  (list :no-next args))

(deftest clos-no-next-method.eql-specialized-method
  (nnm-eql :only)
  (:only (:no-next (:only))))

(defgeneric nnm-closure (x))
(defmethod nnm-closure ((x integer)) (funcall #'call-next-method))
(defmethod no-next-method ((gf (eql #'nnm-closure)) (m standard-method) &rest args)
  (declare (ignore m))
  (list :no-next args))

(deftest clos-no-next-method.escaped-closure
  (nnm-closure 5)
  (:no-next (5)))

;;; A method of an operator method combination also has no next method.
(defgeneric nnm-operator (x) (:method-combination +))
(defmethod nnm-operator + ((x integer))
  (list :method (call-next-method)))
(defmethod no-next-method ((gf (eql #'nnm-operator)) (m standard-method) &rest args)
  (declare (ignore m))
  (list :no-next args))

(deftest clos-no-next-method.operator-combination
  (nnm-operator 5)
  (:method (:no-next (5))))
