;;; CLOS/MOP differences found by the deep differential run against SBCL.

(defgeneric c2f-egf-g (x y)
  (:method ((x t) (y symbol)) 1)
  (:method ((x symbol) (y t)) 2))

(deftest c2f-ensure-gf-argument-precedence-order
  (progn
    (ensure-generic-function 'c2f-egf-g
                             :argument-precedence-order '(y x))
    (list (c2f-egf-g 'a 'b)
          (dotcl-mop:generic-function-argument-precedence-order #'c2f-egf-g)))
  (1 (y x)))

(deftest c2f-ensure-gf-new-argument-precedence-order
  (handler-case
      (progn (ensure-generic-function 'c2f-egf-new
                                      :argument-precedence-order '(b a))
             :ok)
    (error (e) (type-of e)))
  :ok)

(defclass c2f-mlf-c ()
  ((a :initarg :a)
   (s :initarg :s :initform 1 :allocation :class)))

(deftest c2f-mlf-default-skips-class-slots
  (let ((o (make-instance 'c2f-mlf-c :a 1 :s 7)))
    (multiple-value-bind (create init) (make-load-form-saving-slots o)
      (declare (ignore init))
      (setf (slot-value o 's) 99)
      (let ((n (eval create)))
        (list (slot-value n 'a) (slot-value n 's) (slot-boundp n 's)))))
  (1 99 t))

(deftest c2f-mlf-explicit-class-slot
  (let ((o (make-instance 'c2f-mlf-c :a 1 :s 7)))
    (multiple-value-bind (create init)
        (make-load-form-saving-slots o :slot-names '(s))
      (declare (ignore init))
      (setf (slot-value o 's) 99)
      (slot-value (eval create) 's)))
  7)

(deftest c2f-restart-class-of
  (restart-case
      (let ((r (find-restart 'c2f-restart)))
        (list (class-name (class-of r))
              (typep r 'restart)
              (eq (class-of r) (find-class 'restart))
              (type-of r)))
    (c2f-restart () nil))
  (restart t t restart))

(defgeneric c2f-mc-g (x))
(defmethod c2f-mc-g ((x t)) x)
(defgeneric c2f-mc-h (x) (:method-combination +))

(deftest c2f-gf-method-combination-metaobject
  (let ((mc (dotcl-mop:generic-function-method-combination #'c2f-mc-g))
        (h (dotcl-mop:generic-function-method-combination #'c2f-mc-h)))
    (list (not (symbolp mc))
          (typep mc 'method-combination)
          (class-name (class-of mc))
          (dotcl-mop:method-combination-name mc)
          (dotcl-mop:method-combination-type-name h)
          (dotcl-mop:method-combination-options h)
          (not (null (dotcl-mop:compute-effective-method
                      #'c2f-mc-g mc
                      (dotcl-mop:generic-function-methods #'c2f-mc-g))))))
  (t t method-combination standard + nil t))
