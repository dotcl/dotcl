;;; Structure classes seen through the MOP, as SBCL shows them: STRUCTURE-OBJECT is itself a
;;; STRUCTURE-CLASS, class-direct-slots lists only a class's own slots, slot-definition-initform
;;; answers the slot's default form, allocate-instance refuses a built-in class, and a simple
;;; vector's class is SIMPLE-VECTOR.

(defstruct smi-base a b)
(defstruct (smi-mid (:include smi-base)) c)
(defstruct (smi-leaf (:include smi-mid)) d)
(defstruct smi-init (a 5) (b (list 1 2)) c)

(defun %smi-names (slots) (mapcar #'dotcl-mop:slot-definition-name slots))

(deftest struct-mop-introspection.structure-object-metaclass
  (list (class-name (class-of (find-class 'structure-object)))
        (class-name (class-of (find-class 'smi-leaf)))
        (class-name (class-of (find-class 'standard-object))))
  (structure-class structure-class standard-class))

(deftest struct-mop-introspection.direct-and-effective-slots
  (list (%smi-names (dotcl-mop:class-direct-slots (find-class 'smi-base)))
        (%smi-names (dotcl-mop:class-direct-slots (find-class 'smi-mid)))
        (%smi-names (dotcl-mop:class-direct-slots (find-class 'smi-leaf)))
        (%smi-names (dotcl-mop:class-slots (find-class 'smi-leaf)))
        (let ((x (make-smi-leaf :a 1 :b 2 :c 3 :d 4)))
          (list (smi-base-a x) (smi-mid-c x) (smi-leaf-d x)
                (slot-value x 'a) (slot-value x 'd))))
  ((a b) (c) (d) (a b c d) (1 3 4 1 4)))

(deftest struct-mop-introspection.initform
  (list (mapcar #'dotcl-mop:slot-definition-initform (dotcl-mop:class-slots (find-class 'smi-init)))
        (mapcar #'dotcl-mop:slot-definition-initform (dotcl-mop:class-direct-slots (find-class 'smi-init)))
        (let ((x (make-smi-init))) (list (smi-init-a x) (smi-init-b x) (smi-init-c x))))
  ((5 (list 1 2) nil) (5 (list 1 2) nil) (5 (1 2) nil)))

(deftest struct-mop-introspection.allocate-instance-built-in
  (list (handler-case (progn (allocate-instance (find-class 'integer)) :allocated)
          (error () :error))
        (typep (allocate-instance (find-class 'smi-base)) 'smi-base)
        (typep (allocate-instance (find-class 'standard-object)) 'standard-object))
  (:error t t))

(deftest struct-mop-introspection.simple-vector-class
  (list (class-name (class-of (vector 1 2)))
        (subtypep (class-of (vector 1 2)) (find-class 'simple-vector))
        (subtypep (class-of (vector 1 2)) (find-class 'vector))
        (class-name (class-of (make-array 3 :adjustable t)))
        (class-name (class-of (make-array 3 :element-type 'bit))))
  (simple-vector t t vector simple-bit-vector))
