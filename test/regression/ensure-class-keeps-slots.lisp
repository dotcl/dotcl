;;; ENSURE-CLASS on a class that already exists reinitializes it (AMOP): an initarg
;;; that is not given keeps its present value. (ensure-class 'c :direct-superclasses
;;; ...) used to drop every slot the class had; hu.dwim.perec does exactly that on
;;; a class it has just defined.

(defclass ecks-a () ((x :initarg :x :initform 1) (y :initform 2 :allocation :class)))
(defclass ecks-b () ((z :initform 3)))

(deftest ensure-class-keeps-slots.direct-slots-kept
  (progn
    (dotcl-mop:ensure-class 'ecks-a :direct-superclasses (list (find-class 'ecks-b)))
    (list (mapcar #'dotcl-mop:slot-definition-name (dotcl-mop:class-direct-slots (find-class 'ecks-a)))
          (mapcar #'class-name (dotcl-mop:class-direct-superclasses (find-class 'ecks-a)))
          (let ((i (make-instance 'ecks-a :x 7)))
            (list (slot-value i 'x) (slot-value i 'y) (slot-value i 'z)))))
  ((x y) (ecks-b) (7 2 3)))

(defclass ecks-c () ((p :initform 5)) (:default-initargs :p 9))

(deftest ensure-class-keeps-slots.explicit-direct-slots-still-replace
  (progn
    (dotcl-mop:ensure-class 'ecks-c :direct-slots '((:name q :initform 8)))
    (mapcar #'dotcl-mop:slot-definition-name (dotcl-mop:class-direct-slots (find-class 'ecks-c))))
  (q))
