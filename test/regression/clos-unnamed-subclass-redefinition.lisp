;;; A class taken out of the registry with (SETF (FIND-CLASS NAME) NIL) still has instances
;;; and subclasses; redefining its superclass updates their instances (CLHS 4.3.6).
;;; The expected values are SBCL's.

(defclass cusr-sup () ((z :initarg :z :initform :delta) (k :initform 5)))
(defclass cusr-sub (cusr-sup) ((w :initarg :w)))

(deftest clos-unnamed-subclass-redefinition.class-slot-and-new-slot
  (let ((i (make-instance 'cusr-sub :z 31 :w 30))
        (j (make-instance 'cusr-sup)))
    (setf (find-class 'cusr-sub) nil)
    (defclass cusr-sup ()
      ((z :initarg :z :initform :delta :allocation :class)
       (m :initform 7)))
    (list (slot-value i 'z) (slot-value i 'm) (slot-value i 'w)
          (slot-value j 'z) (slot-value j 'm)))
  (:delta 7 30 :delta 7))

(defclass cusr-p () ((a :initform 1)))
(defclass cusr-q (cusr-p) ((b :initform 2)))
(defclass cusr-r (cusr-q) ((c :initform 3)))

(deftest clos-unnamed-subclass-redefinition.two-levels-below
  (let ((i (make-instance 'cusr-r)))
    (setf (find-class 'cusr-q) nil)
    (defclass cusr-p () ((a :initform 1) (d :initform 4)))
    (list (slot-value i 'd) (slot-value i 'a) (slot-value i 'c)))
  (4 1 3))

(deftest clos-unnamed-subclass-redefinition.named-subclass-still-works
  (let ((i (make-instance 'cusr-p)))
    (defclass cusr-p () ((a :initform 1) (d :initform 4) (e :initform 9)))
    (list (slot-value i 'e) (slot-value i 'a)))
  (9 1))
