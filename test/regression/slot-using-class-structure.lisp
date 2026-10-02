;;; SLOT-VALUE-USING-CLASS, its SETF and SLOT-BOUNDP-USING-CLASS work on a
;;; structure instance with the slot definitions of its class, as in SBCL.
;;; They signalled "not a CLOS instance". hu.dwim.serializer walks the slots of
;;; any object this way, structures included.

(defstruct sucs-point (x 1) y)

(defun %sucs-slot (name)
  (find name (dotcl-mop:class-slots (find-class 'sucs-point))
        :key #'dotcl-mop:slot-definition-name))

(deftest slot-using-class-structure.read
  (let ((p (make-sucs-point :y 2)))
    (list (dotcl-mop:slot-value-using-class (class-of p) p (%sucs-slot 'x))
          (dotcl-mop:slot-value-using-class (class-of p) p (%sucs-slot 'y))))
  (1 2))

(deftest slot-using-class-structure.write
  (let ((p (make-sucs-point)))
    (setf (dotcl-mop:slot-value-using-class (class-of p) p (%sucs-slot 'x)) 9)
    (sucs-point-x p))
  9)

(deftest slot-using-class-structure.boundp
  (let ((p (make-sucs-point)))
    (and (dotcl-mop:slot-boundp-using-class (class-of p) p (%sucs-slot 'x)) t))
  t)
