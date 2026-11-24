;;; MAKE-INSTANCES-OBSOLETE makes the instances of the class and of every class that
;;; inherits from it obsolete (CLHS 4.3.6): UPDATE-INSTANCE-FOR-REDEFINED-CLASS runs for
;;; each of them, and slot values are kept. The expected values are SBCL's.

(defvar *mios-calls* nil)
(defclass mios-a () ((x :initform 1 :accessor mios-x)))
(defclass mios-b (mios-a) ((y :initform 2 :accessor mios-y)))
(defclass mios-c (mios-b) ((z :initform 3 :accessor mios-z)))
(defclass mios-d () ((w :initform 9)))
(defmethod update-instance-for-redefined-class :after ((i mios-a) added discarded plist &rest r)
  (declare (ignore r added discarded plist))
  (push (class-name (class-of i)) *mios-calls*))

(defun %mios-calls () (prog1 (sort (copy-list *mios-calls*) #'string<) (setq *mios-calls* nil)))

(deftest make-instances-obsolete-subclasses.superclass-reaches-subclass-instances
  (let ((ia (make-instance 'mios-a)) (ib (make-instance 'mios-b))
        (ic (make-instance 'mios-c)) (id (make-instance 'mios-d)))
    (setq *mios-calls* nil)
    (setf (mios-x ib) 10 (mios-y ic) 20)
    (make-instances-obsolete (find-class 'mios-a))
    (let ((vals (list (mios-x ia) (mios-x ib) (mios-y ib) (mios-x ic) (mios-y ic) (mios-z ic)
                      (slot-value id 'w))))
      (list vals (%mios-calls))))
  ((1 10 2 1 20 3 9) (mios-a mios-b mios-c)))

(deftest make-instances-obsolete-subclasses.middle-class-reaches-below-only
  (let ((ia (make-instance 'mios-a)) (ib (make-instance 'mios-b)) (ic (make-instance 'mios-c)))
    (setq *mios-calls* nil)
    (make-instances-obsolete (find-class 'mios-b))
    (let ((vals (list (mios-x ia) (mios-x ib) (mios-x ic) (mios-z ic))))
      (list vals (%mios-calls))))
  ((1 1 1 3) (mios-b mios-c)))

(deftest make-instances-obsolete-subclasses.by-name-leaf-class
  (let ((ic (make-instance 'mios-c)))
    (setq *mios-calls* nil)
    (make-instances-obsolete 'mios-c)
    (list (mios-z ic) (%mios-calls)))
  (3 (mios-c)))
