;;; A metaclass's :DEFAULT-INITARGS apply when a class is made with it, as they
;;; do for any MAKE-INSTANCE (CLHS 7.1.3), whether the class comes from DEFCLASS
;;; :METACLASS or from MAKE-INSTANCE on the metaclass. Both routes used to skip
;;; them: the slot kept its initform and INITIALIZE-INSTANCE saw NIL for the key.
;;; serapeum's TOPMOST-OBJECT-CLASS gets its :TOPMOST-CLASS this way.
;;; MAKE-INSTANCE on the metaclass also dropped initargs other than :NAME,
;;; :DIRECT-SUPERCLASSES and :DIRECT-SLOTS.

(defclass mdi-meta (standard-class)
  ((tc :initarg :tc :initform :none :reader mdi-tc)))
(defmethod dotcl-mop:validate-superclass ((a mdi-meta) (b standard-class)) t)
(defclass mdi-meta2 (mdi-meta) () (:default-initargs :tc 'foo))

(defvar *mdi-seen* nil)
(defmethod initialize-instance :around ((c mdi-meta) &rest initargs &key tc)
  (declare (ignore initargs))
  (push tc *mdi-seen*)
  (call-next-method))

(deftest metaclass-default-initargs.defclass
  (progn
    (setf *mdi-seen* nil)
    (defclass mdi-c1 () () (:metaclass mdi-meta2))
    (list (mdi-tc (find-class 'mdi-c1)) *mdi-seen*))
  (foo (foo)))

(deftest metaclass-default-initargs.defclass-option-wins
  (progn
    (defclass mdi-c2 () () (:metaclass mdi-meta2) (:tc baz))
    (mdi-tc (find-class 'mdi-c2)))
  (baz))

(deftest metaclass-default-initargs.make-instance
  (progn
    (setf *mdi-seen* nil)
    (list (mdi-tc (make-instance 'mdi-meta2 :name 'mdi-c3))
          (mdi-tc (make-instance 'mdi-meta2 :name 'mdi-c4 :tc 'bar))
          (reverse *mdi-seen*)))
  (foo bar (foo bar)))
