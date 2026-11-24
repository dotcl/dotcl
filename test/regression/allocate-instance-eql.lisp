;;; MAKE-INSTANCE allocates through ALLOCATE-INSTANCE when a method of it is
;;; specialized on (eql class).

(defvar *aie-log* nil)
(defclass aie-a () ((x :initarg :x :initform 1 :reader aie-x)))
(defclass aie-b () ())

(defmethod allocate-instance :around ((c (eql (find-class 'aie-a))) &rest r)
  (push :a *aie-log*)
  (call-next-method))

(deftest allocate-instance-eql.make-instance-calls-method
  (progn (setf *aie-log* nil)
         (let ((o (make-instance 'aie-a :x 5)))
           (list (aie-x o) *aie-log*)))
  (5 (:a)))

(deftest allocate-instance-eql.other-class-unaffected
  (progn (setf *aie-log* nil)
         (make-instance 'aie-b)
         *aie-log*)
  nil)

;; An eql-specialized method makes every ALLOCATE-INSTANCE dispatch costlier;
;; take it out so later allocation-counting tests are not affected.
(remove-method #'allocate-instance
               (find-method #'allocate-instance '(:around)
                            (list (intern-eql-specializer (find-class 'aie-a)))))
