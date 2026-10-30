;;; MAKE-INSTANCE of a structure class. A DEFSTRUCT slot takes the keyword of
;;; its name as initarg, as it does in SBCL, and slots not given one get their
;;; DEFSTRUCT default. jingoh builds its issue records this way.

(defstruct mis-point (x 10) y)
(defstruct (mis-point3 (:include mis-point)) (z (list :z)))

(deftest make-instance-struct-initargs
  (let ((p (make-instance 'mis-point :x 1 :y 2)))
    (list (mis-point-p p) (mis-point-x p) (mis-point-y p)))
  (t 1 2))

(deftest make-instance-struct-defaults
  (let ((p (make-instance 'mis-point :y 5)))
    (list (mis-point-x p) (mis-point-y p)))
  (10 5))

(deftest make-instance-struct-included-slots
  (let ((p (make-instance (find-class 'mis-point3) :x 1 :z 3)))
    (list (mis-point3-p p) (mis-point-x p) (mis-point-y p) (mis-point3-z p)))
  (t 1 nil 3))

(deftest make-instance-struct-equalp-constructor
  (equalp (make-instance 'mis-point :x 1 :y 2) (make-mis-point :x 1 :y 2))
  t)

(deftest make-instance-struct-unknown-initarg
  (handler-case (progn (make-instance 'mis-point :w 1) :no-error)
    (error () :error))
  :error)

(deftest make-instance-struct-allow-other-keys
  (mis-point-x (make-instance 'mis-point :w 1 :x 3 :allow-other-keys t))
  3)

(defstruct mis-defaults (items (list :fresh)) (n 1))

(deftest make-instance-struct-initforms-each-call
  (let ((a (make-instance 'mis-defaults))
        (b (make-instance 'mis-defaults)))
    (list (equal (mis-defaults-items a) '(:fresh))
          (eq (mis-defaults-items a) (mis-defaults-items b))))
  (t nil))

(defvar *mis-after-log* nil)
(defstruct mis-after (x 0))

(defmethod shared-initialize :after ((o mis-after) slots &key &allow-other-keys)
  (push :si *mis-after-log*))

(defmethod initialize-instance :after ((o mis-after) &key x &allow-other-keys)
  (push (list :ii x) *mis-after-log*)
  (setf (mis-after-x o) 42))

(deftest make-instance-struct-after-methods
  (let ((*mis-after-log* nil))
    (let ((o (make-instance 'mis-after :x 7)))
      (values (mis-after-x o) *mis-after-log*)))
  42 ((:ii 7) :si))

(defstruct mis-late (x 0))

(deftest make-instance-struct-no-late-method
  (let ((*mis-after-log* nil))
    (make-instance 'mis-late)
    *mis-after-log*)
  nil)

(defmethod initialize-instance :after ((o mis-late) &key &allow-other-keys)
  (push :late *mis-after-log*))

(deftest make-instance-struct-method-added-later
  (let ((*mis-after-log* nil))
    (make-instance 'mis-late)
    *mis-after-log*)
  (:late))

(deftest make-instance-struct-method-removed
  (let ((*mis-after-log* nil))
    (remove-method #'initialize-instance
                   (find-method #'initialize-instance '(:after)
                                (list (find-class 'mis-late))))
    (make-instance 'mis-late)
    *mis-after-log*)
  nil)

(defstruct mis-ii-primary (x 0))
(defmethod initialize-instance ((o mis-ii-primary) &key x &allow-other-keys)
  (push (list :primary x) *mis-after-log*)
  (call-next-method))

(deftest make-instance-struct-ii-primary
  (let ((*mis-after-log* nil))
    (let ((o (make-instance 'mis-ii-primary :x 7)))
      (list (mis-ii-primary-x o) *mis-after-log*)))
  (7 ((:primary 7))))

(defstruct mis-ii-before (x 0))
(defmethod initialize-instance :before ((o mis-ii-before) &key &allow-other-keys)
  (push :before *mis-after-log*))

(deftest make-instance-struct-ii-before
  (let ((*mis-after-log* nil))
    (let ((o (make-instance 'mis-ii-before :x 5)))
      (list (mis-ii-before-x o) *mis-after-log*)))
  (5 (:before)))

(defstruct mis-ii-around (x 0))
(defmethod initialize-instance :around ((o mis-ii-around) &key &allow-other-keys)
  (push :around *mis-after-log*)
  (call-next-method))

(deftest make-instance-struct-ii-around
  (let ((*mis-after-log* nil))
    (let ((o (make-instance 'mis-ii-around :x 6)))
      (list (mis-ii-around-x o) *mis-after-log*)))
  (6 (:around)))

(defstruct mis-si-all (x 0))
(defmethod shared-initialize :around ((o mis-si-all) slots &key &allow-other-keys)
  (push :si-around *mis-after-log*)
  (call-next-method))
(defmethod shared-initialize :before ((o mis-si-all) slots &key &allow-other-keys)
  (push :si-before *mis-after-log*))
(defmethod shared-initialize ((o mis-si-all) slots &key x &allow-other-keys)
  (push (list :si-primary x) *mis-after-log*)
  (call-next-method))
(defmethod shared-initialize :after ((o mis-si-all) slots &key &allow-other-keys)
  (push :si-after *mis-after-log*))

(deftest make-instance-struct-si-all-methods
  (let ((*mis-after-log* nil))
    (let ((o (make-instance 'mis-si-all :x 7)))
      (list (mis-si-all-x o) *mis-after-log*)))
  (7 (:si-after (:si-primary 7) :si-before :si-around)))

(defstruct mis-late-before (x 0))

(deftest make-instance-struct-before-method-absent
  (let ((*mis-after-log* nil))
    (make-instance 'mis-late-before)
    *mis-after-log*)
  nil)

(defmethod initialize-instance :before ((o mis-late-before) &key &allow-other-keys)
  (push :before *mis-after-log*))

(deftest make-instance-struct-before-method-added
  (let ((*mis-after-log* nil))
    (make-instance 'mis-late-before)
    *mis-after-log*)
  (:before))

(deftest make-instance-struct-before-method-removed
  (let ((*mis-after-log* nil))
    (remove-method #'initialize-instance
                   (find-method #'initialize-instance '(:before)
                                (list (find-class 'mis-late-before))))
    (make-instance 'mis-late-before)
    *mis-after-log*)
  nil)

(defstruct mis-late-around (x 0))

(deftest make-instance-struct-around-method-absent
  (let ((*mis-after-log* nil))
    (make-instance 'mis-late-around)
    *mis-after-log*)
  nil)

(defmethod initialize-instance :around ((o mis-late-around) &key &allow-other-keys)
  (push :around *mis-after-log*)
  (call-next-method))

(deftest make-instance-struct-around-method-added
  (let ((*mis-after-log* nil))
    (make-instance 'mis-late-around)
    *mis-after-log*)
  (:around))

(deftest make-instance-struct-around-method-removed
  (let ((*mis-after-log* nil))
    (remove-method #'initialize-instance
                   (find-method #'initialize-instance '(:around)
                                (list (find-class 'mis-late-around))))
    (make-instance 'mis-late-around)
    *mis-after-log*)
  nil)

(defstruct mis-late-primary (x 0))

(deftest make-instance-struct-primary-method-absent
  (let ((*mis-after-log* nil))
    (let ((o (make-instance 'mis-late-primary :x 3)))
      (list (mis-late-primary-x o) *mis-after-log*)))
  (3 nil))

(defmethod initialize-instance ((o mis-late-primary) &key x &allow-other-keys)
  (push (list :primary x) *mis-after-log*)
  (call-next-method))

(deftest make-instance-struct-primary-method-added
  (let ((*mis-after-log* nil))
    (let ((o (make-instance 'mis-late-primary :x 3)))
      (list (mis-late-primary-x o) *mis-after-log*)))
  (3 ((:primary 3))))

(deftest make-instance-struct-primary-method-removed
  (let ((*mis-after-log* nil))
    (remove-method #'initialize-instance
                   (find-method #'initialize-instance '()
                                (list (find-class 'mis-late-primary))))
    (let ((o (make-instance 'mis-late-primary :x 3)))
      (list (mis-late-primary-x o) *mis-after-log*)))
  (3 nil))

(defstruct mis-late-si (x 0))

(deftest make-instance-struct-si-method-absent
  (let ((*mis-after-log* nil))
    (make-instance 'mis-late-si)
    *mis-after-log*)
  nil)

(defmethod shared-initialize :before ((o mis-late-si) slots &key &allow-other-keys)
  (push :si *mis-after-log*))

(deftest make-instance-struct-si-method-added
  (let ((*mis-after-log* nil))
    (make-instance 'mis-late-si)
    *mis-after-log*)
  (:si))

(deftest make-instance-struct-si-method-removed
  (let ((*mis-after-log* nil))
    (remove-method #'shared-initialize
                   (find-method #'shared-initialize '(:before)
                                (list (find-class 'mis-late-si) (find-class 't))))
    (make-instance 'mis-late-si)
    *mis-after-log*)
  nil)

(defstruct mis-super (x 0))
(defstruct (mis-sub (:include mis-super)) (y 0))

(deftest make-instance-struct-super-method-absent
  (let ((*mis-after-log* nil))
    (make-instance 'mis-sub)
    *mis-after-log*)
  nil)

(defmethod initialize-instance :after ((o mis-super) &key &allow-other-keys)
  (push :super *mis-after-log*))

(deftest make-instance-struct-super-method-added
  (let ((*mis-after-log* nil))
    (make-instance 'mis-sub)
    *mis-after-log*)
  (:super))

(deftest make-instance-struct-super-method-removed
  (let ((*mis-after-log* nil))
    (remove-method #'initialize-instance
                   (find-method #'initialize-instance '(:after)
                                (list (find-class 'mis-super))))
    (make-instance 'mis-sub)
    *mis-after-log*)
  nil)

(defstruct mis-noinit x (y 2))
(defmethod initialize-instance :after ((o mis-noinit) &key &allow-other-keys)
  nil)

(deftest make-instance-struct-no-initform-bound
  (let ((o (make-instance 'mis-noinit)))
    (list (mis-noinit-x o) (mis-noinit-y o) (slot-boundp o 'x)))
  (nil 2 t))

(defstruct mis-slow-args (x 0))
(defmethod initialize-instance :after ((o mis-slow-args) &key &allow-other-keys)
  nil)

(deftest make-instance-struct-slow-unknown-initarg
  (handler-case (progn (make-instance 'mis-slow-args :nope 1) :no-error)
    (error () :error))
  :error)

(deftest make-instance-struct-slow-allow-other-keys
  (mis-slow-args-x (make-instance 'mis-slow-args :x 5 :nope 1 :allow-other-keys t))
  5)

(deftest make-instance-struct-slow-allow-other-keys-nil
  (handler-case (make-instance 'mis-slow-args :nope 1 :allow-other-keys nil)
    (error () :error))
  :error)

(defstruct mis-redef (a 1))

(deftest make-instance-struct-before-redefinition
  (mis-redef-a (make-instance 'mis-redef))
  1)

(defstruct mis-redef (a 2) (b 3))

(deftest make-instance-struct-redefinition-picked-up
  (list (mis-redef-a (make-instance 'mis-redef))
        (mis-redef-b (make-instance 'mis-redef)))
  (2 3))

(defstruct mis-redef-order (a 1) (b 2))

(deftest make-instance-struct-redefinition-order-before
  (let ((o (make-instance 'mis-redef-order)))
    (list (mis-redef-order-a o) (mis-redef-order-b o)))
  (1 2))

(defstruct mis-redef-order (b 20) (a 10))

(deftest make-instance-struct-redefinition-order-defaults
  (let ((o (make-instance 'mis-redef-order)))
    (list (mis-redef-order-a o) (mis-redef-order-b o)))
  (10 20))

(deftest make-instance-struct-redefinition-order-initargs
  (let ((o (make-instance 'mis-redef-order :a 5 :b 6)))
    (list (mis-redef-order-a o) (mis-redef-order-b o)))
  (5 6))
