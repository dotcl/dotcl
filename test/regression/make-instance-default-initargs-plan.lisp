;;; MAKE-INSTANCE of a class with :DEFAULT-INITARGS goes through the same per-class
;;; plan as a class without them. Each test makes instances first, so the plan
;;; exists, then changes something it depends on and makes more.

(defvar *mdp-count* 0)
(defvar *mdp-value* :v1)
(defclass mdp-a ()
  ((x :initarg :x :initform :x0)
   (y :initarg :y :initform :y0)
   (z :initarg :z :initform :z0))
  (:default-initargs :x (incf *mdp-count*) :y *mdp-value*))
(defun %mdp-slots (o) (list (slot-value o 'x) (slot-value o 'y) (slot-value o 'z)))
(defun %mdp-a (&rest r) (declare (ignore r)) (make-instance 'mdp-a))
(defun %mdp-ax (v) (make-instance 'mdp-a :x v))
(defun %mdp-az (v) (make-instance 'mdp-a :z v))
(%mdp-a) (%mdp-ax 1) (%mdp-az 1) (%mdp-a) (%mdp-ax 1) (%mdp-az 1)

(deftest make-instance-default-initargs-plan.form-each-instance
  (progn (setq *mdp-count* 0 *mdp-value* :v1)
         (let ((a (%mdp-slots (%mdp-a))) (b (%mdp-slots (%mdp-a))))
           (setq *mdp-value* :v2)
           (list a b (%mdp-slots (%mdp-a)) *mdp-count*)))
  ((1 :v1 :z0) (2 :v1 :z0) (3 :v2 :z0) 3))

(deftest make-instance-default-initargs-plan.supplied-wins
  (progn (setq *mdp-count* 0 *mdp-value* :v1)
         (list (%mdp-slots (%mdp-ax :given)) *mdp-count*
               (%mdp-slots (%mdp-az :zz)) *mdp-count*
               (%mdp-slots (make-instance 'mdp-a :x :a :x :b :y :c))
               *mdp-count*))
  ((:given :v1 :z0) 0 (1 :v1 :zz) 1 (:a :c :z0) 1))

(deftest make-instance-default-initargs-plan.one-value
  (length (multiple-value-list (%mdp-a)))
  1)

(defun %mdp-bad () (make-instance 'mdp-a :nope 2))
(deftest make-instance-default-initargs-plan.invalid-initarg
  (list (handler-case (progn (%mdp-bad) :no-error) (error () :error))
        (handler-case (progn (%mdp-bad) :no-error) (error () :error))
        (slot-value (make-instance 'mdp-a :nope 2 :allow-other-keys t) 'z))
  (:error :error :z0))

;; Inherited defaults, and a subclass that overrides one.
(defclass mdp-base () ((p :initarg :p :initform nil) (q :initarg :q :initform nil))
  (:default-initargs :p :base-p :q :base-q))
(defclass mdp-sub (mdp-base) () (:default-initargs :q :sub-q))
(defclass mdp-sub2 (mdp-sub) ((r :initarg :r :initform :r0)))
(defun %mdp-pq (o) (list (slot-value o 'p) (slot-value o 'q)))
(dotimes (i 3) (make-instance 'mdp-base) (make-instance 'mdp-sub) (make-instance 'mdp-sub2))
(deftest make-instance-default-initargs-plan.inherited
  (list (%mdp-pq (make-instance 'mdp-base))
        (%mdp-pq (make-instance 'mdp-sub))
        (%mdp-pq (make-instance 'mdp-sub2))
        (%mdp-pq (make-instance 'mdp-sub2 :p 1)))
  ((:base-p :base-q) (:base-p :sub-q) (:base-p :sub-q) (1 :sub-q)))

;; :AFTER methods see the defaulted initargs; a default with no slot is passed too.
(defvar *mdp-seen* nil)
(defclass mdp-b () ((x :initarg :x :initform 0))
  (:default-initargs :x :dx :ghost :boo))
(defmethod initialize-instance :after ((o mdp-b) &key x ghost other)
  (push (list x ghost other) *mdp-seen*))
(dotimes (i 3) (make-instance 'mdp-b))
(deftest make-instance-default-initargs-plan.after-sees-defaults
  (progn (setq *mdp-seen* nil)
         (make-instance 'mdp-b)
         (make-instance 'mdp-b :x 1 :other :o)
         (make-instance 'mdp-b :ghost :g)
         (list (slot-value (make-instance 'mdp-b) 'x) *mdp-seen*))
  (:dx ((:dx :boo nil) (:dx :g nil) (1 :boo :o) (:dx :boo nil))))

;; A method added once the plan exists takes the full protocol.
(defclass mdp-c () ((x :initarg :x :initform 0)) (:default-initargs :x :dx))
(dotimes (i 3) (make-instance 'mdp-c))
(defvar *mdp-log* nil)
(defmethod initialize-instance :after ((o mdp-c) &key x)
  (push (list :after x) *mdp-log*))
(defmethod shared-initialize :before ((o mdp-c) slots &key x)
  (declare (ignore slots))
  (push (list :si x) *mdp-log*))
(deftest make-instance-default-initargs-plan.method-added
  (progn (setq *mdp-log* nil)
         (let ((o (make-instance 'mdp-c)))
           (list (slot-value o 'x) (reverse *mdp-log*))))
  (:dx ((:si :dx) (:after :dx))))

;; Redefinition: another default, and the default dropped.
(defclass mdp-d () ((x :initarg :x :initform :x0)) (:default-initargs :x 1))
(defun %mdp-d () (slot-value (make-instance 'mdp-d) 'x))
(%mdp-d) (%mdp-d) (%mdp-d)
(defclass mdp-d () ((x :initarg :x :initform :x0)) (:default-initargs :x 2))
(defvar *mdp-d-first* (list (%mdp-d) (%mdp-d)))
(defclass mdp-d () ((x :initarg :x :initform :x0)))
(deftest make-instance-default-initargs-plan.redefined
  (list *mdp-d-first* (%mdp-d) (%mdp-d))
  ((2 2) :x0 :x0))

;; A default added by redefinition to a class that had none.
(defclass mdp-e () ((x :initarg :x :initform :x0)))
(defun %mdp-e () (slot-value (make-instance 'mdp-e) 'x))
(%mdp-e) (%mdp-e) (%mdp-e)
(defclass mdp-e () ((x :initarg :x :initform :x0)) (:default-initargs :x :e1))
(deftest make-instance-default-initargs-plan.default-added
  (list (%mdp-e) (%mdp-e))
  (:e1 :e1))

;; A default form returning several values supplies the first.
(defun %mdp-mv () (values :first :second))
(defclass mdp-h () ((x :initarg :x)) (:default-initargs :x (%mdp-mv)))
(dotimes (i 3) (make-instance 'mdp-h))
(deftest make-instance-default-initargs-plan.multiple-values
  (slot-value (make-instance 'mdp-h) 'x)
  :first)

;; Redefining a superclass must rebuild its subclasses' inherited defaults and plans.
(defclass mdp-sa () ((x :initarg :x :initform :x0)))
(defclass mdp-sa-sub (mdp-sa) ())
(defun %mdp-sa () (slot-value (make-instance 'mdp-sa-sub) 'x))
(defun %mdp-sa-dyn () (slot-value (apply #'make-instance (list 'mdp-sa-sub)) 'x))
(defvar *mdp-sa-old* (make-instance 'mdp-sa-sub))
(%mdp-sa) (%mdp-sa) (%mdp-sa-dyn) (%mdp-sa-dyn)
(defclass mdp-sa () ((x :initarg :x :initform :x0)) (:default-initargs :x :sa1))
(deftest make-instance-default-initargs-plan.superclass-default-added
  (list (slot-value *mdp-sa-old* 'x) (%mdp-sa) (%mdp-sa-dyn)
        (mapcar #'first (dotcl-mop:class-default-initargs (find-class 'mdp-sa-sub))))
  (:x0 :sa1 :sa1 (:x)))

;; Changed, not just added.
(defclass mdp-sb () ((x :initarg :x :initform :x0)) (:default-initargs :x :sb1))
(defclass mdp-sb-sub (mdp-sb) ())
(defun %mdp-sb () (slot-value (make-instance 'mdp-sb-sub) 'x))
(defun %mdp-sb-dyn () (slot-value (funcall #'make-instance 'mdp-sb-sub) 'x))
(defvar *mdp-sb-old* (make-instance 'mdp-sb-sub))
(%mdp-sb) (%mdp-sb) (%mdp-sb-dyn) (%mdp-sb-dyn)
(defclass mdp-sb () ((x :initarg :x :initform :x0)) (:default-initargs :x :sb2))
(deftest make-instance-default-initargs-plan.superclass-default-changed
  (list (slot-value *mdp-sb-old* 'x) (%mdp-sb) (%mdp-sb-dyn)
        (mapcar #'first (dotcl-mop:class-default-initargs (find-class 'mdp-sb-sub))))
  (:sb1 :sb2 :sb2 (:x)))

;; Removed: the subclass falls back to the initform.
(defclass mdp-sc () ((x :initarg :x :initform :x0)) (:default-initargs :x :sc1))
(defclass mdp-sc-sub (mdp-sc) ())
(defun %mdp-sc () (slot-value (make-instance 'mdp-sc-sub) 'x))
(defvar *mdp-sc-old* (make-instance 'mdp-sc-sub))
(%mdp-sc) (%mdp-sc)
(defclass mdp-sc () ((x :initarg :x :initform :x0)))
(deftest make-instance-default-initargs-plan.superclass-default-removed
  (list (slot-value *mdp-sc-old* 'x) (%mdp-sc)
        (slot-value (make-instance 'mdp-sc-sub) 'x)
        (dotcl-mop:class-default-initargs (find-class 'mdp-sc-sub)))
  (:sc1 :x0 :x0 nil))

;; Two levels down, plus a subclass first made after the redefinition.
(defclass mdp-sd () ((x :initarg :x :initform :x0)) (:default-initargs :x :sd1))
(defclass mdp-sd-mid (mdp-sd) ())
(defclass mdp-sd-leaf (mdp-sd-mid) ())
(defclass mdp-sd-new (mdp-sd-mid) ())
(defun %mdp-sd () (slot-value (make-instance 'mdp-sd-leaf) 'x))
(defun %mdp-sd-dyn () (slot-value (funcall #'make-instance 'mdp-sd-leaf) 'x))
(defvar *mdp-sd-old* (make-instance 'mdp-sd-leaf))
(%mdp-sd) (%mdp-sd) (%mdp-sd-dyn) (%mdp-sd-dyn)
(defclass mdp-sd () ((x :initarg :x :initform :x0)) (:default-initargs :x :sd2))
(deftest make-instance-default-initargs-plan.superclass-default-two-levels
  (list (slot-value *mdp-sd-old* 'x) (%mdp-sd) (%mdp-sd-dyn)
        (slot-value (make-instance 'mdp-sd-new) 'x)
        (mapcar #'first (dotcl-mop:class-default-initargs (find-class 'mdp-sd-mid)))
        (mapcar #'first (dotcl-mop:class-default-initargs (find-class 'mdp-sd-leaf))))
  (:sd1 :sd2 :sd2 :sd2 (:x) (:x)))
