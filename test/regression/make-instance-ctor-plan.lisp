;;; MAKE-INSTANCE of a class with no initialization methods of its own keeps a
;;; per-class plan (which slot each initarg seen so far fills, and the
;;; initforms). Each test makes instances first, so the plan exists, then changes
;;; something the plan depends on and makes more. Expected values are SBCL 2.6.8's.

(defvar *mcp-count* 0)
(defclass mcp-a ()
  ((x :initarg :x :initform (incf *mcp-count*))
   (y :initarg :y :initarg :y2 :initform :y0)))
(defun %mcp-a (&rest r) (declare (ignore r)) (make-instance 'mcp-a))
(defun %mcp-ax (v) (make-instance 'mcp-a :x v))
(defun %mcp-axy (v w) (make-instance 'mcp-a :x v :y w :x 99))
(defun %mcp-slots (o) (list (slot-value o 'x) (slot-value o 'y)))

(deftest make-instance-ctor-plan.initform-each-time
  (progn (setq *mcp-count* 0)
         (list (%mcp-slots (%mcp-a)) (%mcp-slots (%mcp-a)) (%mcp-slots (%mcp-ax 5))
               *mcp-count*))
  ((1 :y0) (2 :y0) (5 :y0) 2))

(deftest make-instance-ctor-plan.first-initarg-wins
  (list (%mcp-slots (%mcp-axy 1 2)) (%mcp-slots (%mcp-axy 3 4))
        (%mcp-slots (make-instance 'mcp-a :x 0 :y2 7 :y 8)))
  ((1 2) (3 4) (0 7)))

(deftest make-instance-ctor-plan.one-value
  (progn (%mcp-ax 1) (%mcp-ax 1)
         (length (multiple-value-list (%mcp-ax 1))))
  1)

(defun %mcp-bad () (make-instance 'mcp-a :x 1 :nope 2))
(deftest make-instance-ctor-plan.invalid-initarg-after-plan
  (progn (%mcp-ax 1) (%mcp-ax 1)
         (list (handler-case (progn (%mcp-bad) :no-error) (error () :error))
               (handler-case (progn (%mcp-bad) :no-error) (error () :error))
               (slot-value (make-instance 'mcp-a :x 1 :nope 2 :allow-other-keys t) 'x)))
  (:error :error 1))

;; An :after method on INITIALIZE-INSTANCE added once the plan exists.
(defclass mcp-b () ((x :initarg :x :initform 0)))
(defun %mcp-b (v) (make-instance 'mcp-b :x v))
(%mcp-b 1) (%mcp-b 2)
(defvar *mcp-after* nil)
(defmethod initialize-instance :after ((o mcp-b) &key (extra :none))
  (push (list (slot-value o 'x) extra) *mcp-after*))
(deftest make-instance-ctor-plan.method-added
  (progn (setq *mcp-after* nil)
         (%mcp-b 3)
         (make-instance 'mcp-b :x 4 :extra :e)
         *mcp-after*)
  ((4 :e) (3 :none)))

;; Redefinition: another initform, an initarg dropped, a slot added in front.
(defclass mcp-c () ((x :initarg :x :initform 1)))
(defun %mcp-c () (make-instance 'mcp-c))
(defun %mcp-cx (v) (make-instance 'mcp-c :x v))
(%mcp-c) (%mcp-cx 2) (%mcp-c) (%mcp-cx 2)
(defclass mcp-c () ((z :initform :z) (x :initarg :x2 :initform 10)))
(deftest make-instance-ctor-plan.redefined
  (list (slot-value (%mcp-c) 'x) (slot-value (%mcp-c) 'z)
        (handler-case (progn (%mcp-cx 2) :no-error) (error () :error))
        (slot-value (make-instance 'mcp-c :x2 3) 'x))
  (10 :z :error 3))

;; :DEFAULT-INITARGS added by a redefinition.
(defclass mcp-d () ((x :initarg :x :initform 1)))
(defun %mcp-d () (make-instance 'mcp-d))
(%mcp-d) (%mcp-d)
(defclass mcp-d () ((x :initarg :x :initform 1)) (:default-initargs :x 7))
(deftest make-instance-ctor-plan.default-initargs-added
  (list (slot-value (%mcp-d) 'x) (slot-value (%mcp-d) 'x))
  (7 7))

;; The name made to denote another class.
(defclass mcp-e () ((x :initform :e)))
(defclass mcp-f () ((x :initform :f)))
(defun %mcp-e () (make-instance 'mcp-e))
(%mcp-e) (%mcp-e)
(deftest make-instance-ctor-plan.setf-find-class
  (let ((old (find-class 'mcp-e)))
    (setf (find-class 'mcp-e) (find-class 'mcp-f))
    (prog1 (list (slot-value (%mcp-e) 'x) (class-name (class-of (%mcp-e))))
      (setf (find-class 'mcp-e) old)))
  (:f mcp-f))

;; Literal initforms, inherited and overridden.
(defvar *mcp-g-count* 0)
(defclass mcp-g () ((a :initform 42) (b :initform :k) (c :initform 'sym) (d :initform #\x)))
(defclass mcp-h (mcp-g) ((a :initform (incf *mcp-g-count*)) (b :initform "s")))
(defun %mcp-g () (make-instance 'mcp-g))
(defun %mcp-h () (make-instance 'mcp-h))
(defun %mcp-abcd (o) (mapcar (lambda (s) (slot-value o s)) '(a b c d)))
(deftest make-instance-ctor-plan.literal-initforms
  (progn (setq *mcp-g-count* 0)
         (list (%mcp-abcd (%mcp-g)) (%mcp-abcd (%mcp-g))
               (%mcp-abcd (%mcp-h)) (%mcp-abcd (%mcp-h))))
  ((42 :k sym #\x) (42 :k sym #\x) (1 "s" sym #\x) (2 "s" sym #\x)))

;; :AFTER methods on INITIALIZE-INSTANCE, and what else gets added later.
(defvar *mcp-log* nil)
(defclass mcp-i () ((x :initarg :x :initform 0)))
(defclass mcp-j (mcp-i) ((y :initarg :y :initform 1)))
(defmethod initialize-instance :after ((o mcp-i) &rest r)
  (push (list :i (slot-value o 'x) r) *mcp-log*))
(defmethod initialize-instance :after ((o mcp-j) &key (tag :none) &allow-other-keys)
  (push (list :j (slot-value o 'y) tag) *mcp-log*))
(defun %mcp-j (&rest r) (setq *mcp-log* nil) (apply #'make-instance 'mcp-j r) (reverse *mcp-log*))
(defun %mcp-jq () (setq *mcp-log* nil) (make-instance 'mcp-j :x 5 :tag :t) (reverse *mcp-log*))
(deftest make-instance-ctor-plan.after-methods
  (list (%mcp-j) (%mcp-j :x 2) (%mcp-jq) (%mcp-jq)
        (length (multiple-value-list (make-instance 'mcp-j))))
  (((:i 0 nil) (:j 1 :none))
   ((:i 2 (:x 2)) (:j 1 :none))
   ((:i 5 (:x 5 :tag :t)) (:j 1 :t))
   ((:i 5 (:x 5 :tag :t)) (:j 1 :t))
   1))

(defmethod initialize-instance :before ((o mcp-j) &rest r)
  (declare (ignore r))
  (push (list :before (slot-boundp o 'x)) *mcp-log*))
(deftest make-instance-ctor-plan.before-added
  (list (%mcp-jq) (%mcp-jq))
  (((:before nil) (:i 5 (:x 5 :tag :t)) (:j 1 :t))
   ((:before nil) (:i 5 (:x 5 :tag :t)) (:j 1 :t))))

(remove-method #'initialize-instance
               (find-method #'initialize-instance '(:before) (list (find-class 'mcp-j))))
(remove-method #'initialize-instance
               (find-method #'initialize-instance '(:after) (list (find-class 'mcp-i))))
(deftest make-instance-ctor-plan.after-removed
  (list (%mcp-jq) (%mcp-jq))
  (((:j 1 :t)) ((:j 1 :t))))

(defmethod shared-initialize :after ((o mcp-j) slots &rest r)
  (declare (ignore slots r))
  (push :si *mcp-log*))
(deftest make-instance-ctor-plan.shared-initialize-added
  (list (%mcp-jq) (%mcp-jq))
  ((:si (:j 1 :t)) (:si (:j 1 :t))))
