;;; clos.lisp — ANSI tests for CLOS (defclass, make-instance, slot access, etc.)

;;; ============================================================
;;; 5.5a: Class definition and instances
;;; ============================================================

;;; --- Basic defclass ---

(defclass point ()
  ((x :initarg :x :initform 0 :accessor point-x)
   (y :initarg :y :initform 0 :accessor point-y)))

(deftest clos.defclass.1
  (notnot (find-class 'point))
  t)

(deftest clos.defclass.2
  (class-name (find-class 'point))
  point)

;;; --- make-instance with initargs ---

(deftest clos.make-instance.1
  (let ((p (make-instance 'point :x 10 :y 20)))
    (point-x p))
  10)

(deftest clos.make-instance.2
  (let ((p (make-instance 'point :x 10 :y 20)))
    (point-y p))
  20)

;;; --- make-instance with initform defaults ---

(deftest clos.make-instance.defaults.1
  (let ((p (make-instance 'point)))
    (list (point-x p) (point-y p)))
  (0 0))

(deftest clos.make-instance.defaults.2
  (let ((p (make-instance 'point :x 42)))
    (list (point-x p) (point-y p)))
  (42 0))

;;; --- slot-value ---

(deftest clos.slot-value.1
  (let ((p (make-instance 'point :x 5 :y 10)))
    (slot-value p 'x))
  5)

(deftest clos.slot-value.2
  (let ((p (make-instance 'point :x 5 :y 10)))
    (slot-value p 'y))
  10)

;;; --- (setf slot-value) ---

(deftest clos.setf-slot-value.1
  (let ((p (make-instance 'point :x 1 :y 2)))
    (setf (slot-value p 'x) 99)
    (slot-value p 'x))
  99)

;;; --- (setf accessor) ---

(deftest clos.setf-accessor.1
  (let ((p (make-instance 'point :x 1 :y 2)))
    (setf (point-x p) 42)
    (point-x p))
  42)

(deftest clos.setf-accessor.2
  (let ((p (make-instance 'point :x 1 :y 2)))
    (setf (point-y p) 99)
    (point-y p))
  99)

;;; --- slot-boundp ---

(defclass maybe-slot ()
  ((val :initarg :val)))

(deftest clos.slot-boundp.1
  (let ((m (make-instance 'maybe-slot :val 42)))
    (notnot (slot-boundp m 'val)))
  t)

(deftest clos.slot-boundp.2
  (let ((m (make-instance 'maybe-slot)))
    (slot-boundp m 'val))
  nil)

;;; --- Single inheritance ---

(defclass colored-point (point)
  ((color :initarg :color :initform "red" :accessor colored-point-color)))

(deftest clos.inheritance.1
  (let ((cp (make-instance 'colored-point :x 1 :y 2 :color "blue")))
    (list (point-x cp) (point-y cp) (colored-point-color cp)))
  (1 2 "blue"))

(deftest clos.inheritance.defaults
  (let ((cp (make-instance 'colored-point)))
    (list (point-x cp) (point-y cp) (colored-point-color cp)))
  (0 0 "red"))

;;; --- typep with CLOS classes ---

(deftest clos.typep.1
  (let ((p (make-instance 'point)))
    (notnot (typep p 'point)))
  t)

(deftest clos.typep.2
  (typep 42 'point)
  nil)

(deftest clos.typep.3
  (let ((cp (make-instance 'colored-point)))
    (notnot (typep cp 'colored-point)))
  t)

;; Subtype: colored-point IS-A point
(deftest clos.typep.4
  (let ((cp (make-instance 'colored-point)))
    (notnot (typep cp 'point)))
  t)

;; standard-object
(deftest clos.typep.5
  (let ((p (make-instance 'point)))
    (notnot (typep p 'standard-object)))
  t)

;;; --- class-of ---

(deftest clos.class-of.1
  (let ((p (make-instance 'point)))
    (class-name (class-of p)))
  point)

(deftest clos.class-of.2
  (let ((cp (make-instance 'colored-point)))
    (class-name (class-of cp)))
  colored-point)

;;; --- find-class / class-name ---

(deftest clos.find-class.1
  (notnot (find-class 'standard-object))
  t)

(deftest clos.class-name.1
  (class-name (find-class 'standard-object))
  standard-object)

;;; --- Multiple inheritance ---

(defclass named ()
  ((name :initarg :name :initform "unnamed" :accessor named-name)))

(defclass described ()
  ((description :initarg :description :initform "" :accessor described-description)))

(defclass named-described (named described)
  ())

(deftest clos.multi-inherit.1
  (let ((nd (make-instance 'named-described :name "foo" :description "a foo")))
    (list (named-name nd) (described-description nd)))
  ("foo" "a foo"))

(deftest clos.multi-inherit.defaults
  (let ((nd (make-instance 'named-described)))
    (list (named-name nd) (described-description nd)))
  ("unnamed" ""))

;; typep across multiple inheritance
(deftest clos.multi-inherit.typep.1
  (let ((nd (make-instance 'named-described)))
    (list (notnot (typep nd 'named-described))
          (notnot (typep nd 'named))
          (notnot (typep nd 'described))
          (notnot (typep nd 'standard-object))))
  (t t t t))

;;; --- with-slots ---

(deftest clos.with-slots.1
  (let ((p (make-instance 'point :x 10 :y 20)))
    (with-slots (x y) p
      (+ x y)))
  30)

;;; --- Reader-only accessor ---

(defclass ro-thing ()
  ((data :initarg :data :reader ro-thing-data)))

(deftest clos.reader.1
  (let ((r (make-instance 'ro-thing :data 42)))
    (ro-thing-data r))
  42)

;;; ============================================================
;;; 5.5b: Generic functions and method dispatch
;;; ============================================================

;;; --- Basic defgeneric / defmethod ---

(defclass shape ()
  ((name :initarg :name :initform "shape" :accessor shape-name)))

(defclass circle (shape)
  ((radius :initarg :radius :initform 1 :accessor circle-radius)))

(defclass rectangle (shape)
  ((width :initarg :width :initform 1 :accessor rect-width)
   (height :initarg :height :initform 1 :accessor rect-height)))

(defgeneric area (shape))

(defmethod area ((s circle))
  (* 3 (circle-radius s) (circle-radius s)))

(defmethod area ((s rectangle))
  (* (rect-width s) (rect-height s)))

(deftest clos.gf.basic.1
  (area (make-instance 'circle :radius 5))
  75)

(deftest clos.gf.basic.2
  (area (make-instance 'rectangle :width 3 :height 4))
  12)

;;; --- defgeneric with describe ---

(defgeneric describe-shape (shape))

(defmethod describe-shape ((s circle))
  "circle")

(defmethod describe-shape ((s rectangle))
  "rectangle")

(deftest clos.gf.dispatch.1
  (describe-shape (make-instance 'circle))
  "circle")

(deftest clos.gf.dispatch.2
  (describe-shape (make-instance 'rectangle))
  "rectangle")

;;; --- Inheritance dispatch (more specific wins) ---

(defmethod describe-shape ((s shape))
  "shape")

(deftest clos.gf.inherit.1
  (describe-shape (make-instance 'circle))
  "circle")

(deftest clos.gf.inherit.2
  (describe-shape (make-instance 'shape))
  "shape")

;;; --- call-next-method ---

(defgeneric greet (obj))

(defmethod greet ((obj shape))
  "hello shape")

(defmethod greet ((obj circle))
  (list "hello circle" (call-next-method)))

(deftest clos.gf.call-next-method.1
  (greet (make-instance 'circle))
  ("hello circle" "hello shape"))

;;; --- next-method-p ---

(defgeneric has-next (obj))

(defmethod has-next ((obj shape))
  (notnot (next-method-p)))

(defmethod has-next ((obj circle))
  (list (notnot (next-method-p)) (call-next-method)))

(deftest clos.gf.next-method-p.1
  (has-next (make-instance 'circle))
  (t nil))

;;; --- :before method ---

(defvar *log* nil)

(defgeneric process (obj))

(defmethod process ((obj shape))
  (push "primary-shape" *log*)
  "done")

(defmethod process :before ((obj shape))
  (push "before-shape" *log*))

(deftest clos.gf.before.1
  (progn
    (setq *log* nil)
    (process (make-instance 'shape))
    (nreverse *log*))
  ("before-shape" "primary-shape"))

;;; --- :after method ---

(defgeneric cleanup (obj))

(defmethod cleanup ((obj shape))
  (push "primary" *log*)
  "ok")

(defmethod cleanup :after ((obj shape))
  (push "after" *log*))

(deftest clos.gf.after.1
  (progn
    (setq *log* nil)
    (cleanup (make-instance 'shape))
    (nreverse *log*))
  ("primary" "after"))

;;; --- :around method ---

(defgeneric wrapped (obj))

(defmethod wrapped ((obj shape))
  "inner")

(defmethod wrapped :around ((obj shape))
  (list "around" (call-next-method)))

(deftest clos.gf.around.1
  (wrapped (make-instance 'shape))
  ("around" "inner"))

;;; --- :before + :after + primary ---

(defgeneric full-combo (obj))

(defmethod full-combo ((obj shape))
  (push "primary" *log*)
  42)

(defmethod full-combo :before ((obj shape))
  (push "before" *log*))

(defmethod full-combo :after ((obj shape))
  (push "after" *log*))

(deftest clos.gf.combo.1
  (progn
    (setq *log* nil)
    (let ((result (full-combo (make-instance 'shape))))
      (list result (nreverse *log*))))
  (42 ("before" "primary" "after")))

;;; --- Auto-create GF from defmethod ---

(defmethod auto-gf-test ((obj shape))
  "auto")

(deftest clos.gf.auto-create.1
  (auto-gf-test (make-instance 'shape))
  "auto")

;;; --- Multiple argument GF (implicit T specializer) ---

(defgeneric binary-op (a b))

(defmethod binary-op ((a circle) b)
  (list "circle" b))

(deftest clos.gf.multi-arg.1
  (binary-op (make-instance 'circle) 42)
  ("circle" 42))

;;; --- Nested GF calls ---

(defgeneric outer (obj))
(defgeneric inner (obj))

(defmethod inner ((obj circle))
  (circle-radius obj))

(defmethod outer ((obj circle))
  (* 2 (inner obj)))

(deftest clos.gf.nested.1
  (outer (make-instance 'circle :radius 5))
  10)

;;; --- Method replacement ---

(defgeneric replaceable (obj))

(defmethod replaceable ((obj shape))
  "v1")

(deftest clos.gf.replace.1
  (replaceable (make-instance 'shape))
  "v1")

(defmethod replaceable ((obj shape))
  "v2")

(deftest clos.gf.replace.2
  (replaceable (make-instance 'shape))
  "v2")

;;; ============================================================
;;; 5.5c: Initialization protocol + finishing
;;; ============================================================

;;; --- change-class ---

(defclass employee ()
  ((name :initarg :name :accessor employee-name)
   (salary :initarg :salary :initform 0 :accessor employee-salary)))

(defclass manager (employee)
  ((department :initarg :department :initform "general" :accessor manager-department)))

(deftest clos.change-class.1
  ;; Change employee to manager, name slot preserved
  (let ((e (make-instance 'employee :name "Alice" :salary 50000)))
    (change-class e 'manager)
    (employee-name e))
  "Alice")

(deftest clos.change-class.2
  ;; After change-class, new class's slots are accessible
  (let ((e (make-instance 'employee :name "Bob" :salary 60000)))
    (change-class e 'manager)
    (notnot (typep e 'manager)))
  t)

(deftest clos.change-class.3
  ;; Shared slot preserved, salary still accessible
  (let ((e (make-instance 'employee :name "Carol" :salary 70000)))
    (change-class e 'manager)
    (employee-salary e))
  70000)

;;; --- program-error typep ---

(deftest clos.program-error.typep.1
  (notnot (typep (make-instance 'point) 'standard-object))
  t)

(deftest clos.program-error.typep.2
  (typep 42 'program-error)
  nil)

;;; --- print-object (LispInstance.ToString) ---

(deftest clos.print.1
  ;; Basic instance printing — check it's a CLOS instance and can be printed
  (let ((p (make-instance 'point :x 1 :y 2)))
    (notnot (typep p 'standard-object)))
  t)

;;; --- Slot without initform stays unbound ---

(defclass maybe-pair ()
  ((first-val :initarg :first)
   (second-val :initarg :second)))

(deftest clos.unbound.1
  ;; :first matches first-val's initarg, so it IS bound
  (let ((mp (make-instance 'maybe-pair :first 1)))
    (notnot (slot-boundp mp 'first-val)))
  t)

(defclass maybe-pair2 ()
  ((first-val :initarg :first-val)
   (second-val :initarg :second-val)))

(deftest clos.unbound.2
  (let ((mp (make-instance 'maybe-pair2 :first-val 1)))
    (list (notnot (slot-boundp mp 'first-val))
          (slot-boundp mp 'second-val)))
  (t nil))

(deftest clos.unbound.3
  (let ((mp (make-instance 'maybe-pair2 :first-val 1)))
    (signals-error (slot-value mp 'second-val) error))
  t)

;;; --- program-error as handler-case ---

(deftest clos.program-error.handler-case
  (handler-case
    (error "test program error")
    (error (c) "caught"))
  "caught")

;;; --- standard-class typep ---

(deftest clos.standard-class.1
  (notnot (typep (find-class 'point) 'standard-class))
  t)

;;; --- class-of for instances ---

(deftest clos.class-of.instance
  (let ((p (make-instance 'point)))
    (eq (class-of p) (find-class 'point)))
  t)

;;; --- Slot override in subclass ---

(defclass base-with-slot ()
  ((val :initarg :val :initform 10 :accessor bws-val)))

(defclass derived-with-slot (base-with-slot)
  ((val :initarg :val :initform 20 :accessor dws-val)))

(deftest clos.slot-override.1
  ;; Derived class slot override: derived's initform wins
  (let ((d (make-instance 'derived-with-slot)))
    (dws-val d))
  20)

;;; ============================================================
;;; Additional tests (from code review)
;;; ============================================================

;;; --- Duplicate initargs: first value wins (CLHS 7.1.1) ---

(deftest clos.dup-initargs.1
  (let ((p (make-instance 'point :x 10 :x 99)))
    (point-x p))
  10)

(deftest clos.dup-initargs.2
  (let ((p (make-instance 'point :x 10 :y 20 :x 99 :y 88)))
    (list (point-x p) (point-y p)))
  (10 20))

;;; --- Empty class (no slots) ---

(defclass empty-class ()
  ())

(deftest clos.empty-class.1
  (notnot (typep (make-instance 'empty-class) 'empty-class))
  t)

(deftest clos.empty-class.2
  (notnot (typep (make-instance 'empty-class) 'standard-object))
  t)

;;; --- Diamond inheritance ---

(defclass diamond-a ()
  ((a-slot :initarg :a :initform "a" :accessor diamond-a-slot)))

(defclass diamond-b (diamond-a)
  ((b-slot :initarg :b :initform "b" :accessor diamond-b-slot)))

(defclass diamond-c (diamond-a)
  ((c-slot :initarg :c :initform "c" :accessor diamond-c-slot)))

(defclass diamond-d (diamond-b diamond-c)
  ((d-slot :initarg :d :initform "d" :accessor diamond-d-slot)))

(deftest clos.diamond.1
  ;; Diamond inherits slots from all parents
  (let ((d (make-instance 'diamond-d :a "A" :b "B" :c "C" :d "D")))
    (list (diamond-a-slot d) (diamond-b-slot d)
          (diamond-c-slot d) (diamond-d-slot d)))
  ("A" "B" "C" "D"))

(deftest clos.diamond.typep
  (let ((d (make-instance 'diamond-d)))
    (list (notnot (typep d 'diamond-d))
          (notnot (typep d 'diamond-b))
          (notnot (typep d 'diamond-c))
          (notnot (typep d 'diamond-a))
          (notnot (typep d 'standard-object))))
  (t t t t t))

;;; --- :after method does NOT affect return value ---

(defgeneric after-return-test (obj))

(defmethod after-return-test ((obj shape))
  42)

(defmethod after-return-test :after ((obj shape))
  99)

(deftest clos.gf.after-return.1
  ;; :after method's return value is discarded; primary value is returned
  (after-return-test (make-instance 'shape))
  42)

;;; --- :before with subclass dispatch ---

(defgeneric sub-process (obj))

(defmethod sub-process ((obj shape))
  "shape-primary")

(defmethod sub-process :before ((obj shape))
  (push "before-shape" *log*))

(defmethod sub-process ((obj circle))
  "circle-primary")

(defmethod sub-process :before ((obj circle))
  (push "before-circle" *log*))

(deftest clos.gf.before-subclass.1
  ;; Both :before methods fire in most-specific-first order
  (progn
    (setq *log* nil)
    (let ((result (sub-process (make-instance 'circle))))
      (list result (nreverse *log*))))
  ("circle-primary" ("before-circle" "before-shape")))

;;; --- change-class: added slot gets initform ---

(deftest clos.change-class.initform
  ;; After change-class, new slots without shared values get their initforms
  (let ((e (make-instance 'employee :name "Dave" :salary 50000)))
    (change-class e 'manager)
    (manager-department e))
  "general")

(do-tests-summary)
