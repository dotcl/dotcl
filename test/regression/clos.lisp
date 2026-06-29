;;; CLOS regression tests

;;; Basic defclass / make-instance / slot-value
(defclass clos-point ()
  ((x :initarg :x :initform 0 :accessor point-x)
   (y :initarg :y :initform 0 :accessor point-y)))

(deftest clos-basic-make-instance
  (let ((p (make-instance 'clos-point :x 3 :y 4)))
    (list (point-x p) (point-y p)))
  (3 4))

;;; defmethod dispatch
(defgeneric clos-area (shape))
(defclass clos-square () ((side :initarg :side)))
(defmethod clos-area ((s clos-square))
  (let ((side (slot-value s 'side)))
    (* side side)))

(deftest clos-method-dispatch
  (clos-area (make-instance 'clos-square :side 5))
  25)

;;; Method combination — standard :around/:before/:after
(defvar *clos-log* nil)
(defgeneric clos-logged (x))
(defclass clos-base-obj ())
(defmethod clos-logged :before ((x clos-base-obj))
  (push :before *clos-log*))
(defmethod clos-logged ((x clos-base-obj))
  (push :primary *clos-log*)
  42)
(defmethod clos-logged :after ((x clos-base-obj))
  (push :after *clos-log*))

(deftest clos-before-after
  (let ((*clos-log* nil))
    (let ((result (clos-logged (make-instance 'clos-base-obj))))
      (list result (reverse *clos-log*))))
  (42 (:before :primary :after)))

;;; call-next-method
(defclass clos-child () ())
(defmethod clos-area ((s clos-child))
  (+ (call-next-method) 1))

;;; Initarg validation — invalid initarg should error
(deftest clos-invalid-initarg-error
  (signals-error (make-instance 'clos-point :z 99) error)
  t)

;;; defstruct
(defstruct clos-person name age)

(deftest clos-defstruct-basic
  (let ((p (make-clos-person :name "Alice" :age 30)))
    (list (clos-person-name p) (clos-person-age p)))
  ("Alice" 30))

;;; defstruct :read-only — accessor works
(defstruct clos-ro-struct
  (value 0 :read-only t))

(deftest clos-defstruct-read-only-access
  (clos-ro-struct-value (make-clos-ro-struct :value 7))
  7)

;;; typep and class-of
(deftest clos-typep
  (typep (make-instance 'clos-point) 'clos-point)
  t)

(deftest clos-class-of-name
  (class-name (class-of (make-instance 'clos-point)))
  clos-point)

;;; ValidateInitargs should apply to condition classes too
(define-condition clos-test-condition (error)
  ((msg :initarg :message :reader condition-msg)))

(deftest clos-condition-valid-initarg
  ;; :message is a valid slot initarg for clos-test-condition
  (let ((c (make-condition 'clos-test-condition :message "hello")))
    (condition-msg c))
  "hello")

(deftest clos-condition-invalid-initarg
  ;; :no-such-slot is NOT a valid initarg — should signal an error
  (signals-error (make-condition 'clos-test-condition :no-such-slot 42) error)
  t)

;;; reinitialize-instance validates initargs when no custom methods exist
;;; (Full &key-param collection from methods is deferred)
(defclass clos-ri-obj ()
  ((val :initarg :val :accessor ri-val :initform 0)))

(deftest clos-reinitialize-instance-invalid-initarg
  ;; :no-such-key is not a valid initarg — should signal an error (no custom methods on this class)
  (signals-error (reinitialize-instance (make-instance 'clos-ri-obj) :no-such-key 42) error)
  t)

;;; Cross-package typep: pa:widget and pb:widget are distinct classes
(defpackage :typep-test-pa (:use :cl) (:export :widget))
(defpackage :typep-test-pb (:use :cl) (:export :widget))
(defclass typep-test-pa:widget () ())
(defclass typep-test-pb:widget () ())

(deftest clos-typep-cross-package-no-false-positive
  ;; pb:widget instance must NOT satisfy pa:widget
  (typep (make-instance 'typep-test-pb:widget) 'typep-test-pa:widget)
  nil)

(deftest clos-typep-cross-package-positive
  ;; pa:widget instance must satisfy pa:widget
  (typep (make-instance 'typep-test-pa:widget) 'typep-test-pa:widget)
  t)

;;; next-method-p fast path — funcall #'next-method-p must agree with
;;; compiled (next-method-p) even when an :around method for the same GF was
;;; dispatched previously (which set nmpSym.Function to a stale closure).

(defgeneric nmp-gf (x))
(defclass nmp-base ())
(defclass nmp-derived (nmp-base) ())

;; Primary only for nmp-base (fast path: 1 primary, no before/after)
(defmethod nmp-gf ((x nmp-base))
  (list (next-method-p) (funcall #'next-method-p)))

;; :around + primary for nmp-derived — exercises InvokeWithNextMethods which
;; sets nmpSym.Function; after this dispatch nmpSym.Function must be restored
(defmethod nmp-gf :around ((x nmp-derived))
  (call-next-method))
(defmethod nmp-gf ((x nmp-derived))
  :derived-primary)

;; Compiled (next-method-p) and (funcall #'next-method-p) must both be NIL
;; when there is no next method (fast path).
(deftest nmp-fast-path-both-nil
  (nmp-gf (make-instance 'nmp-base))
  (nil nil))

;; After dispatching via :around (sets nmpSym.Function stale), the fast path
;; must reset nmpSym.Function so (funcall #'next-method-p) still returns NIL.
;; This was a regression in the fast path.
(deftest nmp-fast-path-nil-after-around
  (progn
    (nmp-gf (make-instance 'nmp-derived))   ; pollutes nmpSym.Function
    (nmp-gf (make-instance 'nmp-base)))     ; fast path must reset it
  (nil nil))

;; next-method-p must return T when there IS a next method (via call-next-method
;; inside :around calling the primary for nmp-base).
(defgeneric nmp-has-next (x))
(defclass nmp-hn-base ())
(defclass nmp-hn-sub (nmp-hn-base) ())

(defvar *nmp-has-next-result* nil)
(defmethod nmp-has-next ((x nmp-hn-base)) :base-primary)
(defmethod nmp-has-next :around ((x nmp-hn-sub))
  (setf *nmp-has-next-result* (list (next-method-p) (funcall #'next-method-p)))
  (call-next-method))

(deftest nmp-has-next-method-both-t
  (progn
    (nmp-has-next (make-instance 'nmp-hn-sub))
    *nmp-has-next-result*)
  (t t))

;;; CNM capture: (call-next-method) inside a continuation passed to a GF.
;;; Without the capture fix, the inner GF dispatch overwrites _nextMethodChain,
;;; so (call-next-method) inside the thunk errors with "no next method".

(defgeneric cnm-call-thunk (fn))
(defmethod cnm-call-thunk (fn) (funcall fn))

(defgeneric cnm-capture-gf (x))
(defclass cnm-capture-base ())
(defclass cnm-capture-derived (cnm-capture-base) ())

(defmethod cnm-capture-gf ((x cnm-capture-base)) :base)
(defmethod cnm-capture-gf :around ((x cnm-capture-derived))
  ;; (call-next-method) inside the lambda must call the base primary,
  ;; even though cnm-call-thunk dispatch happens in between.
  (cnm-call-thunk (lambda () (call-next-method))))

(deftest cnm-capture-via-thunk
  (cnm-capture-gf (make-instance 'cnm-capture-derived))
  :base)

;;; Same but using #'call-next-method as a first-class value.
(defgeneric cnm-capture-gf2 (x))
(defclass cnm-capture-base2 ())
(defclass cnm-capture-derived2 (cnm-capture-base2) ())

(defmethod cnm-capture-gf2 ((x cnm-capture-base2)) :base2)
(defmethod cnm-capture-gf2 :around ((x cnm-capture-derived2))
  (cnm-call-thunk #'call-next-method))

(deftest cnm-capture-funcref-thunk
  (cnm-capture-gf2 (make-instance 'cnm-capture-derived2))
  :base2)

;;; next-method-p with args must signal program-error
(defgeneric nmp-arity-gf (x))
(defmethod nmp-arity-gf ((x t)) nil)

(deftest nmp-error-with-args
  (handler-case
    (progn (eval '(defmethod nmp-arity-gf ((x t)) (next-method-p nil)))
           (nmp-arity-gf nil)
           :no-error)
    (program-error () :ok))
  :ok)

;;; call-next-method with args that change applicable method set must error
(defgeneric cnm-applicability-gf (x))
(defmethod cnm-applicability-gf ((x (eql 0))) (call-next-method 1))
(defmethod cnm-applicability-gf ((x t)) :base)

(deftest cnm-applicability-check
  (handler-case
    (cnm-applicability-gf 0)
    (error () :ok))
  :ok)

;;; method dispatch honours :argument-precedence-order (CLHS 7.6.6.1.2)
(defgeneric apo-foo (a b)
  (:argument-precedence-order b a))
(defmethod apo-foo ((a t) (b null)) :a-null-b)
(defmethod apo-foo ((a integer) (b t)) :b-integer-a)

;; b is weighed first; for (5 nil), b=nil's class NULL beats T -> method A
(deftest d1109-apo-respected
  (apo-foo 5 nil)
  :a-null-b)

;; natural-order GF (no APO) still weighs a first -> method B
(defgeneric apo-natural (a b))
(defmethod apo-natural ((a t) (b null)) :a-null-b)
(defmethod apo-natural ((a integer) (b t)) :b-integer-a)

(deftest d1109-natural-order-unchanged
  (apo-natural 5 nil)
  :b-integer-a)

;;; call-next-method with EXPLICIT arguments must pass THOSE arguments to the next
;;; method (CLHS 7.6.6.1), including when the next "method" is the before/primary/
;;; after combination reached past the end of the :around chain. Regression for
;;; the McCLIM editing-stream :around bug where the around prepended :peek-p nil but
;;; the primary still saw the original :peek-p t (the fallback captured the original
;;; generic-function args instead of the explicit ones).
(defclass cnm-xargs () ())
(defgeneric cnm-xargs-g (obj &key peek-p))
(defmethod cnm-xargs-g ((obj cnm-xargs) &key peek-p &allow-other-keys) peek-p)
(defmethod cnm-xargs-g :around ((obj cnm-xargs) &rest args &key peek-p &allow-other-keys)
  (declare (ignore peek-p))
  ;; prepend :peek-p nil to args, which already carries :peek-p t
  (apply #'call-next-method obj :peek-p nil args))

;; around supplies :peek-p nil; with the leftmost-wins keyword rule the primary
;; must see NIL even though the original call (and trailing args) had :peek-p t.
(deftest cnm-explicit-args-around-to-primary
  (cnm-xargs-g (make-instance 'cnm-xargs) :peek-p t)
  nil)

;; Same shape but with the direct (call-next-method obj :peek-p nil) form.
(defclass cnm-xargs2 () ())
(defgeneric cnm-xargs-g2 (obj &key peek-p))
(defmethod cnm-xargs-g2 ((obj cnm-xargs2) &key peek-p &allow-other-keys) peek-p)
(defmethod cnm-xargs-g2 :around ((obj cnm-xargs2) &key peek-p &allow-other-keys)
  (declare (ignore peek-p))
  (call-next-method obj :peek-p nil))

(deftest cnm-explicit-args-direct-form
  (cnm-xargs-g2 (make-instance 'cnm-xargs2) :peek-p t)
  nil)

;; Without explicit args, call-next-method forwards the ORIGINAL args unchanged.
(defclass cnm-noargs () ())
(defgeneric cnm-noargs-g (obj &key peek-p))
(defmethod cnm-noargs-g ((obj cnm-noargs) &key peek-p &allow-other-keys) peek-p)
(defmethod cnm-noargs-g :around ((obj cnm-noargs) &key peek-p &allow-other-keys)
  (declare (ignore peek-p))
  (call-next-method))

(deftest cnm-no-explicit-args-forwards-original
  (cnm-noargs-g (make-instance 'cnm-noargs) :peek-p t)
  t)

;; Explicit args propagate down a chain of primary methods too (more-specific to
;; less-specific via call-next-method).
(defclass cnm-base () ())
(defclass cnm-derived (cnm-base) ())
(defgeneric cnm-chain-g (obj &key k))
(defmethod cnm-chain-g ((obj cnm-base) &key k &allow-other-keys) k)
(defmethod cnm-chain-g ((obj cnm-derived) &key k &allow-other-keys)
  (declare (ignore k))
  (call-next-method obj :k :from-derived))

(deftest cnm-explicit-args-primary-chain
  (cnm-chain-g (make-instance 'cnm-derived) :k :original)
  :from-derived)

;; gray-streams-shaped multiple inheritance must compute a CPL (a valid CLOS
;; linearization exists; was a false "inconsistent precedence graph" on 0.1.8).
;; Structural analog of trivial-gray-streams stacked on the impl gray classes.
(defclass i335-g-fs () ())
(defclass i335-g-fis (i335-g-fs) ())
(defclass i335-g-fcs (i335-g-fs) ())
(defclass i335-g-fcis (i335-g-fis i335-g-fcs) ())
(defclass i335-t-fs (i335-g-fs) ())
(defclass i335-t-fis (i335-t-fs i335-g-fis) ())
(defclass i335-t-fcs (i335-t-fs i335-g-fcs) ())
(defclass i335-t-fcis (i335-t-fis i335-t-fcs i335-g-fcis) ())

(deftest i335-gray-streams-cpl
  (mapcar #'class-name
          (class-precedence-list (find-class 'i335-t-fcis)))
  (i335-t-fcis i335-t-fis i335-t-fcs i335-t-fs
   i335-g-fcis i335-g-fis i335-g-fcs i335-g-fs standard-object t))

;; foothold: an :allocation :class slot with an :initarg must be overridden
;; by a later make-instance's initarg, even when already bound by a prior instance
;; (CLHS: initargs always override existing slot values in shared-initialize).
;; Was: class-slot init guarded on "currently unbound", so the 2nd make-instance
;; kept the 1st value. ANSI CLASS-0214.2.
(defclass i333-ca () ((a :initarg :a1 :allocation :class)))
(defclass i333-cb (i333-ca) (b))

(deftest i333-class-alloc-initarg-override
  (progn
    (make-instance 'i333-ca :a1 'x)
    (slot-value (make-instance 'i333-cb :a1 'y) 'a))
  y)

;; class redefinition identity (CLHS ensure-class — redefine in place only
;; when the name is still the existing class's PROPER name). After clearing the
;; old class's proper name (setf class-name nil / find-class nil / rename), a
;; defclass under that name must produce a fresh, distinct class; an instance of
;; the old class must NOT be (typep) the new one (CPL by object identity, not
;; name). ANSI CLASS-0309.1 / 0310.1 / 0311.1.
(deftest i333-class-redef-clear-name
  (progn
    (setf (find-class 'i333-r9 nil) nil)
    (let* ((c1 (eval '(defclass i333-r9 () ((a)))))
           (o1 (make-instance 'i333-r9)))
      (setf (class-name c1) nil)
      (let ((c2 (eval '(defclass i333-r9 () ((a))))))
        (list (eq (class-of o1) c1) (eq c1 c2) (typep o1 c1) (typep o1 c2)))))
  (t nil t nil))

(deftest i333-class-redef-rename
  (progn
    (setf (find-class 'i333-r10a nil) nil (find-class 'i333-r10b nil) nil)
    (let* ((c1 (eval '(defclass i333-r10a () ((a)))))
           (o1 (make-instance 'i333-r10a)))
      (setf (class-name c1) 'i333-r10b)
      (let ((c2 (eval '(defclass i333-r10a () ((a))))))
        (list (eq c1 c2) (typep o1 c1) (typep o1 c2)
              (class-name c1) (class-name c2)))))
  (nil t nil i333-r10b i333-r10a))

;; a generic function with &key (no &allow-other-keys) must signal
;; program-error for an unknown keyword on EVERY call, including when the
;; monomorphic dispatch cache is warm. The cache-hit path used to skip keyword
;; validation, so the 2nd+ call with a bad keyword silently succeeded.
;; ANSI DEFMETHOD.ERROR.14/15.
(defgeneric i333-kw-gf (x &key))
(defmethod i333-kw-gf ((x t) &key) x)

(deftest i333-gf-keyword-validation-warm-cache
  (progn
    (i333-kw-gf 1)                       ; warm the dispatch cache with a valid call
    (list
     (handler-case (progn (i333-kw-gf 1 :bogus t) :no-error)
       (program-error () :prog-err))
     (handler-case (progn (i333-kw-gf 1 :another t) :no-error)
       (program-error () :prog-err))))
  (:prog-err :prog-err))

;; ensure-generic-function on an existing GF must apply :lambda-list and
;; :argument-precedence-order in place and invalidate the dispatch cache, so a
;; subsequent call dispatches by the new precedence order. ANSI
;; ENSURE-GENERIC-FUNCTION.8.
(deftest i333-ensure-gf-argument-precedence-order
  (let ((f 'i333-egf))
    (when (fboundp f) (fmakunbound f))
    (let ((fn (eval `(defgeneric ,f (x y)
                       (:method ((x t) (y symbol)) 1)
                       (:method ((x symbol) (y t)) 2)))))
      (let ((before (mapcar fn '(a a 3) '(b 4 b))))
        (ensure-generic-function f :lambda-list '(x y)
                                   :argument-precedence-order '(y x))
        (list before (mapcar fn '(a a 3) '(b 4 b))))))
  ((2 2 1) (1 2 1)))

;; a &key generic function must also reject an odd / non-symbol keyword
;; section and honor only the FIRST :allow-other-keys (CLHS 3.5.1.6 / 3.4.1.4.1).
;; ANSI DEFMETHOD.ERROR.14/15 values 2 and 4.
(defgeneric i333-kw-gf2 (x &key))
(defmethod i333-kw-gf2 ((x t) &key) x)

(deftest i333-gf-keyword-odd-and-aok-first
  (list
   (handler-case (progn (i333-kw-gf2 1 2) :no-error)            ; non-symbol / odd
     (program-error () :prog-err))
   (handler-case (progn (i333-kw-gf2 1 :allow-other-keys nil
                                       :allow-other-keys t :bogus t) :no-error)
     (program-error () :prog-err)))                              ; first aok nil → validate
  (:prog-err :prog-err))

;; slot-boundp on a missing slot must return a SINGLE generalized boolean
;; even when the slot-missing method returns (values nil X). The secondary value
;; leaked through into the caller's multiple values. ANSI SLOT-MISSING.8.
(defclass i333-sm-class () (a))
(defmethod slot-missing ((c t) (obj i333-sm-class)
                         (slot-name (eql 'i333-nope)) (op (eql 'slot-boundp))
                         &optional nv)
  (declare (ignore nv))
  (values nil :leaked-secondary))

(deftest i333-slot-boundp-single-value
  (multiple-value-list (slot-boundp (make-instance 'i333-sm-class) 'i333-nope))
  (nil))

;; change-class must NOT overwrite an :allocation :class slot of the target
;; class from the old instance — a shared slot keeps its existing class value
;; (CLHS 7.2.1). Here target slot a is class-allocated and was made unbound;
;; after change-class the (instance-allocated) old a=1 must not rebind it.
;; ANSI CHANGE-CLASS.3.2.
(defclass i333-cc-a () ((a :initarg :a) (b :initarg :b)))
(defclass i333-cc-b () ((a :allocation :class :initarg :a2)
                        (b :allocation :class :initarg :b2)))

(deftest i333-change-class-class-allocation
  (let* ((obj (make-instance 'i333-cc-a :a 1))
         (new (find-class 'i333-cc-b))
         (obj2 (make-instance new)))
    (slot-makunbound obj2 'a)
    (setf (slot-value obj2 'b) 17)
    (change-class obj new)
    (list (slot-boundp obj 'a) (slot-boundp obj 'b) (slot-value obj 'b)))
  (nil t 17))

;; a short-form (operator) method combination must signal an error when an
;; applicable method has an invalid qualifier (not the combination name or
;; :around), rather than silently ignoring it (CLHS 7.6.6.2).
;; ANSI DEFGENERIC-METHOD-COMBINATION.APPEND.13.
(defclass i333-mc-a () ())
(defclass i333-mc-b (i333-mc-a) ())

(deftest i333-operator-combination-invalid-qualifier
  (progn
    (eval '(defgeneric i333-mcg (x)
             (:method-combination append)
             (:method append ((x i333-mc-a)) (list 'ok))
             (:method nonsense ((x i333-mc-b)) (list 'bad))))
    (list
     (i333-mcg (make-instance 'i333-mc-a))
     (handler-case (i333-mcg (make-instance 'i333-mc-b)) (error () :caught))))
  ((ok) :caught))

;; CLHS 4.3.5 CLOS class precedence list is non-monotonic. In this
;; hierarchy class-c must precede class-b in h's CPL even though it follows it
;; in the direct supers' CPLs, so h's slot A inherits c's initform 'y, not b's
;; 'x. Mirrors ANSI CLASS-0306.1/2. (C3 linearization would give 'x.)
(defclass i352-a () ((a :initform nil :reader i352-a-slot)))
(defclass i352-b (i352-a) ((a :initform 'x)))
(defclass i352-c (i352-a) ((a :initform 'y)))
(defclass i352-d (i352-b) ())
(defclass i352-e (i352-b) ())
(defclass i352-f (i352-d i352-c) ())
(defclass i352-g (i352-e) ())
(defclass i352-h (i352-f i352-g) ())

(deftest i352-nonmonotonic-cpl-slot
  (loop for cls in '(i352-a i352-b i352-c i352-d i352-e i352-f i352-g i352-h)
        collect (slot-value (make-instance cls) 'a))
  (nil x y x x x x y))

(deftest i352-nonmonotonic-cpl-order
  (mapcar #'class-name
          (dotcl::class-precedence-list (find-class 'i352-h)))
  (i352-h i352-f i352-d i352-c i352-g i352-e i352-b i352-a standard-object t))

;;; Cross-package same-named classes must stay distinct even when one is
;;; forward-referenced by a subclass (FindOrForwardClass used to key the
;;; placeholder by bare name → DOTCL-INTERNAL::SEQ, so a later same-named class
;;; in another package shadowed the earlier one; cl-ppcre::seq vs fset::seq).
(defpackage :i408-pa (:use :cl))
(defpackage :i408-pb (:use :cl))
;; child references the (not-yet-defined) parent → parent is forward-referenced
(defclass i408-pa::child (i408-pa::node) ())
(defclass i408-pa::node () ((aa :initarg :aa :accessor i408-pa::aa)))
(defclass i408-pb::child (i408-pb::node) ())
(defclass i408-pb::node () ((bb :initarg :bb :accessor i408-pb::bb)))

(deftest i408-cross-package-classes-distinct
  (eq (find-class 'i408-pa::node) (find-class 'i408-pb::node))
  nil)

(deftest i408-find-class-returns-queried-package
  (list (eq (find-class 'i408-pa::node) (find-class 'i408-pa::node))
        (eq (find-class 'i408-pb::node) (find-class 'i408-pb::node)))
  (t t))

;; each parent keeps its OWN slots; make-instance with the other package's slot
;; was the reported failure ("Invalid initarg :ELEMENTS for class SEQ")
(deftest i408-make-instance-pa-own-slot
  (slot-value (make-instance 'i408-pa::node :aa 1) 'i408-pa::aa)
  1)

(deftest i408-make-instance-pb-own-slot
  (slot-value (make-instance 'i408-pb::node :bb 2) 'i408-pb::bb)
  2)

;; the forward-referenced subclass resolves to its OWN package's parent
(deftest i408-subclass-super-is-own-package
  (list (class-name (first (dotcl-mop:class-direct-superclasses (find-class 'i408-pa::child))))
        (class-name (first (dotcl-mop:class-direct-superclasses (find-class 'i408-pb::child)))))
  (i408-pa::node i408-pb::node))

;;; (setf find-class) must key by the ORIGINAL package-qualified symbol, not the
;;; bare-name-normalized one. fset's post.lisp aliases each class into a FSET2
;;; package via (setf (find-class fset2-sym) (find-class sym)); the normalized key
;;; collapsed e.g. FSET2::SEQ onto DOTCL-INTERNAL::SEQ and shadowed cl-ppcre::seq.
(defpackage :i408b-s1 (:use :cl))
(defpackage :i408b-s2 (:use :cl))
(defclass i408b-base1 () ((x :initarg :x)))
(defclass i408b-base2 () ((y :initarg :y)))
(setf (find-class (intern "WIDGET" :i408b-s1)) (find-class 'i408b-base1))
(setf (find-class (intern "WIDGET" :i408b-s2)) (find-class 'i408b-base2))

(deftest i408-setf-find-class-distinct-packages
  (list (class-name (find-class (intern "WIDGET" :i408b-s1)))
        (class-name (find-class (intern "WIDGET" :i408b-s2)))
        (eq (find-class (intern "WIDGET" :i408b-s1))
            (find-class (intern "WIDGET" :i408b-s2))))
  (i408b-base1 i408b-base2 nil))

;; the alias resolves to the aliased class (fset2 use-case), and make-instance with
;; the aliased class's own initarg works
(deftest i408-setf-find-class-alias-resolves
  (slot-value (make-instance (find-class (intern "WIDGET" :i408b-s1)) :x 7) 'x)
  7)
