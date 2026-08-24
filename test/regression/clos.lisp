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
          (dotcl-mop:class-precedence-list (find-class 'i352-h)))
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

;;; symbol-macrolet must shadow an enclosing lexical variable of the same name
;;; (CLHS 5.1.2.1: the inner binding wins). Previously compile-var-ref found the
;;; outer local first and never reached the symbol-macro on read.
(deftest symbol-macrolet-shadows-outer-lexical
  (let ((x 1)) (symbol-macrolet ((x 99)) x))
  99)

(deftest symbol-macrolet-shadows-outer-lexical-expansion
  (let ((g 42) (x 'ignored)) (symbol-macrolet ((x (+ g 0))) x))
  42)

;;; with-accessors where an accessor variable name collides with the
;;; instance-form variable must still expand to the accessor call, not resolve
;;; to the instance. This is exactly flexi-streams' close method shape
;;; ((stream flexi-stream-stream)) stream (close stream ...)), which otherwise
;;; re-invoked itself on the instance and ran the control stack out.
(defclass %wa-w () ((inner :initarg :inner :accessor %wa-inner)))
(deftest with-accessors-name-collides-with-instance-var
  (funcall (lambda (x) (with-accessors ((x %wa-inner)) x x))
           (make-instance '%wa-w :inner 42))
  42)

;;; ---- EQL-specialized GF dispatch cache ----
;;; Single-required-arg GFs whose EQL methods are all unqualified are cached
;;; class-keyed; the hit path re-checks the EQL methods against the actual
;;; value each call. These pin the reconstruction: EQL priority over class
;;; primaries, call-next-method ordering, before-method participation,
;;; no-applicable errors, and cache invalidation on defmethod.

(defgeneric %eqlc-fib (x))
(defmethod %eqlc-fib ((x (eql 0))) 10)
(defmethod %eqlc-fib ((x (eql 1))) 11)
(defmethod %eqlc-fib (x) (list :default x))

;; Repeat so later iterations run on a warm cache, both EQL-hit and default.
(deftest eql-cache-hot-loop
  (let ((acc '()))
    (dotimes (i 3)
      (push (%eqlc-fib 0) acc)
      (push (%eqlc-fib 1) acc)
      (push (%eqlc-fib 5) acc))
    (nreverse acc))
  (10 11 (:default 5) 10 11 (:default 5) 10 11 (:default 5)))

;; call-next-method from an EQL method reaches the class default (7.6.6 order),
;; on a warm cache too.
(defgeneric %eqlc-cnm (x))
(defmethod %eqlc-cnm ((x (eql 1))) (cons :eql (call-next-method)))
(defmethod %eqlc-cnm (x) (list :next x))
(deftest eql-cache-call-next-method
  (list (%eqlc-cnm 1) (%eqlc-cnm 1) (%eqlc-cnm 2))
  ((:eql :next 1) (:eql :next 1) (:next 2)))

;; EQL-only GF: next-method-p inside the body must be NIL (direct-invoke fast
;; path publishes "no next method" to the captured closures).
(defgeneric %eqlc-only (x))
(defmethod %eqlc-only ((x (eql :a))) (next-method-p))
(deftest eql-cache-only-nmp
  (list (%eqlc-only :a) (%eqlc-only :a))
  (nil nil))

;; EQL-only GF called with a non-matching value of the SAME class after the
;; cache is warm → no-applicable-method error (hit-path guard).
(deftest eql-cache-no-applicable
  (progn
    (%eqlc-only :a)
    (handler-case (progn (%eqlc-only :b) :no-error)
      (error () :err)))
  :err)

;; Class :before methods still run when an EQL primary matches.
(defclass %eqlc-rec () ())
(defgeneric %eqlc-ba (x))
(defvar *%eqlc-log* '())
(defmethod %eqlc-ba :before ((x symbol)) (push :before *%eqlc-log*))
(defmethod %eqlc-ba ((x (eql :k))) (push :eql *%eqlc-log*))
(defmethod %eqlc-ba ((x symbol)) (push :sym *%eqlc-log*))
(deftest eql-cache-before-runs
  (progn
    (setq *%eqlc-log* '())
    (%eqlc-ba :k) (%eqlc-ba :k) (%eqlc-ba :other)
    (nreverse *%eqlc-log*))
  (:before :eql :before :eql :before :sym))

;; defmethod after the cache is warm must invalidate it.
(defgeneric %eqlc-inval (x))
(defmethod %eqlc-inval (x) :old)
(deftest eql-cache-invalidation
  (list (%eqlc-inval 7)
        (progn (eval '(defmethod %eqlc-inval ((x (eql 7))) :new))
               (%eqlc-inval 7))
        (%eqlc-inval 8))
  (:old :new :old))

;; Multi-required-arg EQL GFs stay uncached — CLHS ordering where a class
;; method out-ranks an EQL method via the leftmost position must hold.
(defgeneric %eqlc-two (x y))
(defmethod %eqlc-two ((x integer) y) (list :int-first (when (next-method-p) (call-next-method))))
(defmethod %eqlc-two ((x number) (y (eql 3))) (list :eql-y))
(deftest eql-cache-multiarg-ordering
  (list (%eqlc-two 5 3) (%eqlc-two 5 3))
  ((:int-first (:eql-y)) (:int-first (:eql-y))))

;;; Class-object specializer (a class metaobject, not a class-name symbol):
;;; (defmethod m ((x #.(find-class 'cons))) ...). SBCL/CCL accept a class object
;;; as a specializer and real libraries (serapeum threads.lisp) rely on it; dotcl
;;; must register and dispatch on it rather than passing it to find-class.
(defgeneric %clos-cospec (x))
(defmethod %clos-cospec ((x #.(find-class 'cons))) :cons-class-object)
(defmethod %clos-cospec ((x t)) :fallback)
(deftest clos-class-object-specializer
  (list (%clos-cospec (cons 1 2)) (%clos-cospec 99))
  (:cons-class-object :fallback))

;;; dotnet:class-for-type — public lookup of the CLOS class dotcl registers for a
;;; .NET type, so user code never hand-spells a specializer symbol (dotcl/dotcl#50).
;;; Returns a class object usable directly as a #. specializer.
(deftest class-for-type-is-class
  (typep (dotnet:class-for-type "System.Text.StringBuilder") 'class)
  t)

;;; Same class object as class-of an instance, and idempotent across calls / input forms.
(deftest class-for-type-eq-class-of
  (let ((c (dotnet:class-for-type "System.Text.StringBuilder")))
    (list (eq c (class-of (dotnet:new "System.Text.StringBuilder")))
          (eq c (dotnet:class-for-type "System.Text.StringBuilder"))
          (eq c (dotnet:class-for-type (dotnet:resolve-type "System.Text.StringBuilder")))))
  (t t t))

;;; Closed generic — the motivating case: its auto-derived name is an ugly
;;; assembly-qualified string, but class-for-type hands back the class directly.
(deftest class-for-type-closed-generic
  (typep (dotnet:class-for-type
           (dotnet:make-generic-type "System.Collections.Generic.List" '("System.Int32")))
         'class)
  t)

;;; The class object dispatches as a #. specializer.
(defgeneric %cft-tag (x))
(defmethod %cft-tag ((x #.(dotnet:class-for-type "System.Text.StringBuilder"))) :sb)
(defmethod %cft-tag ((x t)) :other)
(deftest class-for-type-specializer
  (list (%cft-tag (dotnet:new "System.Text.StringBuilder")) (%cft-tag 42))
  (:sb :other))

;;; Closed generics get distinct, readable class names (List<Int32> vs List<String>)
;;; instead of colliding on List`1 with a load-order-dependent winner (dotcl/dotcl#50).
(deftest class-for-type-closed-generic-distinct-names
  (let ((ci (dotnet:class-for-type
              (dotnet:make-generic-type "System.Collections.Generic.List" '("System.Int32"))))
        (cs (dotnet:class-for-type
              (dotnet:make-generic-type "System.Collections.Generic.List" '("System.String")))))
    (list (not (eq ci cs))
          (string= (string (class-name ci)) "List<Int32>")
          (string= (string (class-name cs)) "List<String>")))
  (t t t))

;;; A closed generic dispatches distinctly by its readable class (no collision with
;;; another instantiation).
(defgeneric %cft-gen (x))
(defmethod %cft-gen ((x #.(dotnet:class-for-type
                            (dotnet:make-generic-type "System.Collections.Generic.List"
                                                      '("System.Int32"))))) :int-list)
(defmethod %cft-gen ((x t)) :other)
(deftest class-for-type-closed-generic-dispatch
  (list (%cft-gen (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                        '("System.Int32"))))
        (%cft-gen 42))
  (:int-list :other))

;;; ============================================================
;;; N-way polymorphic dispatch cache — correctness across
;;; alternating argument classes (the old monomorphic cache only
;;; ever held one class key; the poly cache must still dispatch to
;;; the right method for each, and invalidate on defmethod).
;;; ============================================================

(defclass %pd-a () ()) (defclass %pd-b () ()) (defclass %pd-c () ())
(defclass %pd-d () ()) (defclass %pd-e () ())
(defgeneric %pd (x))
(defmethod %pd ((x %pd-a)) :a)
(defmethod %pd ((x %pd-b)) :b)
(defmethod %pd ((x %pd-c)) :c)
(defmethod %pd ((x %pd-d)) :d)
(defmethod %pd ((x %pd-e)) :e)

;; Alternate 2 classes many times: each call must return its own method.
(deftest pd-alternate-2
  (let ((a (make-instance '%pd-a)) (b (make-instance '%pd-b)) (acc '()))
    (dotimes (i 5) (push (%pd a) acc) (push (%pd b) acc))
    (remove-duplicates (nreverse acc)))
  (:a :b))

;; Cycle 5 classes > cache width (4): all still correct (misses re-fill).
(deftest pd-cycle-over-width
  (let ((xs (list (make-instance '%pd-a) (make-instance '%pd-b) (make-instance '%pd-c)
                  (make-instance '%pd-d) (make-instance '%pd-e))))
    (loop repeat 4 nconc (mapcar #'%pd xs)))
  (:a :b :c :d :e :a :b :c :d :e :a :b :c :d :e :a :b :c :d :e))

;; defmethod after warming the cache must be seen (invalidation).
(defclass %pd-inv () ())
(defgeneric %pdi (x))
(defmethod %pdi ((x %pd-inv)) :old)
(deftest pd-invalidate-on-defmethod
  (let ((o (make-instance '%pd-inv)))
    (dotimes (i 3) (%pdi o))                 ; warm cache with :old
    (eval '(defmethod %pdi ((x %pd-inv)) :new))
    (%pdi o))
  :new)

;; EQL + class specializers alternating (the eql cache path, 1 required arg).
(defgeneric %pde (x))
(defmethod %pde ((x (eql :k1))) :eql1)
(defmethod %pde ((x (eql :k2))) :eql2)
(defmethod %pde ((x symbol)) :sym)
(deftest pd-eql-alternate
  (let ((acc '()))
    (dotimes (i 3)
      (push (%pde :k1) acc) (push (%pde :k2) acc) (push (%pde :other) acc))
    (remove-duplicates (nreverse acc)))
  (:eql1 :eql2 :sym))

;; :before/:after (standard combination) poly across classes.
(defvar *pd-log* '())
(defgeneric %pdc (x))
(defmethod %pdc ((x %pd-a)) :a)
(defmethod %pdc ((x %pd-b)) :b)
(defmethod %pdc :before ((x %pd-a)) (push :ba *pd-log*))
(defmethod %pdc :before ((x %pd-b)) (push :bb *pd-log*))
(deftest pd-combination-alternate
  (let ((a (make-instance '%pd-a)) (b (make-instance '%pd-b)))
    (setf *pd-log* '())
    (list (%pdc a) (%pdc b) (%pdc a) (nreverse *pd-log*)))
  (:a :b :a (:ba :bb :ba)))

;;; ============================================================
;;; Item2: specialized standard slot reader (direct slot read).
;;; Must preserve unbound-slot, inheritance (slot at a different index
;;; in a subclass), and NOT specialize when a :before/around method or a
;;; user-redefined accessor body is present.
;;; ============================================================

(defclass %ra-base () ((v :initarg :v :accessor ra-v)))
(deftest ra-reader-basic
  (let ((o (make-instance '%ra-base :v 42)))
    (dotimes (i 3) (ra-v o))              ; warm the specialized-reader cache
    (ra-v o))
  42)

;; Unbound slot through the specialized reader must still signal.
(deftest ra-reader-unbound
  (let ((o (make-instance '%ra-base)))
    (dotimes (i 3) (ignore-errors (ra-v o)))
    (handler-case (progn (ra-v o) :no-error)
      (unbound-slot () :unbound)))
  :unbound)

;; Subclass places an extra slot first, so the inherited slot's index differs;
;; the specialized reader must use the subclass's own index.
(defclass %ra-sub (%ra-base) ((extra :initform 0)))
(deftest ra-reader-inheritance
  (let ((base (make-instance '%ra-base :v 1)) (sub (make-instance '%ra-sub :v 2)))
    (dotimes (i 3) (ra-v base) (ra-v sub))   ; both class keys warm
    (list (ra-v base) (ra-v sub)))
  (1 2))

;; A :before method disables the direct-read specialization (side effect must run).
(defvar *ra-log* nil)
(defclass %ra-b () ((w :initarg :w :accessor ra-w)))
(defmethod ra-w :before ((o %ra-b)) (push :before *ra-log*))
(deftest ra-reader-before-not-specialized
  (let ((o (make-instance '%ra-b :w 7)))
    (setf *ra-log* nil)
    (dotimes (i 3) (ra-w o))
    (list (ra-w o) (length *ra-log*)))     ; :before ran each call (4 total)
  (7 4))

;; A user-redefined accessor body (no longer a plain slot reader) must be honored.
(defclass %ra-c () ((z :initarg :z :accessor ra-z)))
(deftest ra-reader-custom-body
  (let ((o (make-instance '%ra-c :z 5)))
    (dotimes (i 3) (ra-z o))               ; warm specialized reader
    (eval '(defmethod ra-z ((o %ra-c)) :custom))  ; redefine -> invalidates
    (ra-z o))
  :custom)

;;; ============================================================
;;; Item2b: specialized standard slot writer ((setf accessor)).
;;; ============================================================

(defclass %wa () ((v :initarg :v :accessor wa-v)))
(deftest wa-writer-basic
  (let ((o (make-instance '%wa :v 0)))
    (dotimes (i 3) (setf (wa-v o) i))    ; warm the specialized-writer cache
    (setf (wa-v o) 99)
    (wa-v o))
  99)

(deftest wa-writer-returns-newval
  (let ((o (make-instance '%wa :v 0)))
    (dotimes (i 3) (setf (wa-v o) 1))
    (setf (wa-v o) 42))                  ; setf returns the stored value
  42)

;; Subclass: inherited slot at a different index; the specialized writer must
;; use the subclass's own index.
(defclass %wa-sub (%wa) ((extra :initform 0)))
(deftest wa-writer-inheritance
  (let ((b (make-instance '%wa :v 0)) (s (make-instance '%wa-sub :v 0)))
    (dotimes (i 3) (setf (wa-v b) 1) (setf (wa-v s) 2))
    (setf (wa-v b) 10) (setf (wa-v s) 20)
    (list (wa-v b) (wa-v s)))
  (10 20))

;; :before on the writer disables specialization (side effect must run).
(defvar *wa-log* nil)
(defclass %wa-b () ((w :initarg :w :accessor wa-w)))
(defmethod (setf wa-w) :before (nv (o %wa-b)) (push nv *wa-log*))
(deftest wa-writer-before-not-specialized
  (let ((o (make-instance '%wa-b :w 0)))
    (setf *wa-log* nil)
    (dotimes (i 3) (setf (wa-w o) i))
    (list (wa-w o) (reverse *wa-log*)))
  (2 (0 1 2)))

;; A newval-specialized user writer method takes over for that value type; the
;; default (specializable) writer still handles other value types.
(defclass %wa-n () ((z :initarg :z :accessor wa-z)))
(defmethod (setf wa-z) ((nv string) (o %wa-n)) (setf (slot-value o 'z) :was-string))
(deftest wa-writer-newval-specialized-mix
  (let ((o (make-instance '%wa-n :z 0)))
    (dotimes (i 3) (setf (wa-z o) 1))    ; integer newval -> default writer (specialized)
    (setf (wa-z o) "hi")                 ; string newval -> user method
    (list (wa-z o) (progn (setf (wa-z o) 7) (wa-z o))))
  (:was-string 7))

;; ---- Item3b: reader inline-cache (ReaderIC) soundness ----
;; A warm monomorphic call site must stay correct across accessor/class
;; redefinition, subclassing, unbound slots, and a bound NIL value.

(defclass %ric1 () ((x :initarg :x :accessor ric1x)))
(deftest ric-basic-warm
  (let ((o (make-instance '%ric1 :x 42)))
    (dotimes (i 5) (ric1x o))            ; warm the inline cache
    (ric1x o))
  42)

;; Adding a specialized primary method must deopt the cached slot read.
(defclass %ric2 () ((x :initarg :x :accessor ric2x)))
(deftest ric-deopt-after-defmethod
  (let ((o (make-instance '%ric2 :x 10)))
    (dotimes (i 5) (ric2x o))
    (defmethod ric2x ((o %ric2)) 999)
    (ric2x o))
  999)

;; An :around must likewise disable the direct slot read.
(defclass %ric3 () ((x :initarg :x :accessor ric3x)))
(deftest ric-deopt-around
  (let ((o (make-instance '%ric3 :x 7)))
    (dotimes (i 5) (ric3x o))
    (defmethod ric3x :around ((o %ric3)) (1+ (call-next-method)))
    (ric3x o))
  8)

;; One call site warmed on the base class, then hit with a subclass whose slot
;; sits at a different index — must miss and refill, not read the stale index.
(defclass %ricb () ((a :initarg :a :accessor ricg)))
(defclass %rics (%ricb) ((z :initarg :z) (a :initarg :a :accessor ricg)))
(defun %ric-read (o) (ricg o))
(deftest ric-subclass-refill
  (let ((b (make-instance '%ricb :a 1)) (s (make-instance '%rics :a 2 :z 9)))
    (dotimes (i 5) (%ric-read b))
    (list (%ric-read b) (%ric-read s) (%ric-read b)))
  (1 2 1))

;; Redefining the class to reorder slots must invalidate the warm cache.
(defclass %ric5 () ((x :initarg :x :accessor ric5x)))
(deftest ric-class-redef
  (let ((o (make-instance '%ric5 :x 3)))
    (dotimes (i 5) (ric5x o))
    (defclass %ric5 () ((y :initarg :y) (x :initarg :x :accessor ric5x)))
    (ric5x (make-instance '%ric5 :x 55 :y 1)))
  55)

;; Unbound slot goes through the SLOT-UNBOUND protocol even on the fast path.
(defclass %ric6 () ((x :accessor ric6x)))
(deftest ric-unbound
  (let ((o (make-instance '%ric6)))
    (list (handler-case (progn (ric6x o) :no-error)
            (unbound-slot () :unbound))
          (progn (setf (slot-value o 'x) 88) (ric6x o))))
  (:unbound 88))

;; A bound NIL must not be mistaken for an unbound slot.
(defclass %ric7 () ((x :initarg :x :accessor ric7x)))
(deftest ric-bound-nil
  (let ((o (make-instance '%ric7 :x nil)))
    (dotimes (i 5) (ric7x o))
    (ric7x o))
  nil)

;; A defmethod added to INITIALIZE-INSTANCE / SHARED-INITIALIZE AFTER a class has
;; already been instantiated must take effect on subsequent make-instance. The
;; per-class make-instance fast-path caches (SimpleInitChecked / HasCustomInit)
;; were not invalidated on add-method (the invalidation helper had lost its
;; callers in a merge), so a class instantiated before the method was added
;; silently skipped it.
(defclass %sic-init () ((x :initform 1)))
(deftest simple-init-cache-invalidated-on-add-method
  (progn
    (make-instance '%sic-init)   ; caches "no custom init methods" for this class
    (eval '(defmethod initialize-instance :after ((f %sic-init) &key)
             (setf (slot-value f 'x) 42)))
    (slot-value (make-instance '%sic-init) 'x))
  42)

(defclass %sic-shared () ((y :initform 1)))
(deftest simple-init-cache-invalidated-on-shared-initialize
  (progn
    (make-instance '%sic-shared)
    (eval '(defmethod shared-initialize :after ((f %sic-shared) slot-names &key)
             (declare (ignore slot-names))
             (setf (slot-value f 'y) 7)))
    (slot-value (make-instance '%sic-shared) 'y))
  7)

;; ---- Item3c: writer inline-cache (WriterIC) soundness ----
;; The (setf accessor) twin of the ric-* tests above: a warm monomorphic write
;; site must stay correct across accessor/class redefinition, subclassing,
;; newval-specialized methods, :around/:before, and :class allocation.

(defclass %wic1 () ((x :initarg :x :accessor wic1x)))
(deftest wic-basic-warm
  (let ((o (make-instance '%wic1 :x 0)))
    (dotimes (i 5) (setf (wic1x o) i))   ; warm the inline cache
    (setf (wic1x o) 42)
    (wic1x o))
  42)

;; SETF returns the new value, not the slot read-back.
(deftest wic-returns-newval
  (let ((o (make-instance '%wic1 :x 0)))
    (dotimes (i 5) (setf (wic1x o) i))
    (setf (wic1x o) :v))
  :v)

;; Adding a specialized primary writer must deopt the cached slot write.
(defclass %wic2 () ((x :initarg :x :accessor wic2x)))
(deftest wic-deopt-after-defmethod
  (let ((o (make-instance '%wic2 :x 0)))
    (dotimes (i 5) (setf (wic2x o) i))
    (defmethod (setf wic2x) (nv (o %wic2)) (setf (slot-value o 'x) (list :via-method nv)))
    (setf (wic2x o) 9)
    (wic2x o))
  (:via-method 9))

;; An :around on the writer must likewise disable the direct slot write.
(defclass %wic3 () ((x :initarg :x :accessor wic3x)))
(deftest wic-deopt-around
  (let ((o (make-instance '%wic3 :x 0)))
    (dotimes (i 5) (setf (wic3x o) i))
    (defmethod (setf wic3x) :around (nv (o %wic3)) (call-next-method (* 10 nv) o))
    (setf (wic3x o) 4)
    (wic3x o))
  40)

;; A :before must still run once the site is warm (side effect, not just value).
(defvar *wic-log* nil)
(defclass %wic4 () ((x :initarg :x :accessor wic4x)))
(defmethod (setf wic4x) :before (nv (o %wic4)) (push nv *wic-log*))
(deftest wic-before-runs
  (let ((o (make-instance '%wic4 :x 0)))
    (setf *wic-log* nil)
    (dotimes (i 3) (setf (wic4x o) i))
    (list (wic4x o) (reverse *wic-log*)))
  (2 (0 1 2)))

;; One write site warmed on the base class, then hit with a subclass whose slot
;; sits at a different index — must miss and refill, not write the stale index.
(defclass %wicb () ((a :initarg :a :accessor wicg)))
(defclass %wics (%wicb) ((z :initarg :z :initform 0) (a :initarg :a :accessor wicg)))
(defun %wic-write (o v) (setf (wicg o) v))
(deftest wic-subclass-refill
  (let ((b (make-instance '%wicb :a 0)) (s (make-instance '%wics :a 0)))
    (dotimes (i 5) (%wic-write b i))
    (%wic-write b 1) (%wic-write s 2)
    (list (wicg b) (wicg s) (slot-value s 'z)))
  (1 2 0))

;; Redefining the class to reorder slots must invalidate the warm cache.
(defclass %wic5 () ((x :initarg :x :accessor wic5x)))
(deftest wic-class-redef
  (let ((o (make-instance '%wic5 :x 0)))
    (dotimes (i 5) (setf (wic5x o) i))
    (defclass %wic5 () ((y :initarg :y :initform 1) (x :initarg :x :accessor wic5x)))
    (let ((n (make-instance '%wic5 :x 0)))
      (setf (wic5x n) 55)
      (list (wic5x n) (slot-value n 'y))))
  (55 1))

;; A newval-specialized user method must win even after the site is warm on the
;; default writer (the epoch bump from its defmethod deopts the cache).
(defclass %wic6 () ((z :initarg :z :accessor wic6z)))
(defmethod (setf wic6z) ((nv string) (o %wic6)) (setf (slot-value o 'z) :was-string))
(deftest wic-newval-specialized-mix
  (let ((o (make-instance '%wic6 :z 0)))
    (dotimes (i 3) (setf (wic6z o) 1))
    (setf (wic6z o) "hi")
    (list (wic6z o) (progn (setf (wic6z o) 7) (wic6z o))))
  (:was-string 7))

;; :allocation :class must write the shared class slot (visible from a second
;; instance), never a per-instance vector cell.
(defclass %wic7 () ((c :initform 0 :accessor wic7c :allocation :class)))
(deftest wic-class-allocation
  (let ((a (make-instance '%wic7)) (b (make-instance '%wic7)))
    (dotimes (i 3) (setf (wic7c a) i))
    (setf (wic7c a) 77)
    (list (wic7c a) (wic7c b)))
  (77 77))

;; The object form is evaluated for effect exactly once per write.
(defvar *wic-obj-evals* 0)
(defclass %wic8 () ((x :initarg :x :accessor wic8x)))
(defun %wic-obj (o) (incf *wic-obj-evals*) o)
(deftest wic-object-form-evaluated-once
  (let ((o (make-instance '%wic8 :x 0)))
    (dotimes (i 3) (setf (wic8x (%wic-obj o)) i))
    (setf *wic-obj-evals* 0)
    (setf (wic8x (%wic-obj o)) 5)
    (list (wic8x o) *wic-obj-evals*))
  (5 1))

;; A non-instance object (no applicable method) must still signal, not write.
(deftest wic-non-instance-errors
  (let ((o (make-instance '%wic8 :x 0)))
    (dotimes (i 3) (setf (wic8x o) i))
    (handler-case (progn (setf (wic8x 5) 1) :no-error)
      (error () :error)))
  :error)

;;; The other side of the capture rule: a body whose only use of
;;; CALL-NEXT-METHOD is a plain call keeps no closure, and reads the
;;; next-method state when the call runs. A generic-function dispatch in
;;; between must therefore leave that state as it found it.

(defgeneric cnm-direct-inner (x))
(defmethod cnm-direct-inner ((x integer)) (* x 2))

(defgeneric cnm-direct-gf (x))
(defclass cnm-direct-base () ())
(defclass cnm-direct-derived (cnm-direct-base) ())

(defmethod cnm-direct-gf ((x cnm-direct-base)) :base)
(defmethod cnm-direct-gf ((x cnm-direct-derived))
  (let ((n (cnm-direct-inner 21)))
    (list n (call-next-method))))

(deftest cnm-direct-after-inner-dispatch
  (cnm-direct-gf (make-instance 'cnm-direct-derived))
  (42 :base))

;;; NEXT-METHOD-P alone, called directly, likewise needs no capture.
(defgeneric nmp-direct-gf (x))
(defclass nmp-direct-base () ())
(defclass nmp-direct-derived (nmp-direct-base) ())

(defmethod nmp-direct-gf ((x nmp-direct-base)) (list :base (next-method-p)))
(defmethod nmp-direct-gf ((x nmp-direct-derived))
  (list (next-method-p) (call-next-method)))

(deftest nmp-direct-after-inner-dispatch
  (nmp-direct-gf (make-instance 'nmp-direct-derived))
  (t (:base nil)))
