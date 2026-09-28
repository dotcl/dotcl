;;; A structure slot read answers exactly one value, and the compiler is
;;; allowed to know it.
;;;
;;; An inlined DEFSTRUCT accessor lowers to Runtime.StructRefI, which hands
;;; back the object in the slot. That object is never an MvReturn: every store
;;; into a slot evaluates its value in a single-value position, the same
;;; reason CAR and AREF have long been treated as single-valued. So the
;;; Runtime.UnwrapMv the compiler used to append after every such read could
;;; only ever be a no-op, and it is no longer emitted.
;;;
;;; The elision is decided from the code that was emitted, not from the shape
;;; of the source form, and that is what the second half of this file is
;;; about. A name that looks like an accessor is not enough: a macro, a
;;; compiler macro, an INLINE proclamation, a local FLET binding or a typed
;;; (:TYPE LIST / :TYPE VECTOR) structure all mean something other than a slot
;;; read is compiled there, and that something may legitimately answer several
;;; values. Those forms must keep their unwrap. Getting this wrong does not
;;; make anything slow, it silently drops values, so the value tests come
;;; first and the instruction counts after.

(setf dotcl:*save-sil* t)

(defun %svv-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %svv-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- the structures ----

(defstruct svv a b)

;; The accessor name is the slot name.
(defstruct (svv-cn (:conc-name nil)) cx cy)

;; An accessor name that is neither the default nor the slot name.
(defstruct (svv-custom (:conc-name qq-)) (q 0))

;; Inherited accessors reach the same registry as own ones.
(defstruct (svv-inc (:include svv)) c)

;; A typed structure registers no accessor for inlining at all.
(defstruct (svv-list (:type list)) la lb)

;; The name an FLET will shadow, and the name a macro will take over.
(defstruct svv-shadow (sv 0))
(defstruct svv-macro (mv 0))

(defmacro svv-macro-mv (x)
  (declare (ignore x))
  '(values 101 202))

;;; ---- the shapes ----

(defun %svv-read (s) (svv-a s))

(defun %svv-read-the (s) (the integer (svv-a s)))

(defun %svv-pair (s) (list (svv-a s) (svv-b s)))

;; A slot read that follows a form which left two values behind. If the read
;; were to leave the earlier values in place, this is where it would show.
(defun %svv-after-values (s)
  (progn (values 1 2) (svv-a s)))

(defun %svv-setf (s) (setf (svv-a s) (values 30 40)))

(defun %svv-flet (s)
  (flet ((svv-shadow-sv (x) (declare (ignore x)) (values 11 22)))
    (list (svv-shadow-sv s) (multiple-value-list (svv-shadow-sv s)))))

(defun %svv-via-macro (s)
  (list (svv-macro-mv s) (multiple-value-list (svv-macro-mv s))))

(defun %svv-indirect (s) (list (funcall #'svv-a s) (apply #'svv-b (list s))))

(defun %svv-typed (s) (list (svv-list-la s) (svv-list-lb s)))

;;; ---- values ----

(deftest struct-slot-single-value.read
  (%svv-read (make-svv :a 1 :b 2))
  1)

(deftest struct-slot-single-value.read-is-one-value
  (multiple-value-list (%svv-read (make-svv :a 1 :b 2)))
  (1))

;; The read is the last form of the function, so the caller is the one asking
;; for values. There is still only one.
(deftest struct-slot-single-value.tail-read-is-one-value
  (multiple-value-list (svv-a (make-svv :a 5 :b 6)))
  (5))

(deftest struct-slot-single-value.read-in-mv-list
  (multiple-value-list (svv-a (make-svv :a 7 :b 8)))
  (7))

(deftest struct-slot-single-value.after-two-values
  (multiple-value-list (%svv-after-values (make-svv :a 9 :b 10)))
  (9))

(deftest struct-slot-single-value.after-two-values-inline
  (multiple-value-list (progn (values 1 2) (svv-a (make-svv :a 9 :b 10))))
  (9))

(deftest struct-slot-single-value.the-wrapped
  (multiple-value-list (%svv-read-the (make-svv :a 4 :b 0)))
  (4))

(deftest struct-slot-single-value.pair
  (%svv-pair (make-svv :a 1 :b 2))
  (1 2))

;; SETF of an accessor is a store, not a read: it takes the primary value and
;; answers it, as one value.
(deftest struct-slot-single-value.setf-takes-primary
  (let ((s (make-svv :a 1 :b 2)))
    (list (multiple-value-list (%svv-setf s)) (svv-a s)))
  ((30) 30))

(deftest struct-slot-single-value.conc-name-nil
  (let ((s (make-svv-cn :cx 1 :cy 2)))
    (list (multiple-value-list (cx s)) (multiple-value-list (cy s))))
  ((1) (2)))

(deftest struct-slot-single-value.custom-conc-name
  (multiple-value-list (qq-q (make-svv-custom :q 3)))
  (3))

(deftest struct-slot-single-value.inherited-accessor
  (let ((s (make-svv-inc :a 1 :b 2 :c 3)))
    (list (multiple-value-list (svv-a s)) (multiple-value-list (svv-inc-c s))))
  ((1) (3)))

;; An FLET binding the accessor name is a call to that function, and it may
;; answer as many values as it likes.
(deftest struct-slot-single-value.flet-shadow-keeps-values
  (%svv-flet (make-svv-shadow :sv 1))
  (11 (11 22)))

;; So may a macro that has taken the name over: what is compiled there is the
;; expansion, which was never a slot read.
(deftest struct-slot-single-value.macro-shadow-keeps-values
  (%svv-via-macro (make-svv-macro :mv 1))
  (101 (101 202)))

;; Not inlined at all: the accessor is reached as a function object.
(deftest struct-slot-single-value.funcall-and-apply
  (%svv-indirect (make-svv :a 1 :b 2))
  (1 2))

;; A typed structure has no LispStruct behind it, so its accessors are
;; ordinary functions and are compiled as calls.
(deftest struct-slot-single-value.typed-struct
  (%svv-typed (list 1 2))
  (1 2))

;; A slot holding the primary of a multi-valued expression holds that one
;; object, and reading it back gives that one object.
(deftest struct-slot-single-value.slot-holds-primary
  (let ((s (make-svv :a (values 61 62) :b 0)))
    (multiple-value-list (svv-a s)))
  (61))

;;; ---- emitted code ----

;; The read itself: one StructRefI, and nothing after it.
(deftest-emitting-only struct-slot-single-value.no-unwrap-after-read
  (let ((r (%svv-sil #'%svv-read)))
    (list (%svv-count "Runtime.StructRefI" r)
          (%svv-count "Runtime.UnwrapMv" r)))
  (1 0))

(deftest-emitting-only struct-slot-single-value.no-unwrap-under-the
  (let ((r (%svv-sil #'%svv-read-the)))
    (list (%svv-count "Runtime.StructRefI" r)
          (%svv-count "Runtime.UnwrapMv" r)))
  (1 0))

(deftest-emitting-only struct-slot-single-value.no-unwrap-for-either-slot
  (let ((r (%svv-sil #'%svv-pair)))
    (list (%svv-count "Runtime.StructRefI" r)
          (%svv-count "Runtime.UnwrapMv" r)))
  (2 0))

;; The shadowed forms emit no StructRefI, so they keep their unwrap. This is
;; the assertion that would catch a version of the elision that trusted the
;; accessor's name instead of the code emitted for the call.
(deftest-emitting-only struct-slot-single-value.flet-shadow-keeps-unwrap
  (let ((r (%svv-sil #'%svv-flet)))
    (list (%svv-count "Runtime.StructRefI" r)
          (plusp (%svv-count "Runtime.UnwrapMv" r))))
  (0 t))

(deftest-emitting-only struct-slot-single-value.macro-shadow-keeps-unwrap
  (let ((r (%svv-sil #'%svv-via-macro)))
    (list (%svv-count "Runtime.StructRefI" r)
          (plusp (%svv-count "Runtime.UnwrapMv" r))))
  (0 t))

(deftest-emitting-only struct-slot-single-value.typed-struct-keeps-unwrap
  (let ((r (%svv-sil #'%svv-typed)))
    (list (%svv-count "Runtime.StructRefI" r)
          (plusp (%svv-count "Runtime.UnwrapMv" r))))
  (0 t))

;;; ---- a redefined accessor ----
;;;
;;; DEFUN over an accessor name does not unregister the slot, so a compiled
;;; call site still reads the slot rather than calling the new definition.
;;; That predates this change and is the same choice DEFSTRUCT makes when a
;;; slot accessor collides with the predicate name: the compiled call is the
;;; slot read. What matters here is only that it stays one value -- the
;;; elision must not let the new definition's second value escape as a raw
;;; MvReturn. The interpreter has no inlined call site and calls the
;;; redefinition, so this one is about emitted code and is compiled-only.

(defstruct svv-redef (rv 0))

(defun svv-redef-rv (x) (declare (ignore x)) (values 55 66))

(defun %svv-redefined (s)
  (list (svv-redef-rv s) (multiple-value-list (svv-redef-rv s))))

(deftest-compiled-only struct-slot-single-value.redefined-accessor-is-one-value
  (%svv-redefined (make-svv-redef :rv 3))
  (3 (3)))

(setf dotcl:*save-sil* nil)
