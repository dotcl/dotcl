;;; Fixnum structure slots, read and written as raw values.
;;;
;;; A slot read in a fixnum context used to come back as a LispObject through
;;; Runtime.UnwrapMv and an unbox, and a fixnum store had to box the value into
;;; a temporary first -- so a loop that read two slots and wrote one paid three
;;; calls and an allocation per iteration on top of the work. The compiler now
;;; lowers both to typed entries (Runtime.StructRefL / Runtime.StructSetL).
;;;
;;; The point of the tests: the typed entries are what gets emitted where the
;;; context is fixnum, and nothing else about structures changes -- what a slot
;;; answers, what SETF returns, what an untyped slot does, and what a :TYPE
;;; violation does (which is: nothing, as before).

(setf dotcl:*save-sil* t)

(defstruct (sst-point (:constructor make-sst-point (a b)))
  (a 0 :type fixnum)
  (b 0 :type fixnum))

(defstruct sst-mixed
  (n 0 :type fixnum)
  (tag nil))

(defun %sst-typed-read-p (fn)
  (and (search "StructRefL" (princ-to-string (dotcl:function-sil fn))) t))

(defun %sst-typed-write-p (fn)
  (and (search "StructSetL" (princ-to-string (dotcl:function-sil fn))) t))

(defun %sst-generic-p (fn)
  (let ((sil (princ-to-string (dotcl:function-sil fn))))
    (and (or (search "StructRefI" sil) (search "StructSetI" sil)) t)))

;;; ---- what gets emitted ----

(defun %sst-loop (p n)
  (declare (type sst-point p) (fixnum n)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (do ((i 0 (the fixnum (1+ i))))
        ((>= i n) acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc
                               (the fixnum (sst-point-a p))
                               (the fixnum (sst-point-b p)))))
      (setf (sst-point-a p) i))))

(deftest-compiled-only struct-slot-typed.loop-is-typed
  (list (%sst-typed-read-p #'%sst-loop)
        (%sst-typed-write-p #'%sst-loop)
        (%sst-generic-p #'%sst-loop))
  (t t nil))

;; A statement-position fixnum store keeps no box at all: with the UnwrapMv of a
;; freshly boxed Fixnum gone, the peephole deletes the box itself. This function
;; returns NIL, so a surviving Fixnum.Make could only be the discarded store.
(defun %sst-store-only (p i)
  (declare (type sst-point p) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (setf (sst-point-a p) i)
  nil)

(deftest-compiled-only struct-slot-typed.statement-store-has-no-box
  (list (%sst-typed-write-p #'%sst-store-only)
        (and (search "Fixnum.Make" (princ-to-string (dotcl:function-sil #'%sst-store-only))) t))
  (t nil))

;; No fixnum context, no typed entry: the generic path is what still runs.
(defun %sst-untyped-read (p)
  (list (sst-point-a p) (sst-point-b p)))

(deftest-compiled-only struct-slot-typed.untyped-context-stays-generic
  (list (%sst-typed-read-p #'%sst-untyped-read)
        (%sst-generic-p #'%sst-untyped-read))
  (nil t))

;; A local function shadowing the accessor is a call to that function, not a
;; slot read.
(defun %sst-shadowed (p)
  (flet ((sst-point-a (x) (declare (ignore x)) 99))
    (the fixnum (sst-point-a p))))

(deftest-compiled-only struct-slot-typed.local-shadow-not-typed
  (list (%sst-typed-read-p #'%sst-shadowed) (%sst-shadowed (make-sst-point 1 2)))
  (nil 99))

;;; ---- what it answers ----

(deftest struct-slot-typed.loop-value
  (let ((p (make-sst-point 3 4)))
    (list (%sst-loop p 5) (sst-point-a p) (sst-point-b p)))
  (29 4 4))

;; Values past the small-integer cache: the typed store has to carry the whole
;; fixnum, not a cached one.
(defun %sst-big-store (p n)
  (declare (type sst-point p) (fixnum n))
  (setf (sst-point-a p) n)
  (the fixnum (sst-point-a p)))

(deftest struct-slot-typed.large-values
  (list (%sst-big-store (make-sst-point 0 0) 1000000)
        (%sst-big-store (make-sst-point 0 0) -1000000)
        (%sst-big-store (make-sst-point 0 0) most-positive-fixnum))
  (1000000 -1000000 #.most-positive-fixnum))

;; SETF answers the value it stored, on both paths.
(deftest struct-slot-typed.setf-returns-value
  (let ((p (make-sst-point 0 0))
        (m (make-sst-mixed)))
    (let ((i 5))
      (declare (fixnum i))
      (list (setf (sst-point-a p) i)            ; typed path
            (setf (sst-point-b p) 77)           ; constant, typed path
            (setf (sst-mixed-tag m) :hello)     ; untyped slot, generic path
            (sst-point-a p) (sst-point-b p) (sst-mixed-tag m))))
  (5 77 :hello 5 77 :hello))

;; A value with a side effect is evaluated exactly once.
(defvar *sst-evals* 0)
(defun %sst-next ()
  (setq *sst-evals* (+ *sst-evals* 1))
  42)

(deftest struct-slot-typed.value-evaluated-once
  (let ((p (make-sst-point 0 0)))
    (setq *sst-evals* 0)
    (let ((v (setf (sst-point-a p) (%sst-next))))
      (list v (sst-point-a p) *sst-evals*)))
  (42 42 1))

;; An untyped slot in the same structure is untouched by any of this.
(deftest struct-slot-typed.mixed-struct
  (let ((m (make-sst-mixed :n 3 :tag :x)))
    (setf (sst-mixed-n m) 9)
    (setf (sst-mixed-tag m) '(a b))
    (list (sst-mixed-n m) (sst-mixed-tag m)))
  (9 (a b)))

;;; ---- the :TYPE declaration ----

;; A slot's :TYPE is now checked on write. It used to be dropped during
;; macroexpansion -- the declared type existed nowhere after DEFSTRUCT had run,
;; so a (:type fixnum) slot took a string in silence. CLHS 3.3.1 leaves a
;; violated declaration undefined and silence was a legal reading, but it is the
;; least useful one: the writer said what belongs in the slot.
(deftest struct-slot-typed.type-violation-signals
  (let ((p (make-sst-point 0 0)))
    (handler-case (progn (setf (sst-point-a p) "not a fixnum")
                         (list :stored (sst-point-a p)))
      (type-error () :type-error)))
  :type-error)

;; safety 0 is where the writer asked not to be checked, so no check is emitted
;; there. What is pinned is the ABSENCE OF THE CHECK in the code, not what
;; happens when the declaration is violated anyway: a slot that fits an int64 is
;; stored raw, and raw storage has nowhere to put a symbol, so the store still
;; reports it -- from the storage rather than from a check. CLHS 3.3.1 leaves a
;; violated declaration undefined, so both are legal; which one you get depends
;; on whether the slot earned raw storage, and that is not a promise worth
;; making to a program that has already broken its own declaration.
;;
;; DEFTEST-COMPILED-ONLY: the check is elided by the COMPILER reading the
;; policy, and the interpreter has no per-function policy to read -- it checks
;; either way, which is the same licence used in the other direction.
(defun %sst-violate-safety0 (p v)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (setf (sst-point-a p) v)
  (sst-point-a p))

(deftest-compiled-only struct-slot-typed.type-violation-safety0-emits-no-check
  (and (search "CheckSlotType"
               (princ-to-string (dotcl:function-sil #'%sst-violate-safety0)))
       t)
  nil)

;; An untyped slot is never stored raw, so a safety 0 store into one goes
;; through exactly as it always did -- this is the shape that still shows the
;; elision end to end.
(defun %sst-violate-safety0-untyped (m v)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (setf (sst-mixed-tag m) v)
  (sst-mixed-tag m))

(deftest-compiled-only struct-slot-typed.untyped-safety0-store-goes-through
  (handler-case (%sst-violate-safety0-untyped (make-sst-mixed) :sym)
    (error () :error))
  :sym)

;; An untyped slot takes anything, as before: the check exists only where a
;; :TYPE was written.
(deftest struct-slot-typed.untyped-slot-unchecked
  (let ((m (make-sst-mixed)))
    (setf (sst-mixed-tag m) "anything")
    (sst-mixed-tag m))
  "anything")

;; The constructor checks the same way the accessor does -- a slot cannot be
;; born holding what it may not later be assigned.
(deftest struct-slot-typed.constructor-checks
  (handler-case (make-sst-point "not a fixnum" 0)
    (type-error () :type-error))
  :type-error)

;; :INCLUDE carries the parent's slot types to the child, for both the
;; inherited accessor and the child's own.
(defstruct (sst-point3 (:include sst-point))
  (c 0 :type fixnum))

(deftest struct-slot-typed.include-inherits-the-type
  (let ((q (make-sst-point3)))
    (list (handler-case (setf (sst-point-a q) "s") (type-error () :type-error))
          (handler-case (setf (sst-point3-c q) "s") (type-error () :type-error))))
  (:type-error :type-error))

;; Redefining the structure with a different slot type uses the new type.
(deftest struct-slot-typed.redefinition-uses-the-new-type
  (progn
    (eval '(defstruct sst-redef (v 0 :type fixnum)))
    (prog1 (list (handler-case (setf (sst-redef-v (make-sst-redef)) "s")
                   (type-error () :type-error))
                 (progn (eval '(defstruct sst-redef (v nil)))
                        (let ((r (make-sst-redef)))
                          (setf (sst-redef-v r) "s")
                          (sst-redef-v r))))))
  (:type-error "s"))


;;; ---- DOUBLE-FLOAT slots ----
;;;
;;; A DOUBLE-FLOAT slot is stored raw too, in the same int64 vector as the
;;; integer slots but as IEEE bits, with a per-slot kind byte saying which it
;;; is. SINGLE-FLOAT deliberately does NOT get raw storage: it would need a
;;; third kind so that a generic read boxes a SINGLE-FLOAT rather than a
;;; DOUBLE-FLOAT, and the tests below pin that it stays on the generic path.

(defstruct (sst-vec (:constructor make-sst-vec (x y)))
  (x 0.0d0 :type double-float)
  (y 0.0d0 :type double-float))

(defstruct sst-sf
  (s 0.0 :type single-float))

(defun %sst-double-read-p (fn)
  (and (search "StructRefD" (princ-to-string (dotcl:function-sil fn))) t))

(defun %sst-double-write-p (fn)
  (and (search "StructSetD" (princ-to-string (dotcl:function-sil fn))) t))

(defun %sst-vec-loop (v n)
  (declare (type sst-vec v) (fixnum n)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0.0d0))
    (declare (double-float acc))
    (do ((i 0 (the fixnum (1+ i))))
        ((>= i n) acc)
      (declare (fixnum i))
      (setq acc (the double-float (+ acc (sst-vec-x v))))
      (setf (sst-vec-x v) (the double-float (+ (sst-vec-x v) 1.0d0))))))

(deftest-compiled-only struct-slot-typed.double-loop-is-typed
  (list (%sst-double-read-p #'%sst-vec-loop)
        (%sst-double-write-p #'%sst-vec-loop)
        (%sst-generic-p #'%sst-vec-loop))
  (t t nil))

(deftest struct-slot-typed.double-loop-value
  (let ((v (make-sst-vec 3.0d0 4.0d0)))
    (list (%sst-vec-loop v 5) (sst-vec-x v) (sst-vec-y v)))
  (25.0d0 8.0d0 4.0d0))

;; A raw slot read without a declaration has to box back as the type that went
;; in. The bits in the vector are the same 64 bits either way, so getting this
;; wrong answers a FIXNUM with the IEEE pattern as its value.
(deftest struct-slot-typed.double-generic-read-boxes-a-double
  (let ((v (make-sst-vec 1.5d0 -2.5d0)))
    (list (type-of (sst-vec-x v)) (sst-vec-x v) (sst-vec-y v)))
  (double-float 1.5d0 -2.5d0))

;; SINGLE-FLOAT stays on the generic path, and keeps working there.
(defun %sst-sf-write (s v)
  (declare (type sst-sf s) (single-float v)
           (optimize (speed 3) (safety 0) (debug 0)))
  (setf (sst-sf-s s) v))

(deftest-compiled-only struct-slot-typed.single-float-slot-stays-generic
  (list (%sst-double-write-p #'%sst-sf-write)
        (%sst-generic-p #'%sst-sf-write))
  (nil t))

(deftest struct-slot-typed.single-float-slot-value
  (let ((s (make-sst-sf)))
    (%sst-sf-write s 1.5)
    (list (type-of (sst-sf-s s)) (sst-sf-s s)))
  (single-float 1.5))

;; The :TYPE is checked on the double path the same as on the integer one, at
;; the accessor and at the constructor.
(deftest struct-slot-typed.double-type-violation-signals
  (let ((v (make-sst-vec 0.0d0 0.0d0)))
    (list (handler-case (setf (sst-vec-x v) "s") (type-error () :type-error))
          (handler-case (setf (sst-vec-x v) 3) (type-error () :type-error))
          (handler-case (make-sst-vec 1.0 0.0d0) (type-error () :type-error))))
  (:type-error :type-error :type-error))

;; As for the integer slots, safety 0 is where the check is not emitted. What
;; happens to a violated declaration there is undefined and comes from the
;; storage, not from a check.
(defun %sst-double-violate-safety0 (v x)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (setf (sst-vec-x v) x)
  (sst-vec-x v))

(deftest-compiled-only struct-slot-typed.double-safety0-emits-no-check
  (and (search "CheckSlotType"
               (princ-to-string (dotcl:function-sil #'%sst-double-violate-safety0)))
       t)
  nil)

;; Copying a structure has to carry the raw slots across, not the bits read as
;; something else.
(deftest struct-slot-typed.double-copy-and-equalp
  (let* ((v (make-sst-vec 1.5d0 -2.5d0))
         (c (copy-sst-vec v)))
    (list (sst-vec-x c) (sst-vec-y c) (equalp v c)
          (progn (setf (sst-vec-x c) 9.0d0)
                 (list (sst-vec-x v) (sst-vec-x c)))))
  (1.5d0 -2.5d0 t (1.5d0 9.0d0)))


;;; ---- the keyword constructor ----
;;;
;;; The constructor's own DEFUN checked its initial values all along, but a call
;;; with constant keywords is rewritten at the call site into the %MAKE-STRUCT
;;; the DEFUN would have run -- and the rewrite dropped the checks. So the same
;;; call signalled interpreted (which calls the DEFUN) and stored silently
;;; compiled, which is the one thing a check must not do.

(defstruct (kct (:constructor make-kct) (:constructor make-kct-boa (n tag)))
  (n 0 :type fixnum)
  (tag nil))

(deftest struct-slot-typed.keyword-ctor-checks
  (handler-case (make-kct :n "not a fixnum")
    (type-error () :type-error))
  :type-error)

;; The three ways to put a value in a slot report the same thing. They did not:
;; one of them reported nothing.
(deftest struct-slot-typed.three-paths-report-alike
  (flet ((msg (thunk)
           (handler-case (progn (funcall thunk) :accepted)
             (type-error (e) (format nil "~a" e))
             (error (e) (list :wrong-condition (type-of e))))))
    (let ((keyword (msg (lambda () (make-kct :n "s"))))
          (boa (msg (lambda () (make-kct-boa "s" nil))))
          (setf- (msg (lambda () (setf (kct-n (make-kct)) "s")))))
      (list (equal keyword boa) (equal keyword setf-) (stringp keyword))))
  (t t t))

;; A slot default is an initial value like any other, and an omitted slot takes
;; it through the same rewrite.
(defstruct kct-bad-default (n "not a fixnum" :type fixnum))

(deftest struct-slot-typed.keyword-ctor-checks-the-default
  (handler-case (make-kct-bad-default)
    (type-error () :type-error))
  :type-error)

;; Keywords out of slot order, and a slot left out: the rewrite has to keep
;; putting values where they belong once it also emits checks.
(deftest struct-slot-typed.keyword-ctor-still-builds
  (let ((a (make-kct :tag :x :n 5))
        (b (make-kct)))
    (list (kct-n a) (kct-tag a) (kct-n b) (kct-tag b)))
  (5 :x 0 nil))

;; Arguments are evaluated left to right as written, checks or no checks.
(defvar *kct-order* '())
(defun %kct-note (x) (push x *kct-order*) x)

(deftest struct-slot-typed.keyword-ctor-evaluation-order
  (progn
    (setq *kct-order* '())
    (make-kct :tag (%kct-note :first) :n (%kct-note 2))
    (reverse *kct-order*))
  (:first 2))

;; safety 0 is where the writer asked not to be checked, and the rewrite obeys
;; that the way every other check does: none is emitted.
(defun %kct-safety0 (v)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (make-kct :n v))

(deftest-compiled-only struct-slot-typed.keyword-ctor-safety0-emits-no-check
  (and (search "CheckSlotType" (princ-to-string (dotcl:function-sil #'%kct-safety0))) t)
  nil)

(setf dotcl:*save-sil* nil)
