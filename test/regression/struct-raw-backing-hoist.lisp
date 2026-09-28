;;; A structure's raw slot array is fetched once per binding, not per access.
;;;
;;; Slots whose declared type fits an int64 or a double live in a long[] beside
;;; the boxed slot vector. Reaching one used to mean re-deriving, on EVERY
;;; access, the array, the layout entry and the position in it, from the object:
;;; about twenty-five instructions where C# has one load. A binding declared to
;;; hold such a structure now fetches the array once, and each access is an
;;; element load at a constant position.
;;;
;;; Two things make that sound, and the tests below are mostly about them.
;;;
;;; The array cannot move under the binding. A structure's raw storage and its
;;; layout map are readonly fields written only by the constructor, so for one
;;; instance they are fixed for its lifetime. This is the opposite of a slot
;;; that HOLDS an array, where (SETF (ACC S) OTHER) replaces the array and a
;;; hoisted reference would go stale -- which is why that hoist was refused.
;;;
;;; And the fetch never signals. It answers NIL for anything it cannot serve --
;;; a false declaration, a stale layout version, an instance with no raw storage
;;; at all, and an instance that HAD raw slots but dropped them at construction
;;; because a value contradicted its declared type. Every access site has an arm
;;; for NIL which is the code that ran before, so those cases keep answering and
;;; failing exactly as they did. The last of them is why signalling in the
;;; prologue is not an option: such an instance is perfectly usable.
;;;
;;; The position in the raw array is NOT the slot index -- only raw slots take a
;;; position -- so a structure with a boxed slot in the middle is the shape that
;;; catches a wrong mapping, and it reads the wrong slot rather than failing.
;;; SRH-MIX is here for that.
;;;
;;; The hoist is also refused where it would not pay. The fetch runs on every
;;; entry to the function, so it is only taken when a RAW slot of that variable
;;; is reached from inside a loop; a body that touches each slot once, or that
;;; loops over something else entirely, keeps the per-access path. That is why
;;; several functions here carry a loop that runs once: without it they would
;;; be testing the old path under the new path's name.

(setf dotcl:*save-sil* t)

(defun %srh-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %srh-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- the structures ----

(defstruct srh (a 0 :type fixnum) (b 0 :type fixnum))

;; A boxed slot between two raw ones: slot indices 0,1,2,3 map to raw
;; positions 0,-1,1,2. Reading R by its slot index would answer S, and S would
;; run off the end. The first slot is W and not P because a slot named P under
;; the default conc-name gives an accessor with the same name as the predicate,
;; which DEFSTRUCT resolves in the accessor's favour with a warning -- true,
;; documented elsewhere, and only noise here.
(defstruct srh-mix
  (w 0 :type fixnum)
  (q nil)
  (r 0 :type fixnum)
  (s 0 :type fixnum))

;; Both raw kinds in one structure: they share the one long[].
(defstruct srh-two (i 0 :type fixnum) (d 0.0d0 :type double-float))

;; No :TYPE anywhere, so no raw storage and nothing to hoist.
(defstruct srh-none x y)

(defstruct (srh-cn (:conc-name nil)) (cna 0 :type fixnum))

(defstruct (srh-inc (:include srh)) (c 0 :type fixnum))

;;; ---- the shapes ----

(defun %srh-sum (s)
  (declare (type srh s) (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (+ (the fixnum (srh-a s)) (the fixnum (srh-b s)))))

(defun %srh-rmw (s n)
  (declare (type srh s) (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (do ((i 0 (the fixnum (1+ i)))) ((>= i n) acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (the fixnum (srh-a s)) (the fixnum (srh-b s)))))
      (setf (srh-a s) i))))

(defun %srh-mix-read (m)
  (declare (type srh-mix m) (optimize (speed 3) (safety 0) (debug 0)))
  (list (the fixnum (srh-mix-w m))
        (srh-mix-q m)
        (the fixnum (srh-mix-r m))
        (the fixnum (srh-mix-s m))))

;; The same three raw slots, read inside a loop and where the compiler wants
;; machine values, which is what the hoist asks for. The boxed slot cannot take
;; that path and does not. One iteration, so the value is the plain sum.
(defun %srh-mix-raw (m)
  (declare (type srh-mix m) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (dotimes (k 1)
      (setq acc (the fixnum (+ (the fixnum (srh-mix-w m))
                               (the fixnum (srh-mix-r m))
                               (the fixnum (srh-mix-s m))))))
    (list acc (srh-mix-q m))))

(defun %srh-mix-write (m)
  (declare (type srh-mix m) (optimize (speed 3) (safety 0) (debug 0)))
  (setf (srh-mix-w m) 10)
  (setf (srh-mix-q m) :boxed)
  (setf (srh-mix-r m) 30)
  (setf (srh-mix-s m) 40)
  m)

(defun %srh-two (v)
  (declare (type srh-two v) (optimize (speed 3) (safety 0) (debug 0)))
  (setf (srh-two-i v) 7)
  (setf (srh-two-d v) 2.5d0)
  (list (the fixnum (srh-two-i v)) (the double-float (srh-two-d v))))

(defun %srh-none (s)
  (declare (type srh-none s) (optimize (speed 3) (safety 0) (debug 0)))
  (list (srh-none-x s) (srh-none-y s)))

(defun %srh-cn (s)
  (declare (type srh-cn s) (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (cna s)))

(defun %srh-inc (s)
  (declare (type srh-inc s) (optimize (speed 3) (safety 0) (debug 0)))
  (list (the fixnum (srh-a s)) (the fixnum (srh-inc-c s))))

;; The bare type name, without TYPE, is the other legal spelling.
(defun %srh-bare-decl (s)
  (declare (srh s) (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (srh-a s)))

;; A LET binding rather than a parameter.
(defun %srh-let (s)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((v s))
    (declare (type srh v))
    (the fixnum (+ (the fixnum (srh-a v)) (the fixnum (srh-b v))))))

;; Assigned inside the body: the binding may come to hold a different instance,
;; whose raw array is a different array, so it must not be hoisted.
(defun %srh-setq (s other)
  (declare (type srh s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((first (the fixnum (srh-a s))))
    (declare (fixnum first))
    (setq s other)
    (list first (the fixnum (srh-a s)))))

;; Shadowed by an inner binding of the same name: the inner one is a different
;; object and the key check is what notices.
(defun %srh-shadow (s other)
  (declare (type srh s) (optimize (speed 3) (safety 0) (debug 0)))
  (list (the fixnum (srh-a s))
        (let ((s other))
          (the fixnum (srh-a s)))
        (the fixnum (srh-a s))))

;; Captured by a closure that assigns to it: a boxed variable does not live in
;; a plain slot, so it is excluded the same way the array hoist excludes it.
(defun %srh-captured (s other)
  (declare (type srh s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((f (lambda () (setq s other))))
    (let ((before (the fixnum (srh-a s))))
      (declare (fixnum before))
      (funcall f)
      (list before (the fixnum (srh-a s))))))

;;; ---- values ----

(deftest struct-raw-backing-hoist.read
  (%srh-sum (make-srh :a 3 :b 4))
  7)

(deftest struct-raw-backing-hoist.read-write-loop
  (let ((s (make-srh :a 1 :b 2)))
    (list (%srh-rmw s 4) (srh-a s) (srh-b s)))
  ;; A starts at 1 and the loop writes I into it each iteration, so the four
  ;; A values read are 1, 0, 1, 2 and B stays 2: 3, 5, 8, 12. A ends at 3.
  (12 3 2))

;; The mapping test. Answering the slot index instead of the raw position
;; would read R where P was asked for, silently.
(deftest struct-raw-backing-hoist.boxed-slot-in-the-middle
  (%srh-mix-read (make-srh-mix :w 1 :q :here :r 3 :s 4))
  (1 :here 3 4))

(deftest struct-raw-backing-hoist.boxed-slot-in-the-middle-raw
  (%srh-mix-raw (make-srh-mix :w 1 :q :here :r 3 :s 4))
  (8 :here))

(deftest struct-raw-backing-hoist.boxed-slot-in-the-middle-write
  (let ((m (%srh-mix-write (make-srh-mix))))
    (list (srh-mix-w m) (srh-mix-q m) (srh-mix-r m) (srh-mix-s m)))
  (10 :boxed 30 40))

(deftest struct-raw-backing-hoist.both-raw-kinds
  (%srh-two (make-srh-two))
  (7 2.5d0))

(deftest struct-raw-backing-hoist.no-raw-slots
  (%srh-none (make-srh-none :x 1 :y 2))
  (1 2))

(deftest struct-raw-backing-hoist.conc-name-nil
  (%srh-cn (make-srh-cn :cna 5))
  5)

(deftest struct-raw-backing-hoist.included-slots
  (%srh-inc (make-srh-inc :a 1 :b 2 :c 3))
  (1 3))

(deftest struct-raw-backing-hoist.bare-type-declaration
  (%srh-bare-decl (make-srh :a 9 :b 0))
  9)

(deftest struct-raw-backing-hoist.let-binding
  (%srh-let (make-srh :a 6 :b 7))
  13)

;; The exclusions. Each of these must read the object it actually has.
(deftest struct-raw-backing-hoist.assigned-variable
  (%srh-setq (make-srh :a 1 :b 0) (make-srh :a 2 :b 0))
  (1 2))

(deftest struct-raw-backing-hoist.shadowing-binding
  (%srh-shadow (make-srh :a 1 :b 0) (make-srh :a 2 :b 0))
  (1 2 1))

(deftest struct-raw-backing-hoist.captured-and-assigned
  (%srh-captured (make-srh :a 1 :b 0) (make-srh :a 2 :b 0))
  (1 2))

;;; ---- a false declaration changes nothing ----
;;;
;;; Declaring a variable to hold one structure and handing it another is
;;; undefined (CLHS 3.3.1), and what dotcl does with it today is read the slot
;;; at that index in whatever object arrived -- a compiled accessor does not
;;; check the object. The hoist must not make that WORSE, and it easily could:
;;; a raw position comes out of the DECLARED structure's layout, and another
;;; structure maps the same slot index to a different raw position, so hoisting
;;; on a version match alone would read a neighbouring slot instead. The fetch
;;; compares the structure name for exactly this reason.
;;;
;;; So the test is not that it signals -- it does not, and did not before --
;;; but that the hoisted read answers what the un-hoisted read of the same
;;; object answers. %SRH-UNDECLARED is the reference: no declaration, so no
;;; hoist, so today's path.

(defun %srh-wrong-type (s)
  (declare (type srh s) (optimize (speed 3) (safety 0) (debug 0)))
  (srh-a s))

(defun %srh-undeclared (s)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (srh-a s))

(deftest struct-raw-backing-hoist.false-declaration-reads-what-it-read-before
  (let ((other (make-srh-mix :w 1 :q :here :r 3 :s 4)))
    (list (equal (%srh-wrong-type other) (%srh-undeclared other))
          (%srh-wrong-type other)))
  (t 1))

;; The same question for a structure with no raw storage at all.
(deftest struct-raw-backing-hoist.false-declaration-no-raw-storage
  (let ((other (make-srh-none :x 11 :y 22)))
    (list (equal (%srh-wrong-type other) (%srh-undeclared other))
          (%srh-wrong-type other)))
  (t 11))

;;; ---- an instance whose raw storage was dropped ----
;;;
;;; A slot value that contradicts its declared :TYPE at construction makes the
;;; instance keep its values boxed and carry no raw array. Such an instance is
;;; perfectly usable, so the hoist must fall back rather than signal, and the
;;; value must come back unchanged. (SAFETY 0) is what lets the value through
;;; the constructor in the first place.
;;;
;;; Compiled-only, and not because of an assertion about emitted code. Getting
;;; the value past the constructor at all depends on (SAFETY 0) compiling the
;;; slot's type check away, which is a thing the compiler does; the tree-walk
;;; interpreter runs the check and signals, correctly for it. So this shape
;;; cannot be built there, and the hoist it is about does not exist there
;;; either.

(defun %srh-make-bad (v)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (make-srh :a v :b 2))

(defun %srh-read-bad (s)
  (declare (type srh s) (optimize (speed 3) (safety 0) (debug 0)))
  (srh-a s))

(deftest-compiled-only struct-raw-backing-hoist.degraded-instance-still-reads
  (let ((s (%srh-make-bad :not-a-fixnum)))
    (list (%srh-read-bad s) (srh-b s)))
  (:not-a-fixnum 2))

;;; ---- evaluation order ----

(defparameter *srh-order* '())

(defun %srh-note (tag x) (push tag *srh-order*) x)

(defun %srh-ordered (s)
  (declare (type srh s) (optimize (speed 3) (safety 0) (debug 0)))
  (setf (srh-a s) (the fixnum (%srh-note :value 5))))

(deftest struct-raw-backing-hoist.store-evaluates-value-once
  (let ((s (make-srh :a 0 :b 0)))
    (setq *srh-order* '())
    (list (%srh-ordered s) (srh-a s) (reverse *srh-order*)))
  (5 5 (:value)))

;;; ---- emitted code ----

;; The fetch happens once, and the per-access calls are gone.
(deftest-emitting-only struct-raw-backing-hoist.fetched-once
  (let ((r (%srh-sil #'%srh-rmw)))
    (list (%srh-count "Runtime.StructRawBacking" r)
          (%srh-count "LDELEM-I8" r)
          (%srh-count "STELEM-I8" r)))
  (1 2 1))

;; The excluded shapes emit no fetch at all.
(deftest-emitting-only struct-raw-backing-hoist.exclusions-emit-no-fetch
  (list (%srh-count "Runtime.StructRawBacking" (%srh-sil #'%srh-setq))
        (%srh-count "Runtime.StructRawBacking" (%srh-sil #'%srh-captured))
        (%srh-count "Runtime.StructRawBacking" (%srh-sil #'%srh-none)))
  (0 0 0))

;; The boxed slot of a mixed structure keeps the ordinary path; only the raw
;; ones become element accesses. %SRH-MIX-RAW reads the three raw slots in a
;; fixnum context and the boxed one generically.
(deftest-emitting-only struct-raw-backing-hoist.boxed-slot-keeps-the-old-path
  (let ((r (%srh-sil #'%srh-mix-raw)))
    (list (%srh-count "Runtime.StructRawBacking" r)
          (%srh-count "LDELEM-I8" r)
          (%srh-count "Runtime.StructRefI" r)))
  (1 3 1))

;;; ---- the scope this does NOT cover ----
;;;
;;; The hoist rewrites a slot access only where the slot is already being read
;;; or written as a raw machine value, which is where the compiler asks for a
;;; long or a double. A read whose value is wanted as an object -- passed to a
;;; function, consed, returned -- still goes through Runtime.StructRefI and
;;; pays the whole per-access chain, because the boxed path was never rewritten
;;; here. That is a real limitation and not an accident, so it is pinned:
;;; if someone extends the hoist to the boxed path, this test is what tells
;;; them the pin is now wrong rather than leaving the old shape untested.
(deftest-emitting-only struct-raw-backing-hoist.generic-context-is-not-hoisted
  (let ((r (%srh-sil #'%srh-mix-read)))
    (list (%srh-count "LDELEM-I8" r)
          (%srh-count "Runtime.StructRefI" r)))
  (0 4))

(setf dotcl:*save-sil* nil)
