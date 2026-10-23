;;; A loop over a structure's raw slots tests the hoisted raw array once, before
;;; the loop, and runs one of two copies of the body.
;;;
;;; The raw slot array of a binding declared to hold a structure is fetched once
;;; (see struct-raw-backing-hoist.lisp). Each access used to carry its own test
;;; of that array: present or not, and then the element load with its bounds
;;; check. Inside a loop that test is the same every time, and leaving it at
;;; the access kept the fallback call in the loop body as well, which is what
;;; pushed the loop counter out of its register. Now a body that reaches the
;;; raw slots from inside a loop is emitted twice behind one test, done once:
;;; the array is there and is as long as the layout says. The first copy does
;;; bare element loads and stores. The second copy is the code from before,
;;; per-access test and all, and runs when the array is missing.
;;;
;;; So every test here comes in pairs: the same loop, once on an instance that
;;; takes the fast copy and once on one that cannot, and both must answer the
;;; same. The second copy is reached by a false declaration (another structure
;;; at the same slot indices), by an instance that dropped its raw storage at
;;; construction, by a redefined structure, and by another hoisted array in the
;;; same body being missing -- the copies are chosen for all hoisted buffers
;;; together.

(setf dotcl:*save-sil* t)

(defun %sru-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %sru-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- the structures ----

(defstruct sru (a 0 :type fixnum) (b 0 :type fixnum))

;; Same slot indices as SRU but another structure, so the fetch answers NIL.
(defstruct sru-other (a 0 :type fixnum) (b 0 :type fixnum))

;; Every raw kind, with a boxed slot in the middle so that a raw position is
;; not the slot index.
(defstruct sru-kinds
  (i 0 :type fixnum)
  (tag nil)
  (d 0d0 :type double-float)
  (f 0f0 :type single-float))

;;; ---- the loops ----

;; The struct-slots kernel of bench/csharp-parity.
(defun %sru-rmw (p n)
  (declare (type sru p) (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (do ((i 0 (the fixnum (1+ i))))
        ((>= i n) acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (the fixnum (sru-a p)) (the fixnum (sru-b p)))))
      (setf (sru-a p) i))))

;; The reference: the same loop with no declaration, so no hoist at all.
(defun %sru-rmw-plain (p n)
  (let ((acc 0))
    (do ((i 0 (1+ i)))
        ((>= i n) acc)
      (setq acc (+ acc (sru-a p) (sru-b p)))
      (setf (sru-a p) i))))

;; All three raw kinds read and written in the loop, the boxed slot read too.
(defun %sru-kinds (k n)
  (declare (type sru-kinds k) (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((tags '()))
    (dotimes (j n)
      (setf (sru-kinds-i k) (the fixnum (+ (the fixnum (sru-kinds-i k)) j)))
      (setf (sru-kinds-d k) (the double-float (+ (the double-float (sru-kinds-d k)) 0.5d0)))
      (setf (sru-kinds-f k) (the single-float (+ (the single-float (sru-kinds-f k)) 0.25f0)))
      (push (sru-kinds-tag k) tags))
    (list (sru-kinds-i k) (sru-kinds-d k) (sru-kinds-f k) tags)))

;; A structure and an array hoisted in the same body. The two copies are chosen
;; for both together, so an array that cannot be hoisted sends the structure
;; down the slow copy as well, where it must still read right.
(defun %sru-with-array (p v)
  (declare (type sru p) (type (simple-array fixnum (*)) v)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (dotimes (i (length v) acc)
      (setq acc (the fixnum (+ acc (the fixnum (aref v i)) (the fixnum (sru-a p)))))
      (setf (sru-b p) acc))))

;; Two structure bindings: both are tested, and either missing takes the slow
;; copy.
(defun %sru-two (p q n)
  (declare (type sru p q) (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (dotimes (i n)
    (setf (sru-a p) (the fixnum (+ (the fixnum (sru-a p)) (the fixnum (sru-b q))))))
  (list (sru-a p) (sru-b q)))

;; A LET binding rather than a parameter.
(defun %sru-let (x n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((p x) (acc 0))
    (declare (type sru p) (fixnum acc))
    (dotimes (i n acc)
      (setq acc (the fixnum (+ acc (the fixnum (sru-b p)))))
      (setf (sru-b p) i))))

;;; ---- the fast copy ----

(deftest struct-raw-backing-unswitch.fast
  (let ((s (make-sru :a 1 :b 2)))
    (list (%sru-rmw s 4) (sru-a s) (sru-b s)))
  ;; A is 1, 0, 1, 2 at the four reads and B stays 2: 3 + 2 + 3 + 4 = 12.
  (12 3 2))

(deftest struct-raw-backing-unswitch.fast-agrees-with-plain
  (let ((s (make-sru :a 5 :b -3))
        (r (make-sru :a 5 :b -3)))
    (list (= (%sru-rmw s 1000) (%sru-rmw-plain r 1000))
          (= (sru-a s) (sru-a r))))
  (t t))

(deftest struct-raw-backing-unswitch.zero-iterations
  (let ((s (make-sru :a 1 :b 2)))
    (list (%sru-rmw s 0) (sru-a s)))
  (0 1))

(deftest struct-raw-backing-unswitch.kinds
  (%sru-kinds (make-sru-kinds :i 1 :tag :x :d 1d0 :f 1f0) 3)
  (4 2.5d0 1.75f0 (:x :x :x)))

(deftest struct-raw-backing-unswitch.let-binding
  (let ((s (make-sru :a 0 :b 10)))
    (list (%sru-let s 3) (sru-b s)))
  ;; B is 10, 0, 1 at the reads.
  (11 2))

(deftest struct-raw-backing-unswitch.two-bindings
  (%sru-two (make-sru :a 0 :b 0) (make-sru :a 0 :b 3) 4)
  (12 3))

;; The same instance through both bindings: the store through P is seen by the
;; read through Q, because both name the one raw array.
(deftest struct-raw-backing-unswitch.two-bindings-same-instance
  (let ((s (make-sru :a 1 :b 1)))
    (%sru-two s s 3))
  (4 1))

(deftest struct-raw-backing-unswitch.with-array
  (let ((s (make-sru :a 2 :b 0))
        (v (make-array 3 :element-type 'fixnum :initial-contents '(1 2 3))))
    (list (%sru-with-array s v) (sru-b s)))
  (12 12))

;;; ---- the slow copy ----

;; A false declaration: another structure, same slot indices. Undefined
;; behaviour, but what dotcl does with it must not change: the slow copy reads
;; by slot index, as the code before this did.
;; Emitting builds only, as in struct-raw-backing-hoist.lisp: there the accessor
;; is not inlined and its own DEFUN checks the object.
(deftest-emitting-only struct-raw-backing-unswitch.other-structure
  (let ((o (make-sru-other :a 1 :b 2)))
    (list (%sru-rmw o 4) (sru-other-a o) (sru-other-b o)))
  (12 3 2))

;; Two bindings where only one gets its array.
(deftest-emitting-only struct-raw-backing-unswitch.two-bindings-one-missing
  (%sru-two (make-sru :a 0 :b 0) (make-sru-other :a 0 :b 3) 4)
  (12 3))

;; An adjustable vector is not hoisted, so the body runs its slow copy with the
;; structure's array present.
(deftest struct-raw-backing-unswitch.with-array-not-hoisted
  (let ((s (make-sru :a 2 :b 0))
        (v (make-array 3 :element-type 'fixnum :initial-contents '(1 2 3)
                         :adjustable t)))
    (list (%sru-with-array s v) (sru-b s)))
  (12 12))

;; An instance that kept its values boxed because one contradicted the declared
;; type at construction has no raw array. Compiled-only for the reason given in
;; struct-raw-backing-hoist.lisp: only (SAFETY 0) compiled code lets the value
;; past the constructor.
(defun %sru-make-bad (v)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (make-sru :a 1 :b v))

(defun %sru-read-tags (p n)
  (declare (type sru p) (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((seen '()))
    (dotimes (i n)
      (setf (sru-a p) (the fixnum (+ (the fixnum (sru-a p)) 1)))
      (push (sru-b p) seen))
    (list (sru-a p) seen)))

(deftest-compiled-only struct-raw-backing-unswitch.degraded-instance
  (%sru-read-tags (%sru-make-bad :tag) 2)
  (3 (:tag :tag)))

;;; ---- redefinition ----
;;;
;;; Code compiled against the old layout must not read the new one through the
;;; fast copy: the fetch compares the layout version and answers NIL, and the
;;; slow copy is what reports the stale accessor. Code compiled against the new
;;; layout reads the new positions.

(defstruct sru-redef (a 0 :type fixnum) (b 0 :type fixnum))

(defun %sru-redef-loop (p n)
  (declare (type sru-redef p) (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (dotimes (i n acc)
      (setq acc (the fixnum (+ acc (the fixnum (sru-redef-a p)) (the fixnum (sru-redef-b p)))))
      (setf (sru-redef-b p) i))))

(defparameter *sru-before* (%sru-redef-loop (make-sru-redef :a 1 :b 2) 3))

(defstruct sru-redef
  (z 0d0 :type double-float) (q nil) (a 0 :type fixnum) (b 0 :type fixnum))

(deftest-compiled-only struct-raw-backing-unswitch.redefined-old-code-signals
  (list *sru-before*
        (handler-case (progn (%sru-redef-loop (make-sru-redef :a 1 :b 2) 3) :no-error)
          (error () :error)))
  (6 :error))

(defun %sru-redef-loop (p n)
  (declare (type sru-redef p) (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (dotimes (i n acc)
      (setq acc (the fixnum (+ acc (the fixnum (sru-redef-a p)) (the fixnum (sru-redef-b p)))))
      (setf (sru-redef-b p) i))))

(deftest struct-raw-backing-unswitch.redefined-new-code
  (let ((r (make-sru-redef :z 1d0 :q :q :a 1 :b 2)))
    (list (%sru-redef-loop r 3) (sru-redef-z r) (sru-redef-q r) (sru-redef-a r) (sru-redef-b r)))
  (6 1d0 :q 1 2))

;;; ---- emitted code ----

;; One test before the loop (the null test and the length test), and the fast
;; copy's accesses are bare element loads and stores. The calls that remain are
;; the slow copy's, one per access.
(deftest-emitting-only struct-raw-backing-unswitch.tested-once
  (let ((r (%sru-sil #'%sru-rmw)))
    (list (%sru-count "Runtime.StructRawBacking" r)
          (%sru-count "LDLEN" r)
          (%sru-count "LDELEM-I8" r)
          (%sru-count "STELEM-I8" r)
          (%sru-count "Runtime.StructRefL" r)
          (%sru-count "Runtime.StructSetL" r)))
  ;; Fast copy: 2 loads, 1 store. Slow copy: the same behind a test each, plus
  ;; the calls.
  (1 1 4 2 2 1))

(setf dotcl:*save-sil* nil)
