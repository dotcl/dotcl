;;; AREF through a structure accessor reads the element unboxed.
;;;
;;; A slot declared (simple-array fixnum (*)) says exactly which backing the
;;; array it holds has, but that fact only reached AREF when the array was
;;; first bound to a declared local: (AREF A I) on such a local rode the raw
;;; element path while (AREF (ACC S) I) on the same array stayed on the generic
;;; Runtime.ArefL / Runtime.ArefSetL, boxing the element on the way out and
;;; unboxing it again at the use. Writing (LET ((A (ACC S))) ...) by hand was
;;; enough to get the fast path, which is the sign that the type simply was not
;;; being carried, not that anything about the array was different.
;;;
;;; The element buffer is NOT hoisted for this shape: hoisting requires a plain
;;; local whose binding pins the array for the whole body, and a slot can be
;;; assigned a different array between two reads. So the read stays a
;;; Runtime.ArefNum*L call, and the declaration stays a hint the runtime
;;; re-checks -- a slot whose contents contradict it still reads correctly,
;;; which the last tests here pin.

(setf dotcl:*save-sil* t)

(defun %ssa-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %ssa-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- the structure ----

(defstruct ssa
  (fix (make-array 0 :element-type 'fixnum)
       :type (simple-array fixnum (*)))
  (byt (make-array 0 :element-type '(unsigned-byte 8))
       :type (simple-array (unsigned-byte 8) (*)))
  (dbl (make-array 0 :element-type 'double-float)
       :type (simple-array double-float (*)))
  ;; T has no unboxed backing, so this one must stay on the generic path.
  (gen (make-array 0) :type (simple-array t (*)))
  ;; No :TYPE at all: nothing is known about the slot, generic path.
  (any (make-array 0)))

;;; ---- the shapes ----

(defun %ssa-get (s i)
  (declare (type ssa s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (aref (ssa-fix s) i)))

(defun %ssa-set (s i v)
  (declare (type ssa s) (fixnum i v) (optimize (speed 3) (safety 0) (debug 0)))
  (setf (aref (ssa-fix s) i) v))

(defun %ssa-byt (s i)
  (declare (type ssa s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (aref (ssa-byt s) i)))

(defun %ssa-byt-set (s i v)
  (declare (type ssa s) (fixnum i v) (optimize (speed 3) (safety 0) (debug 0)))
  (setf (aref (ssa-byt s) i) v))

(defun %ssa-dbl (s i)
  (declare (type ssa s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (the double-float (aref (ssa-dbl s) i)))

(defun %ssa-gen (s i)
  (declare (type ssa s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (aref (ssa-gen s) i))

(defun %ssa-any (s i)
  (declare (type ssa s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (aref (ssa-any s) i))

;; The element feeds arithmetic rather than being returned, so on the raw path
;; nothing around it is boxed at all: this is where the shape pays.
(defun %ssa-sum (s n)
  (declare (type ssa s) (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (do ((i 0 (the fixnum (1+ i)))) ((>= i n) acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (the fixnum (aref (ssa-fix s) i))))))))

;; The same read written the way that already worked. Both must answer alike.
(defun %ssa-get-via-local (s i)
  (declare (type ssa s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((a (ssa-fix s)))
    (declare (type (simple-array fixnum (*)) a))
    (the fixnum (aref a i))))

;;; ---- evaluation order ----
;;;
;;; The array operand is a call now, not a variable, so it can no longer be
;;; loaded after the subscript temps the way a pure local could: CLHS 5.1.1.1
;;; evaluates the subexpressions of a place left to right.

(defparameter *ssa-order* '())

(defun %ssa-note (tag x)
  (push tag *ssa-order*)
  x)

(defun %ssa-ordered-get (s i)
  (declare (type ssa s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (aref (ssa-fix (%ssa-note :array s))
                    (the fixnum (%ssa-note :index i)))))

(defun %ssa-ordered-set (s i v)
  (declare (type ssa s) (fixnum i v) (optimize (speed 3) (safety 0) (debug 0)))
  (setf (aref (ssa-fix (%ssa-note :array s))
              (the fixnum (%ssa-note :index i)))
        (the fixnum (%ssa-note :value v))))

;;; ---- a slot whose contents contradict its declaration ----
;;;
;;; The store-time check of a slot's :TYPE compiles away under (safety 0), so
;;; this is how a declaration and the value in the slot come apart. What the
;;; declaration buys must stay a hint: the runtime re-checks the backing and
;;; falls back, so the read answers correctly rather than signalling.

(defun %ssa-lie (v)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((s (make-ssa)))
    (setf (ssa-fix s) v)
    s))

;;; ---- inputs ----

(defparameter *ssa*
  (make-ssa :fix (let ((a (make-array 4 :element-type 'fixnum :initial-element 0)))
                   (dotimes (i 4 a) (setf (aref a i) (* i 10))))
            :byt (let ((a (make-array 3 :element-type '(unsigned-byte 8)
                                        :initial-element 0)))
                   (dotimes (i 3 a) (setf (aref a i) (+ 250 i))))
            :dbl (let ((a (make-array 2 :element-type 'double-float
                                        :initial-element 0d0)))
                   (setf (aref a 0) 1.5d0)
                   (setf (aref a 1) -2.25d0)
                   a)
            :gen (make-array 2 :initial-contents (list 7 8))
            :any (make-array 2 :initial-contents (list :a :b))))

;;; ---- values ----

(deftest struct-slot-array-aref.read
  (list (%ssa-get *ssa* 0) (%ssa-get *ssa* 3) (%ssa-get-via-local *ssa* 3))
  (0 30 30))

(deftest struct-slot-array-aref.write
  (let ((s (make-ssa :fix (make-array 3 :element-type 'fixnum :initial-element 0))))
    (list (%ssa-set s 1 1000000007)
          (%ssa-get s 1)
          (%ssa-get s 0)))
  (1000000007 1000000007 0))

(deftest struct-slot-array-aref.unsigned-byte
  (let ((s (make-ssa :byt (make-array 2 :element-type '(unsigned-byte 8)
                                        :initial-element 0))))
    (list (%ssa-byt *ssa* 0) (%ssa-byt *ssa* 2)
          (%ssa-byt-set s 1 255) (%ssa-byt s 1)))
  (250 252 255 255))

;; The opcode for a narrow store narrows silently; an out-of-width value must
;; still be the error the boxed store path makes it.
(deftest struct-slot-array-aref.unsigned-byte-overflow
  (let ((s (make-ssa :byt (make-array 2 :element-type '(unsigned-byte 8)
                                        :initial-element 0))))
    (handler-case (progn (%ssa-byt-set s 0 256) :no-error)
      (error () :error)))
  :error)

(deftest struct-slot-array-aref.double
  (list (%ssa-dbl *ssa* 0) (%ssa-dbl *ssa* 1))
  (1.5d0 -2.25d0))

(deftest struct-slot-array-aref.generic-slots
  (list (%ssa-gen *ssa* 1) (%ssa-any *ssa* 0))
  (8 :a))

(deftest struct-slot-array-aref.sum
  (list (%ssa-sum *ssa* 4) (%ssa-sum *ssa* 0))
  (60 0))

;; Out of range is an error on both shapes, and the same one: the raw path
;; range-checks before it reads and hands the subscript to the generic entry.
(deftest struct-slot-array-aref.out-of-range-read
  (list (handler-case (progn (%ssa-get *ssa* 4) :no-error)
          (type-error () :type-error)
          (error () :other-error))
        (handler-case (progn (%ssa-get-via-local *ssa* 4) :no-error)
          (type-error () :type-error)
          (error () :other-error))
        ;; The generic path, which is also what an emit-free build runs for
        ;; all three: the answer has to be the same one.
        (handler-case (progn (%ssa-any *ssa* 4) :no-error)
          (type-error () :type-error)
          (error () :other-error)))
  (:type-error :type-error :type-error))

(deftest struct-slot-array-aref.out-of-range-write
  (let ((s (make-ssa :fix (make-array 2 :element-type 'fixnum :initial-element 0))))
    (list (handler-case (progn (%ssa-set s 2 1) :no-error)
            (type-error () :type-error)
            (error () :other-error))
          (%ssa-get s 0)))
  (:type-error 0))

(deftest struct-slot-array-aref.negative-index
  (list (handler-case (progn (%ssa-get *ssa* -1) :no-error)
          (type-error () :type-error)
          (error () :other-error))
        (handler-case (progn (%ssa-any *ssa* -1) :no-error)
          (type-error () :type-error)
          (error () :other-error)))
  (:type-error :type-error))

;; The declaration is a hint the runtime re-checks, not a proof it acts on: a
;; slot holding an element-type T vector still reads and writes the right
;; values through the accessor.
;;
;; %SSA-LIE stores the vector at (safety 0), where the slot's :type is not
;; checked by either evaluator (struct-slot-type-safety.lisp), so these run on
;; an emit-free build too.
(deftest struct-slot-array-aref.declaration-not-honored-still-reads
  (let ((s (%ssa-lie (make-array 3 :initial-contents (list 7 8 9)))))
    (list (%ssa-get s 0) (%ssa-get s 2) (%ssa-sum s 3)))
  (7 9 24))

(deftest struct-slot-array-aref.declaration-not-honored-still-writes
  (let ((s (%ssa-lie (make-array 2 :initial-contents (list 0 0)))))
    (list (%ssa-set s 1 1000000007) (%ssa-get s 1)))
  (1000000007 1000000007))

;; An element too wide to be a fixnum in a slot declared to hold fixnums. The
;; fallback reads the element and then has to answer with the int64 the
;; declaration promised, which this element is not, so the read signals rather
;; than handing back a value nothing downstream could use. Signalling is the
;; whole of the change here: the generic path returned the bignum, but the
;; program that put it there had already broken its own declaration, and what a
;; violated declaration does is undefined (CLHS 3.3.1). A declared local
;; answers the same way, one step earlier -- see declared-array-native-aref.
;;
;; DEFTEST-EMITTING-ONLY: an interpreter is free to ignore the declaration, and
;; the emit-free build does, so there it reads the bignum out and nothing is
;; wrong with that.
(deftest-emitting-only struct-slot-array-aref.declaration-not-honored-wide-value
  (list (handler-case (progn (%ssa-get (%ssa-lie (vector (expt 2 100))) 0) :no-error)
          (error () :error))
        ;; Not the array's fault: the same slot with fixnum elements is fine.
        (%ssa-get (%ssa-lie (vector 5)) 0))
  (:error 5))

(deftest struct-slot-array-aref.order-of-evaluation-read
  (let ((*ssa-order* '()))
    (list (%ssa-ordered-get *ssa* 2) (reverse *ssa-order*)))
  (20 (:array :index)))

(deftest struct-slot-array-aref.order-of-evaluation-write
  (let ((*ssa-order* '())
        (s (make-ssa :fix (make-array 3 :element-type 'fixnum :initial-element 0))))
    (list (%ssa-ordered-set s 2 42) (%ssa-get s 2) (reverse *ssa-order*)))
  (42 42 (:array :index :value)))

;;; ---- SIL shape ----
;;;
;;; DEFTEST-EMITTING-ONLY: an emit-free build stores no SIL, so every count
;;; taken from FUNCTION-SIL would be 0 for the wrong reason.

;; The read is the raw element helper, and no buffer is hoisted: a slot is not
;; a binding, so nothing pins the array across two reads.
(deftest-emitting-only struct-slot-array-aref.read-is-raw
  (let ((d (%ssa-sil #'%ssa-get)))
    (list (%ssa-count "Runtime.ArefNumL" d)
          (%ssa-count "Runtime.ArefL" d)
          (%ssa-count "Runtime.BackingI64" d)))
  (1 0 0))

(deftest-emitting-only struct-slot-array-aref.write-is-raw
  (let ((d (%ssa-sil #'%ssa-set)))
    (list (%ssa-count "Runtime.ArefSetNumL" d)
          (%ssa-count "Runtime.ArefSetL" d)
          (%ssa-count "Runtime.BackingI64" d)))
  (1 0 0))

(deftest-emitting-only struct-slot-array-aref.unsigned-byte-is-raw
  (let ((r (%ssa-sil #'%ssa-byt))
        (w (%ssa-sil #'%ssa-byt-set)))
    (list (%ssa-count "Runtime.ArefNumL" r)
          (%ssa-count "Runtime.ArefL" r)
          (%ssa-count "Runtime.ArefSetNumL" w)
          (%ssa-count "Runtime.ArefSetL" w)))
  (1 0 1 0))

(deftest-emitting-only struct-slot-array-aref.double-is-raw
  (let ((d (%ssa-sil #'%ssa-dbl)))
    (list (%ssa-count "Runtime.ArefNumD" d)
          (%ssa-count "Runtime.ArefL" d)))
  (1 0))

;; Nothing is claimed about an element type with no unboxed backing, and
;; nothing at all is claimed about a slot with no :TYPE.
(deftest-emitting-only struct-slot-array-aref.generic-slots-stay-generic
  (let ((g (%ssa-sil #'%ssa-gen))
        (a (%ssa-sil #'%ssa-any)))
    (list (%ssa-count "Runtime.ArefNumL" g)
          (%ssa-count "Runtime.ArefL" g)
          (%ssa-count "Runtime.ArefNumL" a)
          (%ssa-count "Runtime.ArefL" a)))
  (0 1 0 1))

;; The accumulating loop is the payoff: the element never becomes an object,
;; so the only Fixnum.Make left is the box of the returned ACC.
(deftest-emitting-only struct-slot-array-aref.sum-boxes-once
  (let ((d (%ssa-sil #'%ssa-sum)))
    (list (%ssa-count "Runtime.ArefNumL" d)
          (%ssa-count "Runtime.ArefL" d)
          (%ssa-count "Fixnum.Make" d)))
  (1 0 1))

(setf dotcl:*save-sil* nil)
