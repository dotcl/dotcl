;;; A structure slot declared to hold a bounded integer carries its range into
;;; EXPR-INT-RANGE, not only into FIXNUM-TYPED-P.
;;;
;;; Three places decide whether an integer expression can be computed in raw
;;; int64 -- FIXNUM-TYPED-P, EXPR-INT-RANGE and COMPILE-AS-LONG -- and a leaf
;;; has to be in all three. The structure slot was in two. FIXNUM-TYPED-P said
;;; the slot is an integer and COMPILE-AS-LONG knew how to read one raw, but
;;; FIXNUM-LEAF-RANGE, which is how EXPR-INT-RANGE handles a leaf, did not
;;; know the slot at all, so the range came back NIL and everything gated on a
;;; range refused at once: the subscript of an AREF, 1+ and 1-, and the
;;; arithmetic operators.
;;;
;;; WRITE THESE BARE. The THE clause in FIXNUM-LEAF-RANGE hands back the full
;;; int64 range for (THE FIXNUM E) whatever E is, so (THE FIXNUM (ACC R))
;;; proved before this and proves after it, and pins nothing. Every form below
;;; reads the slot with no declaration around it.
;;;
;;; Note which slot each test uses. A slot declared FIXNUM gets the full int64
;;; range, which is enough for a subscript but NOT enough for 1+ or 1-: the
;;; range algebra widens it by one and it no longer fits. A slot declared with
;;; real bounds, (SIGNED-BYTE 32) here, proves those too. That difference is
;;; the reason the clause answers the declared type rather than FIXNUM.

(setf dotcl:*save-sil* t)

(defun %flr-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %flr-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defstruct flr
  (head 0 :type fixnum)
  (small 0 :type (signed-byte 32))
  (items (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*))))

;;; ---- the shapes ----

;; The subscript. This is the one the slot's full fixnum range is enough for.
(defun %flr-peek (r)
  (declare (type flr r) (optimize (speed 3) (safety 0) (debug 0)))
  (aref (flr-items r) (flr-head r)))

;; Arithmetic on a bounded slot: [-2^31, 2^31-1] times 3 still fits int64.
(defun %flr-mul (r)
  (declare (type flr r) (optimize (speed 3) (safety 0) (debug 0)))
  (* 3 (flr-small r)))

(defun %flr-add (r)
  (declare (type flr r) (optimize (speed 3) (safety 0) (debug 0)))
  (+ (flr-small r) (flr-small r)))

;; 1- of a bounded slot proves; 1- of a FIXNUM one cannot, because the range
;; algebra widens int64 by one.
(defun %flr-dec-small (r)
  (declare (type flr r) (optimize (speed 3) (safety 0) (debug 0)))
  (1- (flr-small r)))

(defun %flr-dec-head (r)
  (declare (type flr r) (optimize (speed 3) (safety 0) (debug 0)))
  (1- (flr-head r)))

;; An untyped slot is not an integer leaf at all and must stay generic.
(defstruct flr-any x)

(defun %flr-any (r)
  (declare (type flr-any r) (optimize (speed 3) (safety 0) (debug 0)))
  (aref (vector 1 2 3) (flr-any-x r)))

;;; ---- values ----
;;;
;;; The point of the change is the emitted code, but a range proof that is
;;; wrong shows up as a wrong answer, so the values come first.

(deftest fixnum-leaf-range-struct-slot.peek
  (let ((r (make-flr :head 2 :items (make-array 4 :element-type 'fixnum
                                                :initial-contents '(10 11 12 13)))))
    (%flr-peek r))
  12)

(deftest fixnum-leaf-range-struct-slot.mul
  (%flr-mul (make-flr :small 7))
  21)

(deftest fixnum-leaf-range-struct-slot.mul-negative
  (%flr-mul (make-flr :small -7))
  -21)

;; The bounds themselves: the widest values the declaration allows must still
;; answer correctly, since the range proof is what licenses the raw path.
(deftest fixnum-leaf-range-struct-slot.mul-at-the-bounds
  (list (%flr-mul (make-flr :small 2147483647))
        (%flr-mul (make-flr :small -2147483648)))
  (6442450941 -6442450944))

(deftest fixnum-leaf-range-struct-slot.add
  (%flr-add (make-flr :small 2147483647))
  4294967294)

(deftest fixnum-leaf-range-struct-slot.dec
  (list (%flr-dec-small (make-flr :small 0))
        (%flr-dec-small (make-flr :small -2147483648)))
  (-1 -2147483649))

(deftest fixnum-leaf-range-struct-slot.untyped-slot-still-works
  (%flr-any (make-flr-any :x 1))
  2)

;;; ---- emitted code ----
;;;
;;; The needles carry the closing paren, because the generic entry's name is a
;;; PREFIX of the specialised one: "Runtime.Multiply" also counts every
;;; "Runtime.MultiplyFixnum", and the first version of these tests passed for
;;; that reason while asserting nothing. PRINC-TO-STRING is what these read,
;;; and it prints the operand without its quotes, so the paren and not a quote
;;; is what ends the name. Each test names both the entry that should be gone
;;; and the one that should have replaced it, so neither can pass by the
;;; needle simply missing.

;; The subscript reads the slot raw instead of reading it boxed and unboxing
;; it. The boxed read also allocates unless the value is in the small-integer
;; cache, so this removes an allocation per read as well as an instruction.
;;
;; %FLR-PEEK reads TWO slots and only one of them changes. ITEMS holds an
;; array, is not an integer leaf, and is still read with StructRefI -- so the
;; expected counts are one of each, not one and zero. That the other read did
;; NOT move is half of what this pins: the clause answers for the slots whose
;; declared type is a bounded integer and leaves the rest alone.
(deftest-emitting-only fixnum-leaf-range-struct-slot.subscript-is-raw
  (let ((s (%flr-sil #'%flr-peek)))
    (list (%flr-count "Runtime.StructRefL)" s)     ; the integer subscript
          (%flr-count "Runtime.StructRefI)" s)     ; the array slot, unchanged
          (%flr-count "UNBOX-FIXNUM" s)))
  (1 1 0))

;; A bounded slot in arithmetic reaches the fixnum entries instead of the
;; generic ones.
(deftest-emitting-only fixnum-leaf-range-struct-slot.arithmetic-is-fixnum-typed
  (let ((m (%flr-sil #'%flr-mul))
        (a (%flr-sil #'%flr-add)))
    (list (%flr-count "Runtime.MultiplyFixnum)" m)
          (%flr-count "Runtime.Multiply)" m)
          (%flr-count "Runtime.AddFixnum)" a)
          (%flr-count "Runtime.Add)" a)))
  (1 0 1 0))

;; 1- of a BOUNDED slot proves and goes native.
(deftest-emitting-only fixnum-leaf-range-struct-slot.decrement-of-bounded-slot-is-native
  (let ((s (%flr-sil #'%flr-dec-small)))
    (list (%flr-count "Runtime.Decrement)" s)
          (%flr-count "Runtime.StructRefL)" s)))
  (0 1))

;; 1- of a FIXNUM slot does not, and is expected not to: the range algebra
;; widens the full int64 range by one and it no longer fits. This is the
;; limit that makes the clause answer the DECLARED type rather than FIXNUM --
;; if it answered FIXNUM for everything, the test above would fail too.
(deftest-emitting-only fixnum-leaf-range-struct-slot.decrement-of-fixnum-slot-stays-generic
  (plusp (%flr-count "Runtime.Decrement)" (%flr-sil #'%flr-dec-head)))
  t)

;; An untyped slot proves nothing and keeps the generic path, which is what
;; says the clause is reading the declaration rather than assuming.
(deftest-emitting-only fixnum-leaf-range-struct-slot.untyped-slot-stays-generic
  (plusp (%flr-count "Runtime.StructRefI)" (%flr-sil #'%flr-any)))
  t)

(setf dotcl:*save-sil* nil)
