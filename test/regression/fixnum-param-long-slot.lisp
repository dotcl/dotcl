;;; A FIXNUM-declared required parameter gets a raw Int64 slot.
;;;
;;; A FIXNUM declaration on a LET binding already buys the binding an Int64
;;; slot: the value is stored raw once and every read in the body skips an
;;; unbox. The same declaration on a PARAMETER bought nothing -- the argument
;;; arrives as a LispObject and the slot kept it, so every native read unboxed
;;; again. When the parameter is a loop bound, that is once per iteration, and
;;; a DO end test cannot hoist it the way DOTIMES hoists its limit, because the
;;; end test is arbitrary user code.
;;;
;;; The declaration is the same promise in both places, so the parameter now
;;; gets the same representation: unboxed once on entry, read raw after.
;;;
;;; What the tests pin: which parameters qualify, that the unboxing happens in
;;; the prologue and nowhere else, that the disqualified shapes (captured,
;;; boxed, special, and parameters with no native integer use at all) keep the
;;; boxed slot, that a tail self-call still rebinds them correctly, and that the
;;; values are unchanged including outside the small-integer cache.

(setf dotcl:*save-sil* t)

(defun %fpl-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %fpl-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;; The slot type of the first parameter. A direct body opens with that
;; parameter's (DECLARE-LOCAL <key> <type>), so the first one in the printed
;; instruction list is it.
(defun %fpl-first-slot-type (s)
  (let* ((p (+ (search "DECLARE-LOCAL " s) 14))
         (q (position #\Space s :start p))
         (e (position #\) s :start q)))
    (subseq s (1+ q) e)))

;;; ---- the shapes ----

;; The DO from the parallel-step work: the end test reads N once an iteration.
(defun %fpl-sum-do (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (do ((i 0 (1+ i)) (s 0 (+ s (logand i 255))))
      ((>= i n) s)
    (declare (fixnum i s))))

;; Same loop with the bound copied into a local by hand -- what users had to
;; write to get the native comparison. It must now compile to the same thing.
(defun %fpl-sum-hoisted (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((lim n))
    (declare (fixnum lim))
    (do ((i 0 (1+ i)) (s 0 (+ s (logand i 255))))
        ((>= i lim) s)
      (declare (fixnum i s)))))

;;; Disqualified shapes.

;; Captured by a closure: the environment stores objects, so the slot has to
;; hold one. (Not mutated, so it is not boxed either -- capture alone is
;; enough to rule out a native slot.)
(defun %fpl-captured (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (list (lambda () n) (< n 5)))

;; Captured AND mutated: the slot holds the box cell.
(defun %fpl-boxed (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((f (lambda () (setq n (+ n 1)))))
    (funcall f)
    (< n 5)))

(defvar *fpl-special* 0)

;; A special parameter is bound on the dynamic stack, not in the slot.
(defun %fpl-special-param (*fpl-special*)
  (declare (fixnum *fpl-special*) (optimize (speed 3) (safety 0) (debug 0)))
  (< *fpl-special* 5))

;; Never read in a native integer position: a raw slot would only add a
;; Fixnum.Make to each of these reads.
(defun %fpl-generic-only (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (list n n))

;; No declaration at all.
(defun %fpl-undeclared (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (list n (< n 5)))

;; Tail self-call rebinding two promoted parameters. The result is a list, so
;; this is NOT the all-fixnum native entry point -- the arguments are evaluated
;; as LispObjects and the tail store has to put them back in the raw slots.
(defun %fpl-tco (n acc)
  (declare (fixnum n acc) (optimize (speed 3) (safety 0) (debug 0)))
  (if (<= n 0) (list acc) (%fpl-tco (- n 1) (+ acc n))))

;; Optional parameters do not take the direct all-required path at all.
(defun %fpl-optional (n &optional (m 3))
  (declare (fixnum n m) (optimize (speed 3) (safety 0) (debug 0)))
  (+ n m))

;;; ---- SIL shape ----
;;;
;;; DEFTEST-EMITTING-ONLY, not DEFTEST: an emit-free build stores no SIL, so
;;; FUNCTION-SIL answers NIL there and every count taken from it is 0. That
;;; makes an assertion of "this instruction is gone" pass for the wrong reason
;;; and an assertion of "this slot is native" fail for the wrong reason. The
;;; values below are the part that is a statement about the language, and they
;;; keep running everywhere.

(deftest-emitting-only fixnum-param-long-slot.do-bound-is-native
  (%fpl-first-slot-type (%fpl-sil #'%fpl-sum-do))
  "Int64")

;; The only unbox left is the one in the prologue: the loop itself has none.
(deftest-emitting-only fixnum-param-long-slot.unbox-only-in-prologue
  (let ((d (%fpl-sil #'%fpl-sum-do)))
    (list (%fpl-count "UNBOX-FIXNUM" d)
          (< (search "UNBOX-FIXNUM" d) (search "TCOLOOP" d))))
  (1 t))

;; Writing the hoist by hand now buys nothing: same unbox count, same boxing.
(deftest-emitting-only fixnum-param-long-slot.hoisting-by-hand-is-redundant
  (let ((a (%fpl-sil #'%fpl-sum-do))
        (b (%fpl-sil #'%fpl-sum-hoisted)))
    (list (= (%fpl-count "UNBOX-FIXNUM" a) (%fpl-count "UNBOX-FIXNUM" b))
          (= (%fpl-count "Fixnum.Make" a) (%fpl-count "Fixnum.Make" b))))
  (t t))

(deftest-emitting-only fixnum-param-long-slot.captured-stays-boxed
  (list (%fpl-first-slot-type (%fpl-sil #'%fpl-captured))
        (%fpl-first-slot-type (%fpl-sil #'%fpl-boxed)))
  ("LispObject" "LispObject[]"))

(deftest-emitting-only fixnum-param-long-slot.special-param-stays-object
  (%fpl-first-slot-type (%fpl-sil #'%fpl-special-param))
  "LispObject")

(deftest-emitting-only fixnum-param-long-slot.generic-only-stays-object
  (list (%fpl-first-slot-type (%fpl-sil #'%fpl-generic-only))
        (%fpl-first-slot-type (%fpl-sil #'%fpl-undeclared)))
  ("LispObject" "LispObject"))

(deftest-emitting-only fixnum-param-long-slot.optional-params-unaffected
  (%fpl-first-slot-type (%fpl-sil #'%fpl-optional))
  "LispObject")

;; Both parameters are raw, and the tail store restores that representation.
(deftest-emitting-only fixnum-param-long-slot.tco-params-are-native
  (let ((d (%fpl-sil #'%fpl-tco)))
    (list (%fpl-first-slot-type d)
          (%fpl-count "Runtime.SubtractFixnum" d)
          (%fpl-count "Runtime.AddFixnum" d)))
  ("Int64" 1 1))

;;; ---- values ----

(deftest fixnum-param-long-slot.do-values
  (list (%fpl-sum-do 0) (%fpl-sum-do 100) (%fpl-sum-hoisted 100))
  (0 4950 4950))

;; Past the small-integer cache in the counter and in the result.
(deftest fixnum-param-long-slot.do-value-large
  (list (%fpl-sum-do 1000000) (%fpl-sum-hoisted 1000000))
  (127493856 127493856))

(deftest fixnum-param-long-slot.tco-values
  (list (%fpl-tco 10 0) (%fpl-tco 1000000 0))
  ((55) (500000500000)))

(deftest fixnum-param-long-slot.excluded-shapes-still-work
  (list (funcall (first (%fpl-captured 3)))
        (second (%fpl-captured 3))
        (%fpl-boxed 1)
        (let ((*fpl-special* 99)) (list (%fpl-special-param 2) *fpl-special*))
        (%fpl-generic-only 7)
        (%fpl-optional 5)
        (%fpl-optional 5 100))
  (3 t t (t 99) (7 7) 8 105))

;; A promoted parameter read in a generic position boxes on the way out, and
;; the value has to survive that round trip unchanged well past the cache.
(defun %fpl-round-trip (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (list (< n 0) n (- n 1)))

(deftest fixnum-param-long-slot.generic-read-round-trip
  (list (%fpl-round-trip 1000000000000) (%fpl-round-trip -1000000000000))
  ((nil 1000000000000 999999999999)
   (t -1000000000000 -1000000000001)))

;; The entry unbox is the only place the declaration is enforced, and an int64
;; overflow in the body still promotes rather than wrapping.
(deftest fixnum-param-long-slot.overflow-still-promotes
  (%fpl-round-trip -9223372036854775808)
  (t -9223372036854775808 -9223372036854775809))

(setf dotcl:*save-sil* nil)
