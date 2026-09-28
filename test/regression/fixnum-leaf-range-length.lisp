;;; (LENGTH X) carries its range into EXPR-INT-RANGE, not only into
;;; FIXNUM-TYPED-P.
;;;
;;; The sibling of the structure-slot gap, and the cleaner test of what a leaf
;;; range actually buys: COMPILE-AS-LONG has no LENGTH clause at all, so the
;;; generic Runtime.Length call stays exactly where it was and every effect
;;; here is downstream of the range -- 1+ and 1- going native, and the
;;; arithmetic operators reaching their fixnum entries instead of the generic
;;; ones.
;;;
;;; The bound is (0 . 2147483647). All four returns of Runtime.Length are
;;; Fixnum.Make of a non-negative C# int.
;;;
;;; WRITE THESE BARE. The THE clause in FIXNUM-LEAF-RANGE hands back the full
;;; int64 range for (THE FIXNUM E) whatever E is, and the full range is
;;; precisely what does NOT prove 1+ or 1-: widening it by one leaves int64.
;;; So (1- (THE FIXNUM (LENGTH V))) stays generic while (1- (LENGTH V)) goes
;;; native, and writing the declaration would test the opposite of the point.

(setf dotcl:*save-sil* t)

(defun %flrl-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %flrl-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- the shapes ----

(defun %flrl-dec (v)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (1- (length v)))

(defun %flrl-inc (v)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (1+ (length v)))

(defun %flrl-mul (v)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (* 2 (length v)))

(defun %flrl-add (a b)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (+ (length a) (length b)))

;; The declared form, kept as the other side of the comparison: the full
;; fixnum range THE hands back cannot prove 1-, so this one stays generic.
(defun %flrl-dec-declared (v)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (1- (the fixnum (length v))))

;;; ---- values ----

(deftest fixnum-leaf-range-length.dec
  (list (%flrl-dec '(a b c))
        (%flrl-dec "abcd")
        (%flrl-dec #(1 2 3 4 5))
        (%flrl-dec '()))
  (2 3 4 -1))

(deftest fixnum-leaf-range-length.inc
  (list (%flrl-inc '(a b c)) (%flrl-inc "") (%flrl-inc #()))
  (4 1 1))

(deftest fixnum-leaf-range-length.mul
  (list (%flrl-mul '(a b c)) (%flrl-mul ""))
  (6 0))

(deftest fixnum-leaf-range-length.add
  (list (%flrl-add '(a b) "xyz") (%flrl-add '() #()))
  (5 0))

;; The empty sequence is the low end of the declared range, and 1- of it is
;; the one value that leaves [0, int32max]. The range proof has to have
;; allowed for that, so the answer is checked rather than assumed.
(deftest fixnum-leaf-range-length.dec-of-empty-is-negative
  (list (%flrl-dec '()) (%flrl-dec "") (%flrl-dec #()))
  (-1 -1 -1))

(deftest fixnum-leaf-range-length.declared-form-still-answers
  (%flrl-dec-declared '(a b c))
  2)

;;; ---- emitted code ----
;;;
;;; The needles end at the closing paren because a generic entry name is a
;;; prefix of its specialised twin -- "Runtime.Add" also matches every
;;; "Runtime.AddFixnum". PRINC-TO-STRING is what these read, and it prints
;;; neither the operand's quotes nor a keyword's colon, so an instruction
;;; spelled (:SUB) in the source appears here as (SUB). Each test names both
;;; the entry that should be gone and what should have replaced it, so neither
;;; half can pass by a needle that simply never matches.

(deftest-emitting-only fixnum-leaf-range-length.decrement-is-native
  (let ((s (%flrl-sil #'%flrl-dec)))
    (list (%flrl-count "Runtime.Decrement)" s)
          (%flrl-count "UNBOX-FIXNUM" s)
          (%flrl-count "Runtime.Length)" s)))
  (0 1 1))

(deftest-emitting-only fixnum-leaf-range-length.increment-is-native
  (let ((s (%flrl-sil #'%flrl-inc)))
    (list (%flrl-count "Runtime.Increment)" s)
          (%flrl-count "UNBOX-FIXNUM" s)))
  (0 1))

(deftest-emitting-only fixnum-leaf-range-length.multiply-is-fixnum-typed
  (let ((s (%flrl-sil #'%flrl-mul)))
    (list (%flrl-count "Runtime.MultiplyFixnum)" s)
          (%flrl-count "Runtime.Multiply)" s)))
  (1 0))

(deftest-emitting-only fixnum-leaf-range-length.add-is-fixnum-typed
  (let ((s (%flrl-sil #'%flrl-add)))
    (list (%flrl-count "Runtime.AddFixnum)" s)
          (%flrl-count "Runtime.Add)" s)
          (%flrl-count "UNBOX-FIXNUM" s)))
  (1 0 2))

;; The declared form is the control, and it goes the OTHER way: (THE FIXNUM
;; ...) gives the full int64 range, 1- widens it out of int64, and the proof
;; fails. This is what says the tests above are measuring the new tight range
;; and not something the declaration would have given them anyway.
(deftest-emitting-only fixnum-leaf-range-length.declared-form-stays-generic
  (plusp (%flrl-count "Runtime.Decrement)" (%flrl-sil #'%flrl-dec-declared)))
  t)

(setf dotcl:*save-sil* nil)
