;;; A function's leading declarations survive the implicit BLOCK.
;;;
;;; DEFUN wraps its body in an implicit block so RETURN-FROM resolves, and it
;;; only bothers when the body actually contains one. The wrap used to sweep the
;;; declarations inside with everything else:
;;;
;;;     (block f (declare (optimize (safety 0))) ...)
;;;
;;; which is not where a DEFUN's declarations belong -- CLHS 3.4.11 puts them in
;;; the lambda, and a BLOCK body takes no declarations at all -- and everything
;;; that reads the function's leading declarations reads that list. So
;;; (optimize (safety 0)) became invisible to BODY-DECLARED-SAFETY for every
;;; function containing a RETURN-FROM, and three separate things quietly stopped
;;; happening in exactly those functions:
;;;
;;;   - the loop back-edge interrupt poll was not elided
;;;   - (the fixnum ...) kept its overflow check instead of being a licence
;;;   - a structure slot's :TYPE check was emitted
;;;
;;; Each looks like its own feature failing; all three are this one line. The
;;; tests below are the matrix that would have caught it: with and without
;;; RETURN-FROM, at safety 0 and at safety 1, for all three consumers.

(setf dotcl:*save-sil* t)

(defun %dtb-has (needle fn)
  (and (search needle (princ-to-string (dotcl:function-sil fn))) t))

;;; ---- consumer 1: the loop back-edge safepoint ----

(defun %dtb-loop-plain (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((a 0))
    (declare (fixnum a))
    (dotimes (i n) (declare (fixnum i)) (setq a (the fixnum (+ a i))))
    a))

(defun %dtb-loop-return (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((a 0))
    (declare (fixnum a))
    (dotimes (i n)
      (declare (fixnum i))
      (when (> i 1000) (return-from %dtb-loop-return -1))
      (setq a (the fixnum (+ a i))))
    a))

(defun %dtb-loop-return-safe (n)
  (declare (fixnum n) (optimize (speed 3) (safety 1) (debug 0)))
  (let ((a 0))
    (declare (fixnum a))
    (dotimes (i n)
      (declare (fixnum i))
      (when (> i 1000) (return-from %dtb-loop-return-safe -1))
      (setq a (the fixnum (+ a i))))
    a))

;; safety 0 elides the poll whether or not the body returns early; safety 1
;; keeps it either way.
(deftest-compiled-only declarations-through-block.safepoint-follows-safety
  (list (%dtb-has "PollInterrupt" #'%dtb-loop-plain)
        (%dtb-has "PollInterrupt" #'%dtb-loop-return)
        (%dtb-has "PollInterrupt" #'%dtb-loop-return-safe))
  (nil nil t))

;;; ---- consumer 2: a structure slot's declared type ----

(defstruct dtb-box (n 0 :type fixnum))

(defun %dtb-store-plain (b v)
  (declare (type dtb-box b) (fixnum v) (optimize (speed 3) (safety 0) (debug 0)))
  (setf (dtb-box-n b) v)
  nil)

(defun %dtb-store-return (b v)
  (declare (type dtb-box b) (fixnum v) (optimize (speed 3) (safety 0) (debug 0)))
  (when (> v 0)
    (setf (dtb-box-n b) v)
    (return-from %dtb-store-return -1))
  nil)

(defun %dtb-store-return-safe (b v)
  (declare (type dtb-box b) (optimize (speed 3) (safety 1) (debug 0)))
  (when (> v 0)
    (setf (dtb-box-n b) v)
    (return-from %dtb-store-return-safe -1))
  nil)

(deftest-compiled-only declarations-through-block.slot-check-follows-safety
  (list (%dtb-has "CheckSlotType" #'%dtb-store-plain)
        (%dtb-has "CheckSlotType" #'%dtb-store-return)
        (%dtb-has "CheckSlotType" #'%dtb-store-return-safe))
  (nil nil t))

;; And the check that is emitted still works.
(deftest declarations-through-block.slot-check-still-signals
  (handler-case (%dtb-store-return-safe (make-dtb-box) "not a fixnum")
    (type-error () :type-error))
  :type-error)

;;; ---- consumer 3: (the fixnum ...) as a licence ----
;;;
;;; At safety 0 the assertion is a licence and the addition is a raw int64 op;
;;; at safety 1 it is checked. What is pinned here is only that RETURN-FROM does
;;; not change which of the two a function gets.

(defun %dtb-the-plain (a b)
  (declare (fixnum a b) (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (+ a b)))

(defun %dtb-the-return (a b)
  (declare (fixnum a b) (optimize (speed 3) (safety 0) (debug 0)))
  (when (> a 0) (return-from %dtb-the-return (the fixnum (+ a b))))
  (the fixnum (+ a b)))

(deftest-compiled-only declarations-through-block.the-licence-follows-safety
  (let ((plain (%dtb-has "AddFixnumChecked" #'%dtb-the-plain))
        (ret (%dtb-has "AddFixnumChecked" #'%dtb-the-return)))
    (eq plain ret))
  t)

;;; ---- values are unaffected either way ----

(deftest declarations-through-block.values-unchanged
  (list (%dtb-loop-plain 10)
        (%dtb-loop-return 10)
        (%dtb-loop-return-safe 10)
        (%dtb-the-plain 3 4)
        (%dtb-the-return 3 4)
        (let ((b (make-dtb-box))) (%dtb-store-plain b 7) (dtb-box-n b))
        (let ((b (make-dtb-box))) (%dtb-store-return b 7) (dtb-box-n b)))
  (45 45 45 7 7 7 7))

;; An early return still returns from the right place -- the block is still
;; there, only the declarations moved out of it.
(deftest declarations-through-block.return-from-still-works
  (list (%dtb-loop-return 2000)
        (%dtb-store-return (make-dtb-box) 5)
        (%dtb-store-return (make-dtb-box) -5))
  (-1 -1 nil))

(setf dotcl:*save-sil* nil)
