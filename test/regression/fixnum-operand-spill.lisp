;;; Native int64 arithmetic must not leave one operand pending on the CIL stack
;;; while the other operand's code runs.
;;;
;;; The unboxed fixnum path emitted `<operand1> <operand2> op`, which is only
;;; valid CIL when operand2's code is branch-free. It is not when operand2 is a
;;; call to a function proclaimed INLINE whose body is a COND: the inline
;;; expansion is (LET (...) (BLOCK f body)), the block's arms jump to a join
;;; label, and the pending operand makes the stack depth at that label disagree
;;; between the incoming paths. The JIT rejected the method outright
;;; (InvalidProgramException) — and only when it was first CALLED, because JIT is
;;; lazy, so the failure surfaced far from the code that caused it.
;;;
;;; Non-straight-line operands are now evaluated into Int64 temps first. These
;;; tests pin the observable part of that: the values, and left-to-right
;;; evaluation order.

(defparameter *fos-log* nil)

(declaim (inline fos-cond-accessor))
(defun fos-cond-accessor (x)
  ;; A COND with differently shaped arms — the shape that made the inline
  ;; expansion branch.
  (cond ((null x) 0)
        ((consp x) (the fixnum (car x)))
        (t (the fixnum x))))

(defun fos-note (tag n)
  (push tag *fos-log*)
  n)

;;; --- values are unchanged by the spill

(defun fos-sub (a b)
  (declare (optimize (speed 3) (safety 0)))
  (the fixnum (- (the fixnum (fos-cond-accessor a)) (the fixnum (fos-cond-accessor b)))))

(deftest fixnum-operand-spill.subtract
  (list (fos-sub 10 4) (fos-sub '(10) 4) (fos-sub nil 4) (fos-sub '(10) '(4)))
  (6 6 -4 6))

(defun fos-xor (a b)
  (declare (optimize (speed 3) (safety 0)))
  (the fixnum (logxor (the fixnum (fos-cond-accessor a)) (the fixnum (fos-cond-accessor b)))))

(deftest fixnum-operand-spill.logxor
  (list (fos-xor 12 10) (fos-xor '(12) 10) (fos-xor nil 10))
  (6 6 10))

(defun fos-gt (a b)
  (declare (optimize (speed 3) (safety 0)))
  (> (the fixnum (fos-cond-accessor a)) (the fixnum (* (the fixnum (fos-cond-accessor b)) 4))))

(deftest fixnum-operand-spill.compare
  (list (fos-gt 100 4) (fos-gt 10 4) (fos-gt '(100) '(4)))
  (t nil t))

;;; --- operands still evaluate left to right (a spill that reorders would pass
;;; the value tests above and fail here)

(defun fos-ordered (a b)
  (declare (optimize (speed 3) (safety 0)))
  (the fixnum (- (the fixnum (fos-note :left a)) (the fixnum (fos-note :right b)))))

(deftest fixnum-operand-spill.evaluation-order
  (let ((*fos-log* nil))
    (list (fos-ordered 9 4) (reverse *fos-log*)))
  (5 (:left :right)))

;;; --- nested: an inlined-cond operand inside another arithmetic operand

(defun fos-nested (a b c)
  (declare (optimize (speed 3) (safety 0)))
  (the fixnum (+ (the fixnum (- (the fixnum (fos-cond-accessor a))
                                (the fixnum (fos-cond-accessor b))))
                 (the fixnum (fos-cond-accessor c)))))

(deftest fixnum-operand-spill.nested
  (list (fos-nested 10 4 1) (fos-nested '(10) '(4) '(1)))
  (7 7))

;;; --- the result still feeds a place update (the shape fset's CHAMP nodes use)

(defstruct (fos-node (:type vector))
  (size 0 :type fixnum)
  (hash 0 :type fixnum))

(defun fos-accumulate (n a b)
  (declare (optimize (speed 3) (safety 0)) (type simple-vector n))
  (setf (fos-node-hash n)
        (the fixnum (logxor (the fixnum (fos-node-hash n))
                            (the fixnum (- (the fixnum (fos-cond-accessor a))
                                           (the fixnum (fos-cond-accessor b)))))))
  (incf (fos-node-size n) (the fixnum (fos-cond-accessor a)))
  n)

(deftest fixnum-operand-spill.place-accumulate
  (let ((n (make-fos-node :size 1 :hash 12)))
    (fos-accumulate n 10 4)
    (list (fos-node-hash n) (fos-node-size n)))
  (10 11))
