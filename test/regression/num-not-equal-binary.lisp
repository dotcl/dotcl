;;; (/= a b) does not build an argument array.
;;;
;;; Every other numeric comparison lowers two arguments to a direct binary call.
;;; /= did not, because it is the one comparison that is not transitive:
;;; (/= a b c) asks that all three be pairwise distinct, so it cannot be chained
;;; the way (< a b c) can, and the N-ary entry takes an array. But two arguments
;;; have nothing pairwise about them, and that arity was paying for an argument
;;; array plus a second array the callee makes inside -- 80 bytes on the
;;; commonest call of a common operator.

(deftest num-not-equal.two-args
  (list (/= 1 2) (/= 2 2) (/= 1.0 1) (/= 1/2 0.5) (/= 3 3.0d0))
  (t nil nil nil nil))

(deftest num-not-equal.two-args-wide
  (list (/= 10000000000000000000 10000000000000000001)
        (/= 10000000000000000000 10000000000000000000)
        (/= most-positive-fixnum (1+ most-positive-fixnum)))
  (t nil t))

(deftest num-not-equal.complex
  (list (/= #c(1 2) #c(1 2)) (/= #c(1 2) #c(1 3)) (/= #c(1 0) 1))
  (nil t nil))

;;; Pairwise, not chained: the middle pair is equal, so the whole form is false
;;; even though each adjacent pair differs.
(deftest num-not-equal.pairwise
  (list (/= 1 2 3) (/= 1 2 1) (/= 1 2 3 4) (/= 1 2 3 1))
  (t nil t nil))

(deftest num-not-equal.one-arg
  (list (/= 5) (/= 1.5))
  (t t))

;;; A non-number is a type error at every arity, as it was before.
(deftest num-not-equal.type-error
  (list (handler-case (/= 1 'a) (type-error () :type-error) (error () :other))
        (handler-case (/= 'a 1) (type-error () :type-error) (error () :other))
        (handler-case (/= 1 2 'a) (type-error () :type-error) (error () :other)))
  (:type-error :type-error :type-error))

;;; NaN is not equal to itself, so /= answers true for it.
(deftest num-not-equal.arguments-evaluated-once
  (let ((n 0))
    (flet ((bump () (incf n)))
      (list (/= (bump) (bump)) n)))
  (t 2))

;;; --- What it costs ---

(defun %nne-bytes () (nth 4 (dotcl:gc-stats)))

(defvar *nne-a* 3)
(defvar *nne-b* 4)

(defun %nne-loop (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (/= *nne-a* *nne-b*)))))

(defun %nne-per-call ()
  "Bytes allocated per two-argument /=, smallest of five runs."
  (%nne-loop 2000)
  (let ((best nil))
    (dotimes (r 5 best)
      (let ((before (%nne-bytes)))
        (%nne-loop 100000)
        (let ((used (floor (- (%nne-bytes) before) 100000)))
          (when (or (null best) (< used best)) (setq best used)))))))

(deftest-compiled-only num-not-equal.no-argument-array
  (%nne-per-call)
  0)
