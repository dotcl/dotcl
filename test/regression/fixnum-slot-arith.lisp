;;; Arithmetic into a declared-FIXNUM slot stays in the slot.
;;;
;;; A local declared FIXNUM lives in an Int64 slot, and (SETQ ACC (+ ACC X)) --
;;; the shape every accumulating loop has -- used to box both operands, call the
;;; generic promoting ADD, and unbox the result: two allocations and a generic
;;; dispatch per iteration. The value-range proof that gates the raw path cannot
;;; help there, because FIXNUM + FIXNUM spans one bit more than int64 and so
;;; never proves, however the program is written.
;;;
;;; It now computes natively with an overflow CHECK. Overflow has nowhere to go
;;; (the slot is 64 bits) so it must be reported either way; what changed is that
;;; the report names the declaration instead of surfacing as a cast failure.

(defun %fsa-sum (n)
  (declare (fixnum n))
  (let ((s 0))
    (declare (fixnum s))
    (do ((i 0 (1+ i))) ((= i n) s)
      (declare (fixnum i))
      (setq s (+ s i)))))

(defun %fsa-mixed (n)
  (declare (fixnum n))
  (let ((s 1))
    (declare (fixnum s))
    (do ((i 1 (1+ i))) ((= i n) s)
      (declare (fixnum i))
      (setq s (- (* s 3) i)))))

(deftest fixnum-slot-arith.values-are-unchanged
  (list (%fsa-sum 0) (%fsa-sum 1) (%fsa-sum 10) (%fsa-sum 1000)
        (%fsa-mixed 5))
  (0 0 45 499500 23))

;;; The point of the change: the loop does not allocate per iteration.
;;;
;;; Measured as the DIFFERENCE between two loop lengths, so whatever else the
;;; image is doing (other threads, the counter read itself) cancels out instead
;;; of being charged to the loop. A boxed accumulator allocated two Fixnums per
;;; iteration, so the difference would be on the order of 10 MB; the bound below
;;; is far above zero and far below that.
(defun %fsa-bytes-for (n)
  ;; (nth 4 ...) is the cumulative allocation counter. FOURTH is the current
  ;; heap size, which a GC can move DOWN mid-measurement -- reading that one
  ;; made this assertion unfailable.
  ;;
  ;; MIN of several runs: the counter is process-wide, so anything else alive in
  ;; the image (threads left running by earlier tests in the suite) adds to some
  ;; samples and never subtracts from any. The smallest sample is the one least
  ;; polluted by that.
  (let ((best nil))
    (dotimes (r 5 best)
      (let ((before (nth 4 (dotcl:gc-stats))))
        (%fsa-sum n)
        (let ((used (- (nth 4 (dotcl:gc-stats)) before)))
          (when (or (null best) (< used best)) (setq best used)))))))

;; Compiled-only: this is a statement about the code the compiler emits. An
;; emit-free build interprets the loop, where the value has no native slot to
;; stay in and the allocation is expected.
(deftest-compiled-only fixnum-slot-arith.loop-allocates-nothing
  (progn (%fsa-bytes-for 1000)              ; warm
         (let ((small (%fsa-bytes-for 100000))
               (large (%fsa-bytes-for 400000)))
           ;; A boxed accumulator cost about 30 bytes per iteration, so the
           ;; 300k extra iterations would show ~9 MB here.
           (< (- large small) 100000)))
  t)

;;; Overflowing the slot is a declaration violation and is reported as one.
;;;
;;; Compiled-only, like the allocation check below: the slot is what the compiler
;;; gives a declared FIXNUM local, and an emit-free build has no slots -- there
;;; the accumulator simply promotes to a bignum, which is correct for that
;;; evaluator and says nothing about this one.
(defun %fsa-overflow-add (n)
  (declare (fixnum n))
  (let ((s 1))
    (declare (fixnum s))
    (do ((i 0 (1+ i))) ((= i n) s)
      (declare (fixnum i))
      (setq s (+ s s)))))

(defun %fsa-overflow-mul (n)
  (declare (fixnum n))
  (let ((s 1))
    (declare (fixnum s))
    (do ((i 0 (1+ i))) ((= i n) s)
      (declare (fixnum i))
      (setq s (* s 1000000)))))

(deftest fixnum-slot-arith.in-range-is-fine
  (list (%fsa-overflow-add 61) (%fsa-overflow-mul 3))
  (2305843009213693952 1000000000000000000))

(deftest-compiled-only fixnum-slot-arith.overflow-signals
  (list (handler-case (progn (%fsa-overflow-add 64) :no-error)
          (type-error () :type-error) (error () :other))
        (handler-case (progn (%fsa-overflow-mul 5) :no-error)
          (type-error () :type-error) (error () :other)))
  (:type-error :type-error))

;;; An undeclared accumulator still promotes to a bignum, as CL requires.
(defun %fsa-boxed (n) (let ((s 1)) (dotimes (i n s) (setq s (+ s s)))))

(deftest fixnum-slot-arith.undeclared-still-promotes
  (list (%fsa-boxed 70) (integerp (%fsa-boxed 200)))
  (1180591620717411303424 t))

;;; The interpreter has no slots to promote into; it must agree on the values.
(deftest fixnum-slot-arith.interpreted-agrees
  (let ((dotcl:*evaluator-mode* :interpret))
    (list (eval '(let ((s 0)) (declare (fixnum s))
                   (do ((i 0 (1+ i))) ((= i 10) s) (declare (fixnum i))
                     (setq s (+ s i)))))
          (eval '(let ((s 1)) (dotimes (i 70 s) (setq s (+ s s)))))))
  (45 1180591620717411303424))
