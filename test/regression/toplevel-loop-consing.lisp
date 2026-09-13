;;; A declared FIXNUM loop written at top level allocates nothing per iteration.
;;;
;;; Codegen emits a native-slot store in its boxed form -- store the raw value,
;;; then box it for the expression's value -- and relies on the peephole to
;;; delete the box when the value is discarded, which is what a loop step is.
;;; The peephole ran on every function body but not on top-level forms, so the
;;; same loop cost 16 B per iteration when written in a script or typed at the
;;; REPL and 0 B when wrapped in a DEFUN. (16 and not 24 because Fixnum caches
;;; -128..65535, so only the iterations past the cache allocated.)
;;;
;;; The loops below have to be written out at top level: putting one inside a
;;; helper function would compile it through the function-body path and prove
;;; nothing.

(defun %tlc-bytes () (nth 4 (dotcl:gc-stats)))

(defparameter *tlc-small* nil)
(defparameter *tlc-large* nil)
(defparameter *tlc-sum* nil)

;;; Warm: same shape, so nothing below pays for first-call JIT.
(let ((n 1000))
  (declare (fixnum n))
  (do ((i 0 (1+ i))) ((= i n)) (declare (fixnum i))))

;;; MIN of several runs, for the reason FIXNUM-SLOT-ARITH gives: the counter is
;;; process-wide, so other live threads only ever add.
;;;
;;; Guarded, not just the assertion below it: the loops have to be written out at
;;; top level (that is the whole point), so nothing else stops them running in the
;;; builds that skip the assertion -- tens of thousands of interpreted iterations
;;; for a number those builds then discard.
(when (compiled-mode-p)
  (let ((best nil))
    (dotimes (r 3)
      (let ((before (%tlc-bytes)) (n 10000))
        (declare (fixnum n))
        (do ((i 0 (1+ i))) ((= i n)) (declare (fixnum i)))
        (let ((used (- (%tlc-bytes) before)))
          (when (or (null best) (< used best)) (setq best used)))))
    (setq *tlc-small* best))

  (let ((best nil))
    (dotimes (r 3)
      (let ((before (%tlc-bytes)) (n 40000))
        (declare (fixnum n))
        (do ((i 0 (1+ i))) ((= i n)) (declare (fixnum i)))
        (let ((used (- (%tlc-bytes) before)))
          (when (or (null best) (< used best)) (setq best used)))))
    (setq *tlc-large* best)))

;;; The loop still computes: deleting the box must not delete the store.
(let ((s 0))
  (declare (fixnum s))
  (do ((i 0 (1+ i))) ((= i 1000)) (declare (fixnum i)) (setq s (+ s i)))
  (setq *tlc-sum* s))

(deftest toplevel-loop-consing.value-is-unchanged
  *tlc-sum*
  499500)

;;; Compiled-only, like the other consing assertions: an emit-free build
;;; interprets the loop, where there is no native slot for the counter to stay
;;; in and the allocation is expected.
;;;
;;; 30000 extra iterations at 16 B each would show ~480 KB. The bound is far
;;; above the noise of two counter reads and far below that.
(deftest-compiled-only toplevel-loop-consing.no-per-iteration-box
  (< (- *tlc-large* *tlc-small*) 10000)
  t)
