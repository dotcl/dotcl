;;;; Load every il-parity case, check its self-check value, and compile it.
;;;;
;;;; Two jobs in one dotcl process, because both want the same loaded image:
;;;; the self-check is what licenses the IL comparison (two programs that do
;;;; different things have nothing to compare), and COMPILE-FILE produces the
;;;; fasl the comparator reads.
;;;;
;;;; The expected values are written here rather than computed, so that a change
;;;; which quietly breaks one of the structures is caught by this file and not
;;;; by a reader wondering why the instruction counts moved.

(in-package :cl-user)

(defparameter *ilp-root*
  (or (dotcl:getenv "IL_PARITY_ROOT")
      (error "IL_PARITY_ROOT is not set"))
  "Absolute path of bench/il-parity, in the host's own syntax.")

(defparameter *ilp-cases*
  '(("stack"     ilp-stack-selfcheck  50  2872)
    ("hash"      ilp-hash-selfcheck  200  219099)
    ("heap"      ilp-heap-selfcheck  200  1343498)
    ("ring"      ilp-ring-selfcheck  500  858950)
    ("tokenizer" ilp-tok-selfcheck   300  74033288)
    ("vec2"      ilp-vec2-selfcheck 200  245144966))
  "(NAME FUNCTION ARGUMENT EXPECTED). EXPECTED is what the C# half returns for
   the same argument; Ref.cs carries the same table implicitly.")

(defun ilp-path (name file)
  (concatenate 'string *ilp-root* "/" name "/" file))

(let ((failures 0))
  (dolist (c *ilp-cases*)
    (load (ilp-path (first c) "impl.lisp")))
  (format t "~&=== self-check (the Lisp half must agree with the C# half) ===~%")
  (dolist (c *ilp-cases*)
    (destructuring-bind (name fn arg expected) c
      (let* ((start (get-internal-real-time))
             (got (funcall fn arg))
             (ms (/ (* 1000.0d0 (- (get-internal-real-time) start))
                    internal-time-units-per-second)))
        (format t "~12A ~12D  expected ~12D  ~:[MISMATCH~;ok~]  ~,2F ms~%"
                name got expected (eql got expected) ms)
        (unless (eql got expected) (incf failures)))))
  (format t "~&=== compile-file ===~%")
  (dolist (c *ilp-cases*)
    (let ((p (ilp-path (first c) "impl.lisp")))
      (compile-file p)
      (format t "~12A compiled~%" (first c))))
  (finish-output)
  (when (plusp failures)
    (format t "~&il-parity: ~D self-check mismatch(es)~%" failures)
    (dotcl:quit 1)))
