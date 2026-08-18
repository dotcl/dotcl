;;; The tree-walk evaluator must not spend a .NET frame per tail-recursive
;;; iteration. Without this, a tail-recursive loop — the ordinary CL idiom — could
;;; not be written at all on a build with no compiler (netstandard2.0 / WASM /
;;; AOT), where every call is interpreted: 100000 iterations exhausted the stack.
;;;
;;; A call in tail position is returned to the trampoline the evaluator wraps
;;; around each function body rather than made, and the body is re-entered in that
;;; same frame. Only a call back into the SAME function is treated this way, which
;;; is exactly what the compiler optimizes — so the two evaluators still agree,
;;; including on what BACKTRACE shows.
;;;
;;; Each test runs under :INTERPRET through EVAL so it exercises the evaluator on
;;; an ordinary build too, not only on an emit-free one.

(defmacro %itc (form) `(let ((dotcl:*evaluator-mode* :interpret)) (eval ,form)))

(deftest interp-tail-calls.self-recursion-does-not-grow-the-stack
  (%itc '(progn (defun %itc-down (n) (if (= n 0) :done (%itc-down (- n 1))))
                (%itc-down 1000000)))
  :done)

;;; The value still comes back, and so do all of them: a body ending in a call
;;; that returns several values must not decay to the first.

(deftest interp-tail-calls.multiple-values-survive-a-tail-call
  (%itc '(progn (defun %itc-mv () (values 1 2 3))
                (defun %itc-mv-tail () (%itc-mv))
                (multiple-value-list (%itc-mv-tail))))
  (1 2 3))

(deftest interp-tail-calls.no-values-stays-no-values
  (%itc '(progn (defun %itc-none () (values))
                (multiple-value-list (%itc-none))))
  ())

;;; A tail call is only tail where the value is really returned. A body that binds
;;; a special, or sits under UNWIND-PROTECT, must keep its frame: the binding and
;;; the cleanup have to outlive the call.

(deftest interp-tail-calls.special-binding-outlives-the-call
  (%itc '(progn (defvar *itc-v* :outer)
                (defun %itc-read () *itc-v*)
                (defun %itc-bind () (let ((*itc-v* :inner)) (%itc-read)))
                (%itc-bind)))
  :inner)

(deftest interp-tail-calls.unwind-protect-cleanup-still-runs
  (%itc '(let ((acc '()))
          (flet ((%itc-g () (push :body acc) :v))
            (unwind-protect (%itc-g) (push :cleanup acc)))
          (nreverse acc)))
  (:body :cleanup))

;;; A closure that escapes its BLOCK by RETURN-FROM must still find the block
;;; alive. Handing a call to a DIFFERENT function to the trampoline would unwind
;;; the block first and the throw would land on a dead tag — which is why only
;;; self calls are handed over.

(deftest interp-tail-calls.return-from-through-an-escaping-closure
  (%itc '(progn (defun %itc-esc (n)
                  (block lvl
                    (let ((esc (lambda () (return-from lvl (list :own n)))))
                      (if (zerop n) (funcall esc) (cons n (%itc-esc (- n 1)))))))
                (%itc-esc 3)))
  (3 2 1 :own 0))

;;; Non-tail recursion is unaffected — it still builds its frames and returns the
;;; accumulated result.

(deftest interp-tail-calls.non-tail-recursion-still-accumulates
  (%itc '(progn (defun %itc-sum (n) (if (= n 0) 0 (+ 1 (%itc-sum (- n 1)))))
                (%itc-sum 1000)))
  1000)
