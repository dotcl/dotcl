;;; The tree-walk evaluator must not spend a .NET frame per tail-recursive
;;; iteration. Without this, a tail-recursive loop, the ordinary CL idiom, could
;;; not be written at all on a build with no compiler (netstandard2.0 / WASM /
;;; AOT), where every call is interpreted: 100000 iterations exhausted the stack.
;;;
;;; A call in tail position to an interpreted function is returned to the
;;; trampoline the evaluator wraps around each function body rather than made,
;;; and the callee's body runs in that same frame. As in compiled code, the
;;; caller's debugger frame is handed to the callee, and a function declared
;;; (debug 3) makes no tail call except back into itself.
;;;
;;; Each test runs under :INTERPRET through EVAL so it exercises the evaluator on
;;; an ordinary build too, not only on an emit-free one.

(defmacro %itc (form) `(let ((dotcl:*evaluator-mode* :interpret)) (eval ,form)))

;;; The count only has to be past the depth at which interpreted recursion runs
;;; out of stack. That depth is near 10,000 frames (fewer on a Debug build), so
;;; 100,000 is ten times past it, and the second test checks that it still is:
;;; the same count without a tail call does run out. A larger count only adds
;;; interpreted iterations (1,000,000 took about a minute on a Debug build).

(defparameter *itc-depth* 100000)

(deftest interp-tail-calls.self-recursion-does-not-grow-the-stack
  (%itc `(progn (defun %itc-down (n) (if (= n 0) :done (%itc-down (- n 1))))
                (%itc-down ,*itc-depth*)))
  :done)

(deftest interp-tail-calls.the-count-is-past-the-stack
  (%itc `(progn (defun %itc-up (n) (if (= n 0) 0 (+ 1 (%itc-up (- n 1)))))
                (handler-case (%itc-up ,*itc-depth*)
                  (storage-condition () :out-of-stack))))
  :out-of-stack)

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
;;; alive.

(deftest interp-tail-calls.return-from-through-an-escaping-closure
  (%itc '(progn (defun %itc-esc (n)
                  (block lvl
                    (let ((esc (lambda () (return-from lvl (list :own n)))))
                      (if (zerop n) (funcall esc) (cons n (%itc-esc (- n 1)))))))
                (%itc-esc 3)))
  (3 2 1 :own 0))

;;; A tail call leaves the BLOCK, but a RETURN-FROM it from inside the call
;;; still returns from it.
(deftest interp-tail-calls.return-from-a-block-a-self-call-left
  (%itc '(progn (defun %itc-esc-self (n k)
                  (block b
                    (if (= n 0)
                        (funcall k)
                        (%itc-esc-self (1- n) (lambda () (return-from b (values :ok n)))))))
                (multiple-value-list (%itc-esc-self 3 (lambda () :first)))))
  (:ok 1))

;;; Non-tail recursion is unaffected: it still builds its frames and returns the
;;; accumulated result.

(deftest interp-tail-calls.non-tail-recursion-still-accumulates
  (%itc '(progn (defun %itc-sum (n) (if (= n 0) 0 (+ 1 (%itc-sum (- n 1)))))
                (%itc-sum 1000)))
  1000)
;;; --- calls to other functions -------------------------------------------

(deftest interp-tail-calls.mutual-recursion
  (%itc `(progn (defun %itc-ev (n) (if (= n 0) :even (%itc-od (1- n))))
                (defun %itc-od (n) (if (= n 0) :odd (%itc-ev (1- n))))
                (%itc-ev ,*itc-depth*)))
  :even)

(deftest interp-tail-calls.funcall-and-apply
  (%itc `(progn (defun %itc-fc (f n) (if (= n 0) :done (funcall f f (1- n))))
                (defun %itc-ap (n) (if (= n 0) :done (apply '%itc-ap2 (list (1- n)))))
                (defun %itc-ap2 (n) (apply #'%itc-ap n '()))
                (list (%itc-fc #'%itc-fc ,*itc-depth*)
                      (%itc-ap ,*itc-depth*))))
  (:done :done))

(deftest interp-tail-calls.through-a-closure-and-a-block
  (%itc `(progn (defun %itc-cl (n)
                  (let ((g (lambda (k) (if (= k 0) :done (%itc-cl (1- k))))))
                    (funcall g n)))
                (defun %itc-blk (n) (block nil (if (= n 0) :done (%itc-blk2 (1- n)))))
                (defun %itc-blk2 (n) (labels ((f (m) (%itc-blk m))) (f n)))
                (list (%itc-cl ,*itc-depth*) (%itc-blk ,*itc-depth*))))
  (:done :done))

;;; The same through calls to other functions, and to a block an earlier call
;;; in the chain left.
(deftest interp-tail-calls.return-from-a-block-a-tail-call-left
  (%itc '(progn (defun %itc-esc (n) (block b (%itc-esc2 n (lambda () (return-from b :escaped)))))
                (defun %itc-esc2 (n k) (if (= n 0) (funcall k) (%itc-esc (1- n))))
                (defun %itc-esc-first (n k)
                  (if (= n 0) (funcall k) (%itc-esc-first (1- n) k)))
                (defun %itc-esc-outer ()
                  (block b (%itc-esc-first 10 (lambda () (return-from b (values 1 2))))))
                (list (%itc-esc 100) (multiple-value-list (%itc-esc-outer)))))
  (:escaped (1 2)))

;;; Compiled and interpreted functions calling each other: each side keeps its
;;; own rule, and the values come back.
(defun %itc-compiled (n) (if (= n 0) (values :done n) (funcall '%itc-mixed (1- n))))

(deftest interp-tail-calls.mixed-with-compiled-code
  (%itc '(progn (defun %itc-mixed (n) (if (evenp n) (%itc-compiled n) (%itc-mixed-b n)))
                (defun %itc-mixed-b (n) (%itc-mixed (1- n)))
                (multiple-value-list (%itc-mixed 1000))))
  (:done 0))

(deftest interp-tail-calls.unwind-protect-is-not-tail
  (%itc '(let ((acc '()))
          (defun %itc-up-leaf (n) (push (list :leaf n) acc) n)
          (defun %itc-up (n) (unwind-protect (%itc-up-leaf n) (push :cleanup acc)))
          (list (%itc-up 3) (nreverse acc))))
  (3 ((:leaf 3) :cleanup)))

;;; --- the backtrace -------------------------------------------------------

(deftest interp-tail-calls.backtrace-shows-the-callee-in-place-of-the-caller
  (%itc '(progn (defun %itc-bt-leaf (a) (declare (ignore a)) (dotcl:backtrace-with-args))
                (defun %itc-bt-mid (x) (%itc-bt-leaf (list x)))
                (defun %itc-bt-via-lambda (x) (funcall (lambda (y) (%itc-bt-leaf y)) (list x)))
                (flet ((names (bt) (remove-if-not (lambda (f) (search "%ITC-BT-" (car f))) bt)))
                  (list (names (%itc-bt-mid 5)) (names (%itc-bt-via-lambda 6))))))
  ((("%ITC-BT-LEAF" (5))) (("%ITC-BT-LEAF" (6)))))

(deftest interp-tail-calls.debug-3-keeps-the-caller
  (%itc '(progn (defun %itc-bt3-leaf (a) (declare (ignore a)) (dotcl:backtrace))
                (defun %itc-bt3-mid (x) (declare (optimize (debug 3))) (%itc-bt3-leaf x))
                (remove-if-not (lambda (f) (search "%ITC-BT3-" f)) (%itc-bt3-mid 5))))
  ("%ITC-BT3-LEAF" "%ITC-BT3-MID"))
