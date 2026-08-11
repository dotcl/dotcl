;;; Regression: an inlined body compiles at the call site under the CALLER's
;;; tail/TCO context — and that inheritance is load-bearing, not incidental.
;;; A call in tail position stays a tail of the caller after inlining, so a
;;; tail call back to the enclosing function inside the inlined body must ride
;;; the caller's TCO loop (deep recursion survives), and the inlined tail form
;;; must propagate multiple values. This is also why the direct function-body
;;; path never needs to reset *IN-TAIL-POSITION* at entry: nothing compiles
;;; before its own T binding, and at inline sites the inherited value is the
;;; correct one by definition.

(defpackage :itp (:use))

;; Tail call to the ENCLOSING defun from inside an inlined body: after
;; inlining this is a self-tail-call of ITP-F1 and must TCO (200000 deep).
(declaim (inline itp-g1))
(defun itp-g1 (x acc) (if (<= x 0) acc (itp-f1 (- x 1) (+ acc x))))
(defun itp-f1 (x acc) (itp-g1 x acc))

(deftest itp-inline-tail-correct (itp-f1 100 0) 5050)
(deftest-compiled-only itp-inline-tail-deep    (itp-f1 200000 0) 20000100000)

;; Multiple values through an inlined call in tail position...
(declaim (inline itp-g2))
(defun itp-g2 (x) (values x (* x 2)))
(defun itp-f2 (x) (itp-g2 x))
(deftest itp-inline-mv (multiple-value-list (itp-f2 5)) (5 10))

;; ...and primary-value-only when the inlined call is NOT in tail position.
(defun itp-f3 (x) (list (itp-g2 x) :end))
(deftest itp-inline-nontail (itp-f3 5) (5 :END))

;; A self-call of the inlinee inside its own inlined body is refused by the
;; inlining stack and must compile as a normal call to the inlinee — not
;; match the enclosing function's TCO loop.
(declaim (inline itp-g4))
(defun itp-g4 (x) (if (<= x 0) 0 (+ 1 (itp-g4 (- x 1)))))
(defun itp-f4 (x) (itp-g4 x))
(deftest itp-inline-self-ref (itp-f4 5) 5)

;; Inlined tail call under handler-case: the try/catch TCO path (leave +
;; PopCluster) must still fire through the inlined body.
(declaim (inline itp-g5))
(defun itp-g5 (x acc) (if (<= x 0) acc (itp-f5 (- x 1) (+ acc 1))))
(defun itp-f5 (x acc) (handler-case (itp-g5 x acc) (error () :err)))
(deftest itp-inline-handler-correct (itp-f5 100 0) 100)
(deftest-compiled-only itp-inline-handler-deep    (itp-f5 200000 0) 200000)
