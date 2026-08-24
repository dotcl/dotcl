;;; %MINI-EVAL (dotcl's compile-time tree-walk interpreter, used for MACROLET and
;;; other interpreted expander bodies) had no handler for %DOTIMES-1+ — the compiler
;;; intrinsic that dotcl's DOTIMES macro emits for a fixnum counter. A macro whose
;;; expander body used DOTIMES and was expanded through %MINI-EVAL therefore called
;;; %DOTIMES-1+ as an undefined function (seen in the SBCL make-host-2 build via the
;;; DO-FPRS assembler macro). Fix: %mini-eval treats %DOTIMES-1+ as 1+ (no int64
;;; assertion applies in the interpreter).

;; MACROLET expander bodies run via %mini-eval; a plain DOTIMES in one must work.
(deftest mini-eval-dotimes.macrolet
  (macrolet ((m () (let ((acc nil)) (dotimes (i 5) (push i acc)) `(quote ,(nreverse acc)))))
    (m))
  (0 1 2 3 4))

;; A (declare (fixnum i)) DOTIMES forces the %DOTIMES-1+ intrinsic path specifically.
(deftest mini-eval-dotimes.fixnum-counter
  (macrolet ((m () (let ((s 0)) (dotimes (i 4) (declare (fixnum i)) (setf s (+ s i))) `(quote ,s))))
    (m))
  6)

;; DOTIMES result form + counter both reachable through the interpreter.
(deftest mini-eval-dotimes.result-form
  (macrolet ((m () (let ((n 0)) `(quote ,(dotimes (i 3 n) (incf n))))))
    (m))
  3)

;; The variable-count DOTIMES emits the other intrinsic, %FIXNUM-GE-OBJECT
;; (raw counter vs boxed limit). A macrolet expander runs through %MINI-EVAL,
;; which needs its own handler for it -- CI's tree-walk regression job caught
;; the missing one.
(deftest mini-eval-dotimes.variable-count
  (macrolet ((m (depth)
               (let ((acc nil))
                 (dotimes (i depth) (push i acc))
                 `(quote ,(nreverse acc)))))
    (m 4))
  (0 1 2 3))
