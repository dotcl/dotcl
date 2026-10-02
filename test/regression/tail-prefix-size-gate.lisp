;;; The tail. prefix on a function's final call is kept for small bodies and
;;; dropped for large ones.
;;;
;;; The CLR JIT compiles a method containing an explicit tail call with full
;;; optimization on its first call, skipping tier 0. For a large body that is
;;; paid up front whether or not the function ever recurses: a test suite's
;;; subtest lambda of ~35 KB IL spent most of its one and only call in the JIT.
;;; Small functions keep the prefix, so mutual recursion through them stays
;;; flat.

(setf dotcl:*save-sil* t)

(defun %tpsg-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %tpsg-callee (x) x)

;; Small: ends in a call to another function.
(defun %tpsg-small (x) (%tpsg-callee (list x)))

;; Large: the same final call after a long body. Each form for effect is about
;; ten instructions, so 200 of them put the body well past the gate while
;; staying under the count at which a long PROGN is split into chunks (the
;; final call would then sit in a small chunk of its own).
(macrolet ((def ()
             `(defun %tpsg-large (x)
                ,@(loop for i below 200 collect `(%tpsg-callee (list ,i ,i ,i)))
                (%tpsg-callee (list x)))))
  (def))

(deftest-emitting-only tail-prefix-size-gate.small-keeps-prefix
  (and (search "TAIL-PREFIX" (%tpsg-sil #'%tpsg-small)) t)
  t)

(deftest-emitting-only tail-prefix-size-gate.large-drops-prefix
  (search "TAIL-PREFIX" (%tpsg-sil #'%tpsg-large))
  nil)

(deftest tail-prefix-size-gate.large-still-returns-the-call-value
  (%tpsg-large 7)
  (7))

;; Mutual recursion through small functions stays a tail call: 300000 frames
;; deep would exhaust the stack otherwise.
(defun %tpsg-even (n) (if (zerop n) :even (%tpsg-odd (1- n))))
(defun %tpsg-odd (n) (if (zerop n) :odd (%tpsg-even (1- n))))

(deftest-compiled-only tail-prefix-size-gate.small-mutual-recursion-is-flat
  (%tpsg-even 300000)
  :even)
