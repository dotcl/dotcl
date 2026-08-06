;;; Regression: compile-as-long must not unbox a captured+mutated local's cell.
;;;
;;; A local that is both mutated and captured by a closure lives in a
;;; LispObject[1] box cell. compile-as-long's declared-fixnum branch loaded the
;;; local's slot and emitted :unbox-fixnum directly, which casts the cell array
;;; itself to Fixnum — a hard InvalidCastException at run time. Reached via
;;; e.g. (the fixnum (- j lo)) initializing a fixnum-declared local, where J is
;;; boxed: the arithmetic recursion of compile-as-long hit the bare local.
;;; (lparallel's psort granularity variants died this way: the plet closures
;;; capture the mutated loop counters I and J, and the granularity branch adds
;;; fixnum-declared size locals computed from them.)

(deftest boxed-fixnum-as-long-init
  (let ((j 5) (lo 0) (thunk nil))
    (declare (type fixnum j lo))
    (setq thunk (lambda () (setq j (the fixnum (1- j)))))
    (funcall thunk)
    (let ((left-size (the fixnum (- j lo))))
      (declare (type fixnum left-size))
      left-size))
  4)

;;; Same shape but the boxed var feeds a native comparison operand.
(deftest boxed-fixnum-as-long-cmp
  (let ((j 10) (bump nil))
    (declare (type fixnum j))
    (setq bump (lambda () (setq j (the fixnum (+ j 1)))))
    (funcall bump)
    (let ((n (the fixnum (* j 2))))
      (declare (type fixnum n))
      (if (> n 20) :big :small)))
  :big)

;;; Mutation applied through the closure AFTER the fixnum-typed read was
;;; compiled — both reads must see the current cell value.
(deftest boxed-fixnum-as-long-reread
  (let ((i 0) (acc nil) (thunk nil))
    (declare (type fixnum i))
    (setq thunk (lambda () (setq i (the fixnum (1+ i)))))
    (dotimes (k 3)
      (funcall thunk)
      (push (the fixnum (* i 10)) acc))
    (nreverse acc))
  (10 20 30))
