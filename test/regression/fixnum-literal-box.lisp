;;; An integer literal outside Fixnum.Make's small-integer cache, used as a
;;; Lisp object, is loaded as a constant instead of boxed on every evaluation.

(defun %flb-mod (b) (mod 4294967296 b))
(defun %flb-add (x) (+ x -1000000))
(defun %flb-loop (n)
  (let ((s 0))
    (dotimes (i n s) (setf s (%flb-mod 1000)))))
(defun %flb-native (n)
  (let ((x 0))
    (declare (fixnum x))
    (dotimes (i n) (setq x 100000))
    x))

(deftest fixnum-literal-box.values
  (list (%flb-mod 1000) (%flb-mod 4294967297) (%flb-add 1000000) (%flb-add 0)
        (%flb-native 3) (eql (%flb-mod 7) (%flb-mod 7)))
  (296 4294967296 0 -1000000 100000 t))

(deftest-emitting-only fixnum-literal-box.no-allocation
  (progn
    (%flb-loop 100)
    (< (min-allocated-bytes (lambda () (%flb-loop 100000))) 10000))
  t)
