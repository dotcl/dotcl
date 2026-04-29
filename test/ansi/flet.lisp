;;; flet.lisp — Tests from ansi-test/data-and-control-flow/flet.lsp
;;; Excluded: &aux, block from optional/key defaults
;;; Note: signals-error tests require program-error (arg count check), not yet supported

(deftest flet.1
  (flet ((%f () 1))
    (%f))
  1)

(deftest flet.2
  (flet ((%f (x) x))
    (%f 2))
  2)

(deftest flet.3
  (flet ((%f (&rest args) args))
    (%f 'a 'b 'c))
  (a b c))

;;; flet.5 excluded: requires implicit block in flet (not yet implemented)

;;; The function is not visible inside itself
(deftest flet.7
  (flet ((%f (x) (+ x 5)))
    (flet ((%f (y) (if (eql y 20) 30 (%f 20))))
      (%f 15)))
  25)

;;; labels: function visible inside itself
(deftest labels.1
  (labels ((%f (x) (if (= x 0) 1 (* x (%f (- x 1))))))
    (%f 5))
  120)

(deftest labels.2
  (labels ((%even (x) (if (= x 0) t (%odd (- x 1))))
           (%odd (x) (if (= x 0) nil (%even (- x 1)))))
    (values (%even 10) (%odd 11)))
  t t)

(deftest labels.3
  (labels ((fib (n)
             (if (<= n 1) n
                 (+ (fib (- n 1)) (fib (- n 2))))))
    (fib 10))
  55)

(do-tests-summary)
