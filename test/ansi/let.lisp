;;; let.lisp — Tests from ansi-test/data-and-control-flow/let.lsp
;;; Excluded: locally, declare dynamic-extent, duplicate bindings

(deftest let.1
  (let ((x 0)) x)
  0)

(deftest let.2
  (let ((x 0) (y 1)) (values x y))
  0 1)

(deftest let.4
  (let ((x 0))
    (let ((x 1))
      x))
  1)

(deftest let.9
  (let (x y z) (values x y z))
  nil nil nil)

(deftest let.11
  (let ((x 0))
    (let ((x 1) (y x))
      (values x y)))
  1 0)

;;; let*

(deftest let*.1
  (let* ((x 0)) x)
  0)

(deftest let*.2
  (let* ((x 0) (y x)) y)
  0)

(deftest let*.3
  (let* ((x 0) (y 1))
    (values x y))
  0 1)

(deftest let*.4
  (let* ((x 0) (x 1)) x)
  1)

(deftest let*.5
  (let* ((x 1) (y (+ x 1))) y)
  2)

;;; MV state leak regression tests (Bug B fix)

(deftest progn-mv-no-leak
  (let ((x 1))
    (ignore-errors (error "Boo!"))
    x)
  1)

(deftest unwind-protect.5
  (let ((x nil))
    (ignore-errors (error "Boo!"))
    (unwind-protect
        (setq x (list 2))
      nil)
    x)
  (2))

(do-tests-summary)
