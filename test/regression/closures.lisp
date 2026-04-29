;;; Closure and free-variable capture regression tests

;;; Basic closure over immutable variable
(deftest closure-basic
  (let ((x 10))
    (funcall (lambda () x)))
  10)

;;; Closure captures mutable variable (setq)
(deftest closure-mutation
  (let ((n 0))
    (let ((inc (lambda () (setq n (+ n 1)))))
      (funcall inc)
      (funcall inc)
      (funcall inc)
      n))
  3)

;;; Multiple closures share mutable state
(deftest closure-shared-state
  (let ((n 0))
    (let ((inc (lambda () (setq n (+ n 1))))
          (get (lambda () n)))
      (funcall inc)
      (funcall inc)
      (funcall get)))
  2)

;;; Closure returned from function (upward funarg)
(defun make-adder (x)
  (lambda (y) (+ x y)))

(deftest closure-upward-funarg
  (funcall (make-adder 5) 3)
  8)

;;; Closure over loop variable captured per-iteration (do loop)
(deftest closure-do-loop-capture
  (let ((fns nil))
    (do ((i 0 (+ i 1)))
        ((= i 3))
      (let ((captured i))
        (push (lambda () captured) fns)))
    (mapcar #'funcall (reverse fns)))
  (0 1 2))

;;; Nested closures
(deftest closure-nested
  (let ((x 1))
    (let ((f (lambda ()
               (let ((y 2))
                 (lambda () (+ x y))))))
      (funcall (funcall f))))
  3)

;;; Closure in labels
(deftest closure-labels
  (labels ((counter (n)
             (lambda () n)))
    (funcall (counter 42)))
  42)
