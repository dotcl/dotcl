;;; Compiled with COMPILE-FILE by test/regression/same-file-calls.lisp.

(in-package "CL-USER")

(defun %sfc-leaf (x) (list :leaf x))
(defun %sfc-caller (x) (car (list (%sfc-leaf x))))

(declaim (notinline %sfc-ni-leaf))
(defun %sfc-ni-leaf (x) (list :ni x))
(defun %sfc-ni-caller (x) (car (list (%sfc-ni-leaf x))))

(defun %sfc-d3-leaf (x) (declare (optimize (debug 3))) (list :d3 x))
(defun %sfc-d3-caller (x) (car (list (%sfc-d3-leaf x))))

(defun %sfc-bt-leaf (x)
  (declare (ignore x) (optimize (debug 3)))
  (values (dotcl:backtrace)))
(defun %sfc-bt-caller (x) (values (%sfc-bt-leaf x)))

(defun %sfc-thrower (x) (values (error "same-file ~A" x)))
(defun %sfc-mid (x) (values (%sfc-thrower x)))
(defun %sfc-catcher (x)
  (handler-case (%sfc-mid x)
    (error () (values (length (dotcl:backtrace))))))

(defun %sfc-ev (n) (if (= n 0) :even (let ((r (%sfc-od (1- n)))) r)))
(defun %sfc-od (n) (if (= n 0) :odd (let ((r (%sfc-ev (1- n)))) r)))

(defun %sfc-values () (values 1 2 3))
(defun %sfc-mv (x) (list x (multiple-value-list (%sfc-values)) (%sfc-values)))

(defun %sfc-stub (x) (list :stub x))
(defun %sfc-uses-stub (x) (car (list (%sfc-stub x))))
(defun %sfc-stub (x) (list :real x))

(defun %sfc-fib (n) (if (< n 2) n (+ (%sfc-fib (- n 1)) (%sfc-fib (- n 2)))))
(defun %sfc-fib-caller (n) (car (list (%sfc-fib n))))

(defun %sfc-forward (x) (values (%sfc-later x)))
(defun %sfc-later (x) (list :later x))

(defun %sfc-fw-caller (x) (car (list (%sfc-fw-leaf x))))
(defun %sfc-fw-mv (x) (list x (multiple-value-list (%sfc-fw-values)) (%sfc-fw-values)))
(defun %sfc-fw-undefined (x) (car (list (%sfc-fw-never x))))
(defun %sfc-fw-arity (x) (car (list (%sfc-fw-leaf x x))))
(defun %sfc-fw-uses-stub (x) (car (list (%sfc-fw-stub x))))
(declaim (notinline %sfc-fw-ni-leaf))
(defun %sfc-fw-ni-caller (x) (car (list (%sfc-fw-ni-leaf x))))
(defun %sfc-fw-traced-caller (x) (car (list (%sfc-fw-traced x))))
(defun %sfc-fw-bt-caller (x) (values (%sfc-fw-bt-leaf x)))
(defun %sfc-fw-ev (n) (if (= n 0) :even (car (list (%sfc-fw-od (1- n))))))
(defun %sfc-fw-leaf (x) (list :fw-leaf x))
(defun %sfc-fw-values () (values 1 2 3))
(defun %sfc-fw-stub (x) (list :fw-stub x))
(defun %sfc-fw-stub (x) (list :fw-real x))
(defun %sfc-fw-ni-leaf (x) (list :fw-ni x))
(defun %sfc-fw-traced (x) (list :fw-traced x))
(defun %sfc-fw-bt-leaf (x)
  (declare (ignore x) (optimize (debug 3)))
  (values (dotcl:backtrace)))
(defun %sfc-fw-od (n) (if (= n 0) :odd (car (list (%sfc-fw-ev (1- n))))))
(defun %sfc-fw-car-caller (x) (list (%sfc-fw-car x)))
(defun %sfc-fw-car (x) (car x))
