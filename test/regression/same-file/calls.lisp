;;; Compiled with COMPILE-FILE by test/regression/same-file-calls.lisp.

(in-package "CL-USER")

(defun %sfc-leaf (x) (list :leaf x))
(defun %sfc-caller (x) (car (list (%sfc-leaf x))))

(declaim (notinline %sfc-ni-leaf))
(defun %sfc-ni-leaf (x) (list :ni x))
(defun %sfc-ni-caller (x) (car (list (%sfc-ni-leaf x))))

(defun %sfc-d3-leaf (x) (declare (optimize (debug 3))) (list :d3 x))
(defun %sfc-d3-caller (x) (car (list (%sfc-d3-leaf x))))

(defun %sfc-bt-leaf (x) (declare (ignore x)) (values (dotcl:backtrace)))
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
