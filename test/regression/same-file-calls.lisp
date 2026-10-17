;;; COMPILE-FILE compiles a call to a function DEFUN defined earlier in the same
;;; file as a call straight to that function's body, taken while the name still
;;; holds the function the file defined (not for a NOTINLINE name or under
;;; (DEBUG 3)). The callee still records its frame, tail calls stay tail calls,
;;; and a redefinition, in the file or later, is seen as before.

(defvar *sfc-loaded*
  (when (compiled-mode-p)
    (let ((fasl (regression-temp-file "same-file-calls.fasl")))
      (compile-file "test/regression/same-file/calls.lisp" :output-file fasl)
      (load fasl)
      t)))

(defun %sfc-depth () (length (dotcl:backtrace)))

(deftest-compiled-only same-file.calls
  (list (cl-user::%sfc-caller 1) (cl-user::%sfc-forward 2) (cl-user::%sfc-mv 3)
        (cl-user::%sfc-uses-stub 4) (cl-user::%sfc-fib-caller 20))
  ((:leaf 1) (:later 2) (3 (1 2 3) 1) (:real 4) 6765))

(deftest-compiled-only same-file.frames
  (let ((base (%sfc-depth)))
    (list (remove-if-not (lambda (n) (search "%SFC-" n)) (cl-user::%sfc-bt-caller 1))
          (- (cl-user::%sfc-catcher 1) base)
          (- (%sfc-depth) base)))
  (("%SFC-BT-LEAF" "%SFC-BT-CALLER") 0 0))

(deftest-compiled-only same-file.tail-calls
  (cl-user::%sfc-od 1000001)
  :even)

(deftest-compiled-only same-file.redefinition
  (progn
    (eval '(defun cl-user::%sfc-leaf (x) (list :new x)))
    (eval '(defun cl-user::%sfc-ni-leaf (x) (list :new-ni x)))
    (eval '(defun cl-user::%sfc-d3-leaf (x) (list :new-d3 x)))
    (list (cl-user::%sfc-caller 1) (funcall 'cl-user::%sfc-leaf 1)
          (cl-user::%sfc-ni-caller 2) (cl-user::%sfc-d3-caller 3)))
  ((:new 1) (:new 1) (:new-ni 2) (:new-d3 3)))
