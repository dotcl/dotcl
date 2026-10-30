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

;;; A call to a function DEFUNed later in the same file goes the same way, through
;;; a stub the later DEFUN fills.

(deftest-compiled-only same-file.forward-calls
  (list (cl-user::%sfc-fw-caller 1) (cl-user::%sfc-fw-mv 2)
        (cl-user::%sfc-fw-uses-stub 3) (cl-user::%sfc-fw-ev 2000)
        (remove-if-not (lambda (n) (search "%SFC-FW-" n)) (cl-user::%sfc-fw-bt-caller 1)))
  ((:fw-leaf 1) (2 (1 2 3) 1) (:fw-real 3) :even
   ("%SFC-FW-BT-LEAF" "%SFC-FW-BT-CALLER")))

(deftest-compiled-only same-file.forward-not-defined
  (list (handler-case (cl-user::%sfc-fw-undefined 1)
          (undefined-function (c) (cell-error-name c)))
        (handler-case (cl-user::%sfc-fw-arity 1)
          (program-error () :arity))
        (progn (eval '(defun cl-user::%sfc-fw-never (x) (list :never x)))
               (cl-user::%sfc-fw-undefined 2))
        (cl-user::%sfc-fw-car-caller '(1))
        (handler-case (cl-user::%sfc-fw-car-caller 5)
          (type-error (c) (type-error-datum c))))
  (cl-user::%sfc-fw-never :arity (:never 2) (1) 5))

(deftest-compiled-only same-file.forward-redefinition
  (let ((r '()))
    (trace cl-user::%sfc-fw-traced)
    (let ((*trace-output* (make-broadcast-stream)))
      (push (cl-user::%sfc-fw-traced-caller 1) r))
    (untrace cl-user::%sfc-fw-traced)
    (eval '(defun cl-user::%sfc-fw-leaf (x) (list :new x)))
    (eval '(defun cl-user::%sfc-fw-ni-leaf (x) (list :new-ni x)))
    (setf (fdefinition 'cl-user::%sfc-fw-traced) (lambda (x) (list :setf x)))
    (list* (cl-user::%sfc-fw-caller 2) (cl-user::%sfc-fw-ni-caller 3)
           (cl-user::%sfc-fw-traced-caller 4) r))
  ((:new 2) (:new-ni 3) (:setf 4) (:fw-traced 1)))
