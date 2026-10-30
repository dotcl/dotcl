;;; A call to a same-file DEFUN whose later FTYPE declares a FIXNUM or
;;; DOUBLE-FLOAT result goes through the typed entry and leaves the raw value;
;;; the substituted copy of the body has to leave the raw value too, not the
;;; boxed object.

(when (emitting-mode-p)
  (let ((fasl (regression-temp-file "same-file-inline-typed-result.fasl")))
    (compile-file "test/regression/same-file/inline-typed-result.lisp" :output-file fasl)
    (load fasl)))

(deftest-emitting-only same-file-inline-typed-result.values
  (list (funcall '%sfit-long-caller t) (funcall '%sfit-long-caller nil)
        (funcall '%sfit-double-caller t)
        (funcall '%sfit-trim (vector 0 2) t) (funcall '%sfit-trim (vector 0) t))
  ((1 t) (0 t) (1.5d0 t) t nil))

(deftest-emitting-only same-file-inline-typed-result.redefined
  (progn
    (eval '(defun %sfit-long (x) (if x 5 0)))
    (list (funcall '%sfit-long-caller t) (funcall '%sfit-trim (vector 0 2) t)))
  ((5 nil) nil))
