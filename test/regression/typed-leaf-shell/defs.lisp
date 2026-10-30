;;; Compiled with COMPILE-FILE by typed-leaf-shell.lisp.

(declaim (notinline %tlsf-add %tlsf-r %tlsf-err %tlsf-cmp))
(declaim (ftype (function (double-float double-float) double-float) %tlsf-add))
(defun %tlsf-add (a b) (+ a b))
(declaim (ftype (function ((integer 0 10) fixnum) fixnum) %tlsf-r))
(defun %tlsf-r (a b) (+ a b))
(defun %tlsf-err (x) (error "tlsf ~A" x))
(declaim (ftype (function (double-float double-float) t) %tlsf-cmp))
(defun %tlsf-cmp (a b) (if (< a b) (car (load-time-value (list :lt))) :ge))
(defun %tlsf-t0 (f g h x) (funcall f g h x))
(defun %tlsf-t2 (h x) (funcall h x 1d0))
