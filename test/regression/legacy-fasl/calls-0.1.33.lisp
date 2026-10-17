;;; Compiled by dotcl 0.1.33 into calls-0.1.33.fasl, which is kept as that
;;; release wrote it: test/regression/legacy-fasl-calls.lisp loads it into the
;;; current runtime and calls back and forth. Do not regenerate the fasl.

(in-package "CL-USER")

(defun %lf-old-leaf (x) (list :old x))
(defun %lf-old-calls-new (x) (values (%lf-new-leaf x)))
(defun %lf-old-tail-new (x) (%lf-new-leaf x))
(defun %lf-old-bt (x) (values (%lf-new-bt x)))
(defun %lf-old-bt-tail (x) (%lf-new-bt x))
(defun %lf-old-catch (x)
  (handler-case (%lf-new-throw x)
    (error () (dotcl:backtrace))))
(defun %lf-old-closure-catch (x)
  (handler-case (funcall (%lf-new-make-closure) x)
    (error () (dotcl:backtrace))))
(defun %lf-old-ev (n) (if (= n 0) :even (%lf-new-od (1- n))))
(defun %lf-old-five (a b c d n) (if (= n 0) (list a b c d) (%lf-new-five b c d a (1- n))))
(defun %lf-old-unwind (x)
  (let ((seen nil))
    (catch '%lf-tag
      (unwind-protect (%lf-new-throw-tag x)
        (setq seen (dotcl:backtrace))))
    seen))
