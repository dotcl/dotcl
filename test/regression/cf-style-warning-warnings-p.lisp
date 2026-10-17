;;; COMPILE-FILE's second value (warnings-p) is true when a STYLE-WARNING was
;;; the only condition, and the third (failure-p) stays NIL (CLHS COMPILE-FILE:
;;; a STYLE-WARNING is a WARNING). The expected values are SBCL 2.6.8's on the
;;; same files.

(define-condition cfsw-style (style-warning) ())
(define-condition cfsw-base (condition) ())
(define-condition cfsw-mixed (cfsw-base style-warning) ())

(defun cfsw-compile (name text)
  (let ((path (concatenate 'string (namestring (regression-temp-dir)) "/" name))
        (*error-output* (make-broadcast-stream)))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string text s))
    (multiple-value-bind (fasl warnings-p failure-p) (compile-file path)
      (list (and fasl :fasl) (and warnings-p t) (and failure-p t)))))

(deftest-emitting-only compile-file-warnings-p.style-warning-from-macro
  (cfsw-compile "cfsw1.lisp"
                "(defmacro cfsw-m1 () (warn 'cfsw-style) nil)
                 (defun cfsw-f1 () (cfsw-m1))")
  (:fasl t nil))

(deftest-emitting-only compile-file-warnings-p.style-warning-compile-time
  (cfsw-compile "cfsw2.lisp"
                "(eval-when (:compile-toplevel) (warn (make-condition 'style-warning)))")
  (:fasl t nil))

(deftest-emitting-only compile-file-warnings-p.undefined-declared-type
  (cfsw-compile "cfsw3.lisp"
                "(defun cfsw-f3 (x) (declare (type cfsw-no-such-type x)) x)")
  (:fasl t nil))

(deftest-emitting-only compile-file-warnings-p.style-warning-second-parent
  (cfsw-compile "cfsw4.lisp"
                "(defmacro cfsw-m4 () (warn 'cfsw-mixed) nil)
                 (defun cfsw-f4 () (cfsw-m4))")
  (:fasl t nil))

(deftest-emitting-only compile-file-warnings-p.full-warning
  (cfsw-compile "cfsw5.lisp"
                "(defmacro cfsw-m5 () (warn \"cfsw full\") nil)
                 (defun cfsw-f5 () (cfsw-m5))")
  (:fasl t t))

(deftest-emitting-only compile-file-warnings-p.clean
  (cfsw-compile "cfsw6.lisp" "(defun cfsw-f6 () 1)")
  (:fasl nil nil))
