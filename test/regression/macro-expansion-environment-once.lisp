;;; A macro that takes &ENVIRONMENT runs once, where the environment is real.

(defvar cl-user::*meo-calls* nil)

(defun meo-call-once (value calls)
  (list value
        (length calls)
        (first calls)))

(defun meo-run-compile-file (fn-name lines)
  (let ((src (regression-temp-file (concatenate 'string fn-name ".lisp")))
        (fasl (regression-temp-file (concatenate 'string fn-name ".fasl"))))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (dolist (line lines) (write-line line s)))
           (compile-file src :output-file fasl)
           (load fasl)
           (funcall (intern (string-upcase fn-name) :cl-user)))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl)))))

(defmacro meo-closure (var &environment env)
  (push (multiple-value-list (dotcl-cltl2:variable-information var env))
        cl-user::*meo-calls*)
  `(lambda () (incf ,var)))

(deftest-emitting-only meo-closure-compile-expands-once
  (let ((cl-user::*meo-calls* nil))
    (let ((f (compile nil '(lambda ()
                             (let ((x 0))
                               (let ((f (meo-closure x)))
                                 (funcall f) (funcall f) x))))))
      (meo-call-once (funcall f) cl-user::*meo-calls*)))
  (2 1 (:lexical t nil)))

(deftest-emitting-only meo-closure-compile-file-expands-once
  (let ((cl-user::*meo-calls* nil))
    (meo-call-once
     (meo-run-compile-file
      "meo-closure-cf-fn"
      '("(in-package :cl-user)"
        "(defvar cl-user::*meo-calls* nil)"
        "(defmacro meo-closure-cf (var &environment env)"
        "  (push (multiple-value-list (dotcl-cltl2:variable-information var env))"
        "        cl-user::*meo-calls*)"
        "  `(lambda () (incf ,var)))"
        "(defun meo-closure-cf-fn ()"
        "  (let ((x 0))"
        "    (let ((f (meo-closure-cf x)))"
        "      (funcall f) (funcall f) x)))"))
     cl-user::*meo-calls*))
  (2 1 (:lexical t nil)))

(defmacro meo-setq (var &environment env)
  (push 1 cl-user::*meo-calls*)
  (if (eq (first (multiple-value-list (dotcl-cltl2:variable-information var env)))
          :lexical)
      `(progn (setq ,var (+ ,var 1)) ,var)
      `(quote :not-lexical)))

(deftest-emitting-only meo-setq-compile-expands-once
  (let ((cl-user::*meo-calls* nil))
    (let ((f (compile nil '(lambda () (let ((x 1)) (meo-setq x))))))
      (meo-call-once (funcall f) cl-user::*meo-calls*)))
  (2 1 1))

(deftest-emitting-only meo-setq-compile-file-expands-once
  (let ((cl-user::*meo-calls* nil))
    (meo-call-once
     (meo-run-compile-file
      "meo-setq-cf-fn"
      '("(in-package :cl-user)"
        "(defvar cl-user::*meo-calls* nil)"
        "(defmacro meo-setq-cf (var &environment env)"
        "  (push 1 cl-user::*meo-calls*)"
        "  (if (eq (first (multiple-value-list (dotcl-cltl2:variable-information var env))) :lexical)"
        "      `(progn (setq ,var (+ ,var 1)) ,var)"
        "      `(quote :not-lexical)))"
        "(defun meo-setq-cf-fn () (let ((x 1)) (meo-setq-cf x)))"))
     cl-user::*meo-calls*))
  (2 1 1))

(defmacro meo-symbol-macro (&environment env)
  (push 1 cl-user::*meo-calls*)
  (let ((exp (macroexpand-1 'meo-x env)))
    `(quote ,(if (eq exp 'meo-x) :missing exp))))

(deftest-emitting-only meo-symbol-macro-compile-expands-once
  (let ((cl-user::*meo-calls* nil))
    (let ((f (compile nil '(lambda ()
                             (symbol-macrolet ((meo-x 42))
                               (meo-symbol-macro))))))
      (meo-call-once (funcall f) cl-user::*meo-calls*)))
  (42 1 1))

(deftest-emitting-only meo-symbol-macro-compile-file-expands-once
  (let ((cl-user::*meo-calls* nil))
    (meo-call-once
     (meo-run-compile-file
      "meo-symbol-macro-cf-fn"
      '("(in-package :cl-user)"
        "(defvar cl-user::*meo-calls* nil)"
        "(defmacro meo-symbol-macro-cf (&environment env)"
        "  (push 1 cl-user::*meo-calls*)"
        "  (let ((exp (macroexpand-1 'meo-x env)))"
        "    `(quote ,(if (eq exp 'meo-x) :missing exp))))"
        "(defun meo-symbol-macro-cf-fn ()"
        "  (symbol-macrolet ((meo-x 42)) (meo-symbol-macro-cf)))"))
     cl-user::*meo-calls*))
  (42 1 1))
