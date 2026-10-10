;;; COMPILE-FILE runs a top level DECLAIM, PROCLAIM, EXPORT and the other
;;; package operations at compile time. When their arguments are constants the
;;; function is called directly instead of assembling a method for the form.
;;; The effects must be the same: a macro later in the file sees the export and
;;; the special proclamations, a package operation that fails at compile time
;;; does not fail COMPILE-FILE, and loading the fasl still makes the
;;; proclamations. COMPILE-FILE needs the emitter, so not on the emit-free build.

(defun %cfct-compile (name lines &optional load)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l s)))
    (let* ((*error-output* (make-broadcast-stream))
           (out (compile-file src :output-file fasl)))
      (when (and out load) (load out))
      (and out t))))

(defun %cfct-special-p (sym)
  (eql 1 (ignore-errors
          (eval `(let ((,sym 1)) (funcall (lambda () (symbol-value ',sym))))))))

(defvar *cfct-seen* nil)

(deftest-emitting-only cf-constant-compile-time-forms.seen-by-later-macro
  (progn
    (setf *cfct-seen* nil)
    (%cfct-compile
     "cfct-a"
     '("(defpackage \"CFCT-P\" (:use \"CL\"))"
       "(in-package \"CFCT-P\")"
       "(export 'cfct-sym)"
       "(export '(cfct-s2 cfct-s3) \"CFCT-P\")"
       "(declaim (ftype (function (t) t) cfct-f) (special cfct-var) (inline cfct-g))"
       "(proclaim '(special cfct-var2))"
       "(defmacro cfct-probe ()"
       "  (setf cl-user::*cfct-seen*"
       "        (list (nth-value 1 (find-symbol \"CFCT-SYM\" \"CFCT-P\"))"
       "              (nth-value 1 (find-symbol \"CFCT-S3\" \"CFCT-P\"))"
       "              (cl-user::%cfct-special-p 'cfct-var)"
       "              (cl-user::%cfct-special-p 'cfct-var2)))"
       "  nil)"
       "(cfct-probe)"))
    *cfct-seen*)
  (:external :external t t))

(deftest-emitting-only cf-constant-compile-time-forms.failing-package-operation
  (%cfct-compile
   "cfct-b"
   '("(export 'cfct-nowhere \"CFCT-NO-SUCH-PACKAGE\")"
     "(defun cfct-b-f () 1)"))
  t)

(deftest-emitting-only cf-constant-compile-time-forms.load-proclaims
  (progn
    (%cfct-compile
     "cfct-c"
     '("(declaim (special cfct-c-var))"
       "(defun cfct-c-f () 1)")
     t)
    (%cfct-special-p (find-symbol "CFCT-C-VAR" "CL-USER")))
  t)
