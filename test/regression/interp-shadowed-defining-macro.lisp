;;; A package that shadows DEFCONSTANT (or DEFVAR, DEFPARAMETER, DEFUN, DEFMACRO) and
;;; defines its own macro of that name has the macro expanded by the interpreter, not
;;; treated as the CL defining form of the same name. cl-ppcre does this to DEFCONSTANT,
;;; and an emit-free build, which loads such a file through the interpreter, failed
;;; with "Undefined function: DEFCONSTANT".

(defpackage :interp-shadowed-defining-macro
  (:use :cl) (:shadow #:defconstant #:defvar))

(defun %isdm-eval (mode form)
  (let ((dotcl:*evaluator-mode* mode)
        (*package* (find-package :interp-shadowed-defining-macro)))
    (handler-case (eval (let ((*package* *package*)) (read-from-string form)))
      (error (e) (list :error (type-of e))))))

(defun %isdm-forms (tag)
  (format nil "(progn
     (defmacro defconstant (name value) `(cl:defconstant ,name (list :wrapped ,value)))
     (defmacro defvar (name value) `(cl:defparameter ,name (list :var ,value)))
     (defconstant isdm-c-~a 1)
     (defvar isdm-v-~a 2)
     (list isdm-c-~a isdm-v-~a))" tag tag tag tag))

(deftest interp-shadowed-defining-macro.interpret
  (%isdm-eval :interpret (%isdm-forms "i"))
  ((:wrapped 1) (:var 2)))

(deftest interp-shadowed-defining-macro.compile
  (%isdm-eval :compile (%isdm-forms "c"))
  ((:wrapped 1) (:var 2)))
