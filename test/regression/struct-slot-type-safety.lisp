;;; A structure slot's declared :TYPE under (safety 0): both evaluators agree.
;;;
;;; The compiler drops the store check of a typed slot under (optimize (safety 0))
;;; -- a violated declaration is undefined (CLHS 3.3.1) and safety 0 is where the
;;; writer asked not to be checked, which is also what SBCL does. The
;;; interpreter used to check every store whatever the policy, so the same
;;; function stored the value when compiled and signalled when interpreted. An
;;; emit-free build interprets every DEFUN, so there the lenient compiled
;;; behaviour turned strict: code that worked on a desktop build failed on the
;;; shipped one.
;;;
;;; The interpreter now takes SAFETY at the granularity the compiler does: the
;;; function body's own declaration, else the DECLAIM in force. A nested LAMBDA
;;; does not inherit an outer function's declaration (a compiled closure starts
;;; from its own), and a keyword constructor called at (safety 0) stores
;;; unchecked in both, because the compiler builds that call in place.
;;;
;;; Every case runs through EVAL in both modes; on an emit-free build both are
;;; the interpreter, which is the point of running them there too.

(defstruct sstp (s "" :type string) (u nil))

(defun %sstp-both (form)
  "FORM's outcome evaluated once compiled and once interpreted, as a list of two."
  (flet ((run (mode)
           (let ((dotcl:*evaluator-mode* mode))
             (handler-case (eval form)
               (type-error () :type-error)
               (error (e) (list :other (type-of e)))))))
    (list (run :compile) (run :interpret))))

;; The declaration in force is the default: the store is checked.
(deftest struct-slot-type-safety.default-checks
  (%sstp-both '(funcall (lambda (v) (let ((s (make-sstp))) (setf (sstp-s s) v) (sstp-s s)))
                42))
  (:type-error :type-error))

;; (safety 0) in the function: the store goes through unchecked.
(deftest struct-slot-type-safety.safety-0-stores
  (%sstp-both '(funcall (lambda (v)
                          (declare (optimize (safety 0)))
                          (let ((s (make-sstp))) (setf (sstp-s s) v) (sstp-s s)))
                42))
  (42 42))

;; A nested lambda without its own declaration does not inherit (safety 0).
(deftest struct-slot-type-safety.nested-lambda-does-not-inherit
  (%sstp-both '(funcall (funcall (lambda ()
                                   (declare (optimize (safety 0)))
                                   (lambda (v) (let ((s (make-sstp)))
                                                 (setf (sstp-s s) v) (sstp-s s)))))
                42))
  (:type-error :type-error))

;; ...and its own declaration counts inside a checked function.
(deftest struct-slot-type-safety.nested-lambda-own-declaration
  (%sstp-both '(funcall (funcall (lambda ()
                                   (lambda (v)
                                     (declare (optimize (safety 0)))
                                     (let ((s (make-sstp)))
                                       (setf (sstp-s s) v) (sstp-s s)))))
                42))
  (42 42))

;; A body the interpreter wraps in a thunk of its own (HANDLER-BIND) is still
;; the enclosing function's code.
(deftest struct-slot-type-safety.handler-bind-body
  (%sstp-both '(funcall (lambda (v)
                          (declare (optimize (safety 0)))
                          (handler-bind ((warning #'muffle-warning))
                            (let ((s (make-sstp))) (setf (sstp-s s) v) (sstp-s s))))
                42))
  (42 42))

;; A keyword constructor call at (safety 0): the compiler constructs in place
;; and the check follows the caller's safety.
(deftest struct-slot-type-safety.constructor-at-safety-0
  (%sstp-both '(funcall (lambda (v)
                          (declare (optimize (safety 0)))
                          (sstp-s (make-sstp :s v)))
                42))
  (42 42))

(deftest struct-slot-type-safety.constructor-default-checks
  (%sstp-both '(funcall (lambda (v) (sstp-s (make-sstp :s v))) 42))
  (:type-error :type-error))

;; A correct value is of course fine either way, and an untyped slot is never
;; checked.
(deftest struct-slot-type-safety.well-typed-and-untyped
  (%sstp-both '(funcall (lambda (v)
                          (let ((s (make-sstp :s "ok" :u v)))
                            (setf (sstp-u s) (list v))
                            (list (sstp-s s) (sstp-u s))))
                42))
  (("ok" (42)) ("ok" (42))))

;; (THE FIXNUM ...) is dropped under (safety 0) the same way.
(deftest struct-slot-type-safety.the-fixnum-at-safety-0
  (%sstp-both '(funcall (lambda (v) (declare (optimize (safety 0))) (the fixnum v)) "x"))
  ("x" "x"))
