;;; Definitions inside a top level form's body, (let () (defmethod ...)) and
;;; the like, are located at that form. They are found when the compiler
;;; compiles them, so not on the emit-free build.

(defun %dsb-source ()
  (let ((src (merge-pathnames "dsb-fixture.lisp" (regression-temp-dir))))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l '("(defpackage \"DS-BODY\" (:use \"CL\"))"                         ; 1
                   "(in-package \"DS-BODY\")"                                      ; 2
                   "(let () (defmethod m2 ((x integer)) x))"                       ; 3
                   "(let ((v 1)) (defvar *v2* v) (defmacro mm2 () 1) (defgeneric g3 (x) (:method ((x string)) x)) (defclass c3 () ()))" ; 4
                   "(flet ((f () 1)) (defun f2 () (f)))"))                         ; 5
        (write-line l s)))
    (namestring (truename src))))

(defun %dsb-results ()
  (write-to-string
   (mapcar (lambda (n)
             (mapcar (lambda (p)
                       (list* (getf p :kind) (getf p :line)
                              (when (eq (getf p :kind) :method) (list (getf p :specializers)))))
                     (dotcl:definition-sources (find-symbol n "DS-BODY"))))
           '("M2" "*V2*" "MM2" "G3" "C3" "F2"))
   :pretty nil :escape t))

(defparameter *dsb-expected*
  (concatenate 'string
               "(((:METHOD 3 (INTEGER))) ((:VARIABLE 4)) ((:MACRO 4)) "
               "((:GENERIC-FUNCTION 4) (:METHOD 4 (STRING))) ((:CLASS 4)) ((:FUNCTION 5)))"))

(deftest-emitting-only definition-sources-in-body.load-source
  (progn
    (when (find-package "DS-BODY") (delete-package "DS-BODY"))
    (load (%dsb-source))
    (let ((got (%dsb-results)))
      (or (string= got *dsb-expected*) got)))
  t)

(deftest-emitting-only definition-sources-in-body.load-fasl
  (let ((fasl (merge-pathnames "dsb-fixture.fasl" (regression-temp-dir)))
        (src (%dsb-source)))
    (when (find-package "DS-BODY") (delete-package "DS-BODY"))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    (delete-package "DS-BODY")
    (load fasl)
    (let ((got (%dsb-results)))
      (or (string= got *dsb-expected*) got)))
  t)
