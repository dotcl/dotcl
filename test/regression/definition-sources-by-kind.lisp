;;; Definitions of different kinds under one name each keep their own
;;; location, and a function, method or class object leads to its own.

(defparameter *ds-src* nil)

(defun %ds-source ()
  (or *ds-src*
      (let ((src (merge-pathnames "ds-fixture.lisp" (regression-temp-dir))))
        (with-open-file (s src :direction :output :if-exists :supersede)
          (dolist (l '("(defpackage \"DS-KIND\" (:use \"CL\"))"                        ; 1
                       "(in-package \"DS-KIND\")"                                     ; 2
                       "(defclass foo () ())"                                         ; 3
                       "(defun foo (x) x)"                                            ; 4
                       "(define-compiler-macro foo (x) x)"                            ; 5
                       "(defgeneric bar (x))"                                         ; 6
                       "(defmethod bar ((x integer)) x)"                              ; 7
                       "(defmethod bar :around ((x (eql :k))) x)"                     ; 8
                       "(defvar *v* 1)"                                               ; 9
                       "(defun (setf baz) (v x) (declare (ignore x)) v)"              ; 10
                       "(defmacro mac (x) x)"                                         ; 11
                       "(defvar *inner* nil)"                                         ; 12
                       "(defmacro inner-fn () (let ((g (gensym \"INNER\"))) `(load-time-value (progn (defun ,g () 1) (setf *inner* #',g)))))" ; 13
                       "(defun outer () (inner-fn))"                                  ; 14
                       "(outer)"))
            (write-line l s)))
        (setf *ds-src* (namestring (truename src))))))

(defun %ds-sym (name) (find-symbol name "DS-KIND"))

(defun %ds-short (plist)
  (when plist
    (list* (getf plist :kind)
           (and (equal (namestring (probe-file (getf plist :file))) *ds-src*)
                (getf plist :line))
           (when (eq (getf plist :kind) :method)
             (list (getf plist :qualifiers) (getf plist :specializers))))))

(defun %ds-results ()
  (let ((foo (%ds-sym "FOO")) (bar (%ds-sym "BAR")))
    (list (mapcar #'%ds-short (dotcl:definition-sources foo))
          (mapcar #'%ds-short (dotcl:definition-sources bar))
          (mapcar #'%ds-short (dotcl:definition-sources (list 'setf (%ds-sym "BAZ"))))
          (mapcar #'%ds-short (dotcl:definition-sources (%ds-sym "*V*")))
          (mapcar #'%ds-short (dotcl:definition-sources "DS-KIND"))
          (mapcar (lambda (o) (%ds-short (dotcl:definition-source o)))
                  (list (fdefinition foo)
                        (compiler-macro-function foo)
                        (find-class foo)
                        (macro-function (%ds-sym "MAC"))
                        (fdefinition bar)
                        (find-method (fdefinition bar) '(:around) '((eql :k)))
                        (fdefinition (list 'setf (%ds-sym "BAZ")))
                        (symbol-value (%ds-sym "*INNER*"))
                        (find-package "DS-KIND"))))))

(defparameter *ds-expected*
  `(((:function 4) (:compiler-macro 5) (:class 3))
    ((:generic-function 6)
     (:method 7 () (integer))
     (:method 8 (:around) ((eql :k))))
    ((:function 10))
    ((:variable 9))
    ((:package 1))
    ((:function 4) (:compiler-macro 5) (:class 3) (:macro 11)
     (:generic-function 6) (:method 8 (:around) ((eql :k))) (:function 10)
     (:function 14) (:package 1))))

(defun %ds-matches (got)
  ;; INTEGER and the specializers are read in DS-KIND, so compare by name.
  (equal (prin1-to-string got) (prin1-to-string *ds-expected*)))

(deftest-emitting-only definition-sources-by-kind.load-source
  (progn
    (load (%ds-source))
    (let ((got (%ds-results)))
      (or (%ds-matches got) got)))
  t)

(deftest-emitting-only definition-sources-by-kind.load-fasl
  (let ((fasl (merge-pathnames "ds-fixture.fasl" (regression-temp-dir))))
    (load (%ds-source))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file *ds-src* :output-file fasl))
    (let ((*package* (find-package "DS-KIND")))
      (dolist (form (read-from-string
                     "((defclass foo) (defun foo) (define-compiler-macro foo) (defgeneric bar)
                       (defmethod bar ((x integer))) (defmethod bar :around ((x (eql :k))))
                       (defvar *v*) (defun (setf baz)) (defmacro mac)
                       (defpackage \"DS-KIND\"))"))
        (dotcl:record-definition-sources form "elsewhere.lisp" 999)))
    (load fasl)
    (let ((got (%ds-results)))
      (or (%ds-matches got) got)))
  t)
