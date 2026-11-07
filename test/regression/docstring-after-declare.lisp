;;; A docstring may follow the declarations (CLHS 3.4.11): DEFUN, DEFMACRO, DEFMETHOD, DEFTYPE,
;;; DEFINE-COMPILER-MACRO, DEFINE-SETF-EXPANDER and DEFINE-METHOD-COMBINATION recorded it only
;;; when it came first. A string that is the only form after the declarations is a value, not
;;; documentation.

(defun dad-f1 (x) (declare (ignore x)) "Doc f1." 1)
(defun dad-f2 (x) "Doc f2." (declare (ignore x)) 1)
(defun dad-f3 (x) (declare (ignore x)) "only a value")
(defun dad-f4 (x) (declare (ignore x)) (declare (optimize speed)) "Doc f4." 1)
(defmacro dad-m1 (x) (declare (ignore x)) "Doc m1." 1)
(defgeneric dad-g1 (x))
(defmethod dad-g1 (x) (declare (ignore x)) "Doc method." 1)
(deftype dad-t1 (x) (declare (ignore x)) "Doc t1." 'integer)
(define-compiler-macro dad-cm1 (x) (declare (ignore x)) "Doc cm1." 1)
(define-setf-expander dad-se1 (x) (declare (ignore x)) "Doc se1." (values nil nil nil nil nil))
(define-method-combination dad-mc1 () ((all *)) (declare (ignorable all)) "Doc mc1." `(progn ,@all))

(deftest docstring-after-declare.functions
  (list (documentation 'dad-f1 'function) (documentation 'dad-f2 'function)
        (documentation 'dad-f3 'function) (documentation 'dad-f4 'function)
        (documentation 'dad-m1 'function)
        (documentation (find-method #'dad-g1 nil (list t)) t))
  ("Doc f1." "Doc f2." nil "Doc f4." "Doc m1." "Doc method."))

(deftest docstring-after-declare.other-kinds
  (list (documentation 'dad-t1 'type) (documentation 'dad-cm1 'compiler-macro)
        (documentation 'dad-se1 'setf) (documentation 'dad-mc1 'method-combination))
  ("Doc t1." "Doc cm1." "Doc se1." "Doc mc1."))

(deftest docstring-after-declare.bodies-still-run
  (list (dad-f1 0) (dad-f3 0) (dad-g1 0) (dad-m1 0) (typep 3 '(dad-t1 0)))
  (1 "only a value" 1 1 t))
