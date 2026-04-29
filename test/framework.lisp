;;; framework.lisp — Lightweight deftest framework for ANSI test porting
;;;
;;; Load before test files:
;;;   ros cil-compile.ros test/framework.lisp test/ansi/cons.lisp output.sil

(defvar *test-count* 0)
(defvar *pass-count* 0)
(defvar *fail-count* 0)
(defvar *fail-names* nil)

(defmacro deftest (name form &rest expected)
  (let ((result-var (gensym "R"))
        (expected-var (gensym "E")))
    `(let ((,result-var (multiple-value-list ,form))
           (,expected-var '(,@expected)))
       (incf *test-count*)
       (if (equal ,result-var ,expected-var)
           (incf *pass-count*)
           (progn
             (incf *fail-count*)
             (push ',name *fail-names*)
             (print (list 'FAIL ',name))
             (print (list 'EXPECTED ,expected-var))
             (print (list 'GOT ,result-var)))))))

(defmacro do-tests-summary ()
  '(progn
     (print (list *pass-count* 'PASSED *fail-count* 'FAILED
                  'OF *test-count* 'TOTAL))
     (if (= *fail-count* 0)
         (print 'ALL-TESTS-PASSED)
         (progn (print (list 'FAILED-TESTS *fail-names*))))))

;;; ansi-test compatibility helpers
(defun notnot (x) (not (not x)))
(defun eqt (x y) (notnot (eq x y)))
(defun eqlt (x y) (notnot (eql x y)))
(defun equalt (x y) (notnot (equal x y)))

;;; signals-error: returns T if form signals a condition of the given type
(defmacro signals-error (form condition-type)
  (let ((c (gensym "C")))
    `(handler-case (progn ,form nil)
       (,condition-type (,c) t))))
