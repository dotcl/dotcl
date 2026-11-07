;;; Compiling a call of a COMMON-LISP function with a wrong number of arguments warns, as SBCL
;;; does. Calls that are fine, and local functions named like a CL function, do not.

(defun %clcaw-warnings (form)
  (let ((texts '()))
    (handler-bind ((warning (lambda (w) (push (princ-to-string w) texts) (muffle-warning w))))
      (compile nil form))
    (nreverse texts)))

(deftest-emitting-only cl-call-arity-warning.too-many-and-too-few
  (list (length (%clcaw-warnings '(lambda (x) (car x x))))
        (length (%clcaw-warnings '(lambda (x) (cons x))))
        (length (%clcaw-warnings '(lambda () (length))))
        (length (%clcaw-warnings '(lambda (x) (gethash x))))
        (length (%clcaw-warnings '(lambda (x) (last x 1 2))))
        (length (%clcaw-warnings '(lambda () (+ ))))
        (length (%clcaw-warnings '(lambda () (-)))))
  (1 1 1 1 1 0 1))

(deftest-emitting-only cl-call-arity-warning.message
  (let ((w (first (%clcaw-warnings '(lambda (x) (car x x))))))
    (list (and (search "CAR" w) t) (and (search "2 arguments" w) t) (and (search "exactly 1" w) t)))
  (t t t))

(deftest-emitting-only cl-call-arity-warning.valid-calls-quiet
  (list (%clcaw-warnings '(lambda (x) (car x)))
        (%clcaw-warnings '(lambda (x) (gethash x x)))
        (%clcaw-warnings '(lambda (x) (gethash x x 1)))
        (%clcaw-warnings '(lambda (x) (last x)))
        (%clcaw-warnings '(lambda (x) (last x 2)))
        (%clcaw-warnings '(lambda (x) (format nil "~A" x)))
        (%clcaw-warnings '(lambda (x) (flet ((car (a b) (list a b))) (car x x))))
        (%clcaw-warnings '(lambda (x) (labels ((cons (a) a)) (cons x))))
        (%clcaw-warnings '(lambda (x) (macrolet ((car (a b) `(list ,a ,b))) (car x x))))
        (%clcaw-warnings '(lambda (x) (flet ((last (a b c) (list a b c))) (last x x x))))
        (%clcaw-warnings '(lambda (x) (let ((car x)) (declare (ignorable car)) (funcall #'identity x))))
        (%clcaw-warnings '(lambda (x) (apply #'car x nil)))
        (%clcaw-warnings '(lambda (x) (mapcar #'cons x x))))
  (nil nil nil nil nil nil nil nil nil nil nil nil nil))

;;; A symbol of another package that merely has the name of a CL function (a shadowing import
;;; or a library's own function) is not the CL function.
(defpackage :%clcaw-pkg (:use :cl) (:shadow #:car #:position #:length))
(defun %clcaw-pkg::car (a b) (list a b))
(defun %clcaw-pkg::position (a) a)

(deftest-emitting-only cl-call-arity-warning.shadowed-symbols-quiet
  (let ((form `(lambda (x) (,(find-symbol "CAR" :%clcaw-pkg) x x)))
        (form2 `(lambda (x) (,(find-symbol "POSITION" :%clcaw-pkg) x))))
    (list (%clcaw-warnings form) (%clcaw-warnings form2)
          (eq (find-symbol "CAR" :%clcaw-pkg) 'car)))
  (nil nil nil))
