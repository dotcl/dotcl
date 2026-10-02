;;; Regression: FLET of a local function named by a symbol the compiler's own
;;; package inherits (CL:TYPE, CL:SPEED, ...) shadowed a VARIABLE of the same
;;; name in the body. CL is a Lisp-2: the variable must still be read.
;;; CLHS 11.1.2.1.2.1 allows FLET of a CL symbol that is not a standardized
;;; function, macro or special operator, so only such names are used here.
;;; 3d-math's DEFINE-VEC-CONSTRUCTORS binds a local function TYPE inside a
;;; macro whose parameter is also TYPE.

(defun flet-cl-name-1 (type)
  (flet ((type (size) (list size type)))
    (list type (type 2))))

(deftest flet-cl-symbol-name-variable-1
  (flet-cl-name-1 'f64)
  (f64 (2 f64)))

(deftest flet-cl-symbol-name-variable-speed
  (funcall (lambda (speed) (flet ((speed (s) (list s speed))) (list speed (speed 1)))) 'v)
  (v (1 v)))

(deftest flet-cl-symbol-name-variable-let
  (let ((type 'v)) (flet ((type (s) (list s type))) (list type (type 1))))
  (v (1 v)))

(defmacro flet-cl-name-macro (type)
  (flet ((type (size) (list size type)))
    `(quote ,(list type (type 2)))))

(deftest flet-cl-symbol-name-variable-macro
  (flet-cl-name-macro f32)
  (f32 (2 f32)))

;; The function is still reached as a value and from a closure.
(deftest flet-cl-symbol-name-function-value
  (flet ((type (x) (* x 2)))
    (let ((f (lambda () #'type)))
      (list (funcall #'type 3) (funcall (funcall f) 4))))
  (6 8))

;; A :REPORT given as a symbol names a function, as for :INTERACTIVE and :TEST.
(deftest restart-case-report-symbol-flet
  (flet ((%rep (s) (format s "local report")))
    (restart-case
        (princ-to-string (find-restart 'flet-cl-report-restart))
      (flet-cl-report-restart () :report %rep nil)))
  "local report")

(deftest restart-case-report-symbol-cl-name
  (flet ((optimize (s) (format s "shadowed optimize")))
    (restart-case
        (princ-to-string (find-restart 'flet-cl-report-restart))
      (flet-cl-report-restart () :report optimize nil)))
  "shadowed optimize")
