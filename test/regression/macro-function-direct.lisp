;;; MACRO-FUNCTION with one argument no longer allocates the closure for a compiler-macro
;;; wrapper on every call. Macros, non-macros, compiler macros, special operators and the errors
;;; are as before; the expected values are those of the build before the change.

(defun mfn-fn (x) x)
(define-compiler-macro mfn-fn (&whole w x) (declare (ignore w)) x)
(defmacro mfn-mac (x) (list 'quote x))

(deftest macro-function-direct.1
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (FUNCTIONP (MACRO-FUNCTION (QUOTE WHEN))))) (error () (list :error)))
  (T))

(deftest macro-function-direct.2
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (MACRO-FUNCTION (QUOTE CAR)))) (error () (list :error)))
  (NIL))

(deftest macro-function-direct.3
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (MACRO-FUNCTION (QUOTE MFN-UNDEFINED-XYZ)))) (error () (list :error)))
  (NIL))

(deftest macro-function-direct.4
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (LIST (NULL (MACRO-FUNCTION (QUOTE MFN-FN))) (AND (MACRO-FUNCTION (QUOTE MFN-FN)) (FUNCTIONP (MACRO-FUNCTION (QUOTE MFN-FN))))))) (error () (list :error)))
  ((T NIL)))

(deftest macro-function-direct.5
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (MACROEXPAND-1 (QUOTE (WHEN A B))))) (error () (list :error)))
  ((IF A (PROGN B)) T))

(deftest macro-function-direct.6
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (FUNCALL (MACRO-FUNCTION (QUOTE MFN-MAC)) (QUOTE (MFN-MAC Z)) NIL))) (error () (list :error)))
  ((QUOTE Z)))

(deftest macro-function-direct.7
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (MACRO-FUNCTION 5) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (TYPE-ERROR))

(deftest macro-function-direct.8
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (NULL (MACRO-FUNCTION (QUOTE IF))))) (error () (list :error)))
  (T))

(deftest macro-function-direct.9
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (MACRO-FUNCTION) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (PROGRAM-ERROR))

