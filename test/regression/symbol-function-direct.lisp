;;; SYMBOL-FUNCTION, FBOUNDP and SYMBOL-PLIST with one argument are direct calls, and
;;; SYMBOL-FUNCTION / FDEFINITION no longer build a closure on every call. Functions, macros,
;;; special operators, undefined names, NIL and T, non-symbols, wrong argument counts, local
;;; functions of the same name and the function-object forms are as before; the expected values
;;; are those of the build before the change.

(defun sfn-user (x) (* x 2))

(deftest symbol-function-direct.1
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (FUNCTIONP (SYMBOL-FUNCTION (QUOTE CAR))))) (error () (list :error)))
  (T))

(deftest symbol-function-direct.2
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (EQ (SYMBOL-FUNCTION (QUOTE CAR)) (FUNCTION CAR)))) (error () (list :error)))
  (T))

(deftest symbol-function-direct.3
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (FUNCALL (SYMBOL-FUNCTION (QUOTE SFN-USER)) 4))) (error () (list :error)))
  (8))

(deftest symbol-function-direct.4
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (EQ (SYMBOL-FUNCTION (QUOTE SFN-USER)) (FUNCTION SFN-USER)))) (error () (list :error)))
  (T))

(deftest symbol-function-direct.5
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (FUNCTIONP (SYMBOL-FUNCTION (QUOTE WHEN))))) (error () (list :error)))
  (T))

(deftest symbol-function-direct.6
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (FUNCTIONP (SYMBOL-FUNCTION (QUOTE IF))))) (error () (list :error)))
  (T))

(deftest symbol-function-direct.7
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (FUNCALL (SYMBOL-FUNCTION (QUOTE IF)) 1) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (ERROR))

(deftest symbol-function-direct.8
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (SYMBOL-FUNCTION (QUOTE SFN-UNDEFINED-XYZ)) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (UNDEFINED-FUNCTION))

(deftest symbol-function-direct.9
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (SYMBOL-FUNCTION 5) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (TYPE-ERROR))

(deftest symbol-function-direct.10
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (SYMBOL-FUNCTION "car") (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (TYPE-ERROR))

(deftest symbol-function-direct.11
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (SYMBOL-FUNCTION (QUOTE (SETF CAR))) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (TYPE-ERROR))

(deftest symbol-function-direct.12
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (SYMBOL-FUNCTION NIL) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (UNDEFINED-FUNCTION))

(deftest symbol-function-direct.13
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (SYMBOL-FUNCTION T) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (UNDEFINED-FUNCTION))

(deftest symbol-function-direct.14
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (EVAL (QUOTE (SYMBOL-FUNCTION))) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (PROGRAM-ERROR))

(deftest symbol-function-direct.15
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (EVAL (QUOTE (SYMBOL-FUNCTION (QUOTE CAR) (QUOTE CDR)))) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (PROGRAM-ERROR))

(deftest symbol-function-direct.16
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (FUNCTIONP (FDEFINITION (QUOTE IF))))) (error () (list :error)))
  (T))

(deftest symbol-function-direct.17
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (FDEFINITION (QUOTE SFN-UNDEFINED-XYZ)) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (UNDEFINED-FUNCTION))

(deftest symbol-function-direct.18
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (LIST (FBOUNDP (QUOTE CAR)) (FBOUNDP (QUOTE WHEN)) (FBOUNDP (QUOTE IF)) (FBOUNDP (QUOTE SFN-UNDEFINED-XYZ)) (FBOUNDP (QUOTE SFN-USER)) (FBOUNDP NIL) (FBOUNDP T) (FBOUNDP (QUOTE (SETF CAR))) (FBOUNDP (QUOTE (SETF SFN-UNDEFINED-XYZ)))))) (error () (list :error)))
  ((T T T NIL T NIL NIL NIL NIL)))

(deftest symbol-function-direct.19
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (FBOUNDP 5) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (TYPE-ERROR))

(deftest symbol-function-direct.20
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (FBOUNDP (QUOTE (FOO BAR))) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (TYPE-ERROR))

(deftest symbol-function-direct.21
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (EVAL (QUOTE (FBOUNDP))) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (PROGRAM-ERROR))

(deftest symbol-function-direct.22
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (EVAL (QUOTE (FBOUNDP (QUOTE A) (QUOTE B)))) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (PROGRAM-ERROR))

(deftest symbol-function-direct.23
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (PROGN (SETF (GET (QUOTE SFN-S) (QUOTE A)) 1 (GET (QUOTE SFN-S) (QUOTE B)) 2) (SYMBOL-PLIST (QUOTE SFN-S))))) (error () (list :error)))
  ((B 2 A 1)))

(deftest symbol-function-direct.24
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (PROGN (SETF (SYMBOL-PLIST (QUOTE SFN-S2)) (LIST (QUOTE X) 1)) (LIST (SYMBOL-PLIST (QUOTE SFN-S2)) (GET (QUOTE SFN-S2) (QUOTE X)))))) (error () (list :error)))
  (((X 1) 1)))

(deftest symbol-function-direct.25
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (LIST (SYMBOL-PLIST NIL) (SYMBOL-PLIST (QUOTE SFN-FRESH-XYZ))))) (error () (list :error)))
  ((NIL NIL)))

(deftest symbol-function-direct.26
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (SYMBOL-PLIST 5) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (TYPE-ERROR))

(deftest symbol-function-direct.27
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (HANDLER-CASE (EVAL (QUOTE (SYMBOL-PLIST))) (ERROR (E) (TYPE-OF E))))) (error () (list :error)))
  (PROGRAM-ERROR))

(deftest symbol-function-direct.28
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (FLET ((FBOUNDP (X) (LIST :LOCAL X))) (FBOUNDP (QUOTE CAR))))) (error () (list :error)))
  ((:LOCAL CAR)))

(deftest symbol-function-direct.29
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (FLET ((SYMBOL-FUNCTION (X) (LIST :LOCAL X))) (SYMBOL-FUNCTION (QUOTE CAR))))) (error () (list :error)))
  ((:LOCAL CAR)))

(deftest symbol-function-direct.30
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (LABELS ((SYMBOL-PLIST (X) (LIST :LABELS X))) (SYMBOL-PLIST (QUOTE CAR))))) (error () (list :error)))
  ((:LABELS CAR)))

(deftest symbol-function-direct.31
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (MAPCAR (FUNCTION FBOUNDP) (QUOTE (CAR SFN-UNDEFINED-XYZ))))) (error () (list :error)))
  ((T NIL)))

(deftest symbol-function-direct.32
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (MAPCAR (FUNCTION SYMBOL-PLIST) (QUOTE (SFN-S SFN-S2))))) (error () (list :error)))
  (((B 2 A 1) (X 1))))

(deftest symbol-function-direct.33
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (LENGTH (MULTIPLE-VALUE-LIST (SYMBOL-FUNCTION (QUOTE CAR)))))) (error () (list :error)))
  (1))

(deftest symbol-function-direct.34
  (handler-case (mapcar (lambda (x) (if (functionp x) :function x)) (multiple-value-list (MULTIPLE-VALUE-LIST (FBOUNDP (QUOTE CAR))))) (error () (list :error)))
  ((T)))

