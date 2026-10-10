;;; DIVISION-BY-ZERO from / carries its operation and operands, so
;;; ARITHMETIC-ERROR-OPERATION and ARITHMETIC-ERROR-OPERANDS answer them (they
;;; answered NIL), and its report names the condition type and the operation,
;;; as SBCL's does. Coalton's exception tests read the operands and look for
;;; "DIVISION-BY-ZERO" in the report.

(defun dbzo-catch (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (division-by-zero (e)
      (list (arithmetic-error-operation e) (arithmetic-error-operands e)))))

(defun dbzo-integer (a b) (declare (integer a b)) (/ a b))
(defun dbzo-fixnum (a b) (declare (fixnum a b)) (/ a b))

(deftest division-by-zero-operands.generic
  (let ((zero (read-from-string "0")))
    (list (dbzo-catch (lambda () (/ 1 zero)))
          (dbzo-catch (lambda () (/ 1/2 zero)))
          (dbzo-catch (lambda () (/ (expt 2 70) zero)))))
  ((/ (1 0)) (/ (1/2 0)) (/ (1180591620717411303424 0))))

(deftest division-by-zero-operands.declared
  (list (dbzo-catch (lambda () (dbzo-integer 6 0)))
        (dbzo-catch (lambda () (dbzo-fixnum 6 0))))
  ((/ (6 0)) (/ (6 0))))

(deftest division-by-zero-operands.report
  (handler-case (/ 1 (read-from-string "0"))
    (division-by-zero (e)
      (let ((text (princ-to-string e)))
        (list (and (search "DIVISION-BY-ZERO" text) t)
              (and (search "(/ 1 0)" text) t)))))
  (t t))
