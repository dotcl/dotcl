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

;;; The quotient functions too, with the values SBCL 2.6.8 reports: (/ n 0) for
;;; a fixnum numerator n of the dividend, (TRUNCATE n 0) for a bignum one, and
;;; neither when a float is involved.

(deftest division-by-zero-operands.quotients
  (let ((zero (read-from-string "0")))
    (mapcar (lambda (op)
              (list (dbzo-catch (lambda () (funcall op 1 zero)))
                    (dbzo-catch (lambda () (funcall op (expt 2 70) zero)))
                    (dbzo-catch (lambda () (funcall op -3/7 zero)))
                    (dbzo-catch (lambda () (funcall op 1.5 zero)))))
            '(truncate floor ceiling round mod rem ftruncate ffloor fceiling fround)))
  #.(make-list 10 :initial-element
               '((/ (1 0)) (truncate (1180591620717411303424 0)) (/ (-3 0)) (nil nil))))

(defun dbzo-fx-mod (a b) (declare (fixnum a b)) (mod a b))
(defun dbzo-fx-rem (a b) (declare (fixnum a b)) (rem a b))
(defun dbzo-fx-floor (a b) (declare (fixnum a b)) (floor a b))
(defun dbzo-fx-truncate (a b) (declare (fixnum a b)) (values (truncate a b)))
(defun dbzo-fx-sum (a b) (declare (fixnum a b))
  (the fixnum (+ (mod a b) (rem a b) (floor a b) (truncate a b))))

(deftest division-by-zero-operands.declared-quotients
  (mapcar (lambda (f) (dbzo-catch (lambda () (funcall f 6 0))))
          (list #'dbzo-fx-mod #'dbzo-fx-rem #'dbzo-fx-floor #'dbzo-fx-truncate
                #'dbzo-fx-sum))
  ((/ (6 0)) (/ (6 0)) (/ (6 0)) (/ (6 0)) (/ (6 0))))

(deftest division-by-zero-operands.quotient-report
  (let ((zero (read-from-string "0")))
    (flet ((dbzo-report (thunk)
             (handler-case (funcall thunk)
               (division-by-zero (e) (princ-to-string e)))))
      (list (dbzo-report (lambda () (truncate 1 zero)))
            (dbzo-report (lambda () (mod (expt 2 70) zero)))
            (dbzo-report (lambda () (floor 1.5 zero))))))
  ("arithmetic error DIVISION-BY-ZERO signalled
Operation was (/ 1 0)."
   "arithmetic error DIVISION-BY-ZERO signalled
Operation was (TRUNCATE 1180591620717411303424 0)."
   "arithmetic error DIVISION-BY-ZERO signalled"))
