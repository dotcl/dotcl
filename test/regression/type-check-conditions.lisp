;;; Argument type checks that used to signal a bare SIMPLE-ERROR.
;;;
;;; A SIMPLE-ERROR says nothing a handler can act on: no datum, no expected
;;; type, and TYPE-ERROR handlers do not see it. These are all plain "wrong type
;;; of argument" checks, so they signal TYPE-ERROR with both slots filled.

(defun %tcc-outcome (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error (e) (list :type-error (type-error-datum e)
                          (not (null (type-error-expected-type e)))))
    (error (e) (list :other (type-of e)))))

(deftest type-check-conditions.progv-non-symbol
  (%tcc-outcome (lambda () (progv '(1) '(2) nil)))
  (:type-error 1 t))

(deftest type-check-conditions.displaced-to-non-array
  (%tcc-outcome (lambda () (make-array 3 :displaced-to 42)))
  (:type-error 42 t))

(deftest type-check-conditions.package-local-nickname-bad-designator
  (%tcc-outcome (lambda () (dotcl:add-package-local-nickname 42 "CL" *package*)))
  (:type-error 42 t))

;;; The valid cases still work.

(deftest type-check-conditions.progv-binds
  (progv '(*tcc-var*) '(7) (symbol-value '*tcc-var*))
  7)

(deftest type-check-conditions.displaced-to-array
  (let* ((base (make-array 5 :initial-element 3))
         (d (make-array 2 :displaced-to base)))
    (aref d 1))
  3)

;;; Real-only arithmetic handed a complex, and readtable API handed a non-character.
;;; Both used to be raw .NET exceptions (ArgumentException / InvalidCastException),
;;; which the CLR-exception mapping reports as PROGRAM-ERROR with no datum.

(deftest type-check-conditions.floor-of-complex
  (%tcc-outcome (lambda () (floor #c(1 2))))
  (:type-error #c(1 2) t))

(deftest type-check-conditions.round-of-complex-expected-type
  (handler-case (progn (round #c(1 2)) :no-error)
    (type-error (e) (type-error-expected-type e))
    (error (e) (list :other (type-of e))))
  real)

(deftest type-check-conditions.real-arithmetic-unaffected
  (list (floor 7 2) (ceiling 7 2) (round 7 2) (truncate -7 2))
  (3 4 4 -3))

(deftest type-check-conditions.set-syntax-from-char-non-character
  (%tcc-outcome (lambda () (set-syntax-from-char 1 2)))
  (:type-error 1 t))

(deftest type-check-conditions.set-dispatch-on-non-dispatching-char
  (handler-case (progn (set-dispatch-macro-character #\a #\b (lambda (s c n)
                                                               (declare (ignore s c n))))
                       :no-error)
    (program-error () :program-error)
    (error (e) (list :other (type-of e))))
  :program-error)
