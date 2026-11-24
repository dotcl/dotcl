;;; A TYPE-ERROR raised for a wrong argument names the value in its report, also for the
;;; messages that say "must be an integer" rather than "not a list".

(defun %tem-report (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error (e) (princ-to-string e))))

(defun %tem-has (text part) (and (stringp text) (search part text) t))

(deftest type-error-message-value.index
  (list (%tem-has (%tem-report (lambda () (char "abc" 'zzz))) "ZZZ")
        (%tem-has (%tem-report (lambda () (svref (vector 1) 'zzz))) "ZZZ")
        (%tem-has (%tem-report (lambda () (aref (vector 1) "str"))) "str")
        (%tem-has (%tem-report (lambda () (elt '(1 2) :k))) ":K")
        (%tem-has (%tem-report (lambda () (make-list 'zzz))) "ZZZ"))
  (t t t t t))

(deftest type-error-message-value.not-a-form-unchanged
  ;; The older "not a" messages still name the value once.
  (let ((text (%tem-report (lambda () (car 5)))))
    (list (%tem-has text "5") (= 1 (count #\5 text))))
  (t t))

(deftest type-error-message-value.datum-and-expected-type
  (handler-case (char "abc" 'zzz)
    (type-error (e) (list (type-error-datum e) (type-error-expected-type e))))
  (zzz integer))
