;;; NIL and T are symbols with no function: calling them through FUNCALL, APPLY or a function
;;; argument signals UNDEFINED-FUNCTION (cell name NIL / T), as for any other such symbol. A
;;; number or a string is still a TYPE-ERROR.

(defun %fnu-kind (thunk)
  (handler-case (progn (funcall thunk) :ok)
    (undefined-function (c) (list :undefined-function (cell-error-name c)))
    (type-error () :type-error)
    (error () :other)))

(deftest funcall-nil-undefined-function.nil-and-t
  (list (%fnu-kind (lambda () (funcall nil)))
        (%fnu-kind (lambda () (funcall t)))
        (%fnu-kind (lambda () (apply nil nil)))
        (%fnu-kind (lambda () (mapcar nil '(1)))))
  ((:undefined-function nil) (:undefined-function t) (:undefined-function nil)
   (:undefined-function nil)))

(deftest funcall-nil-undefined-function.others-unchanged
  (list (%fnu-kind (lambda () (funcall 5)))
        (%fnu-kind (lambda () (funcall "a")))
        (%fnu-kind (lambda () (funcall 'funcall-nil-undefined-function-nosuch)))
        (funcall 'car '(1)))
  (:type-error :type-error (:undefined-function funcall-nil-undefined-function-nosuch) 1))
