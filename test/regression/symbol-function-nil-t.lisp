;;; SYMBOL-FUNCTION of NIL or T signals UNDEFINED-FUNCTION, like any other
;;; symbol that is not fbound. NIL and T are symbols, but the runtime
;;; represents them with their own classes, and SYMBOL-FUNCTION checked only
;;; the general symbol class, so it signalled TYPE-ERROR for them. cl-marshal
;;; unmarshals a function reference with (symbol-function (find-symbol ...)),
;;; which reached this with NIL.

(defun %sfnt-condition (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (undefined-function (c) (list :undefined-function (cell-error-name c)))
    (type-error () :type-error)))

(deftest symbol-function-nil-t-undefined
  (list (%sfnt-condition (lambda () (symbol-function nil)))
        (%sfnt-condition (lambda () (symbol-function t)))
        (%sfnt-condition (lambda () (symbol-function (find-symbol "NIL" "COMMON-LISP"))))
        (%sfnt-condition (lambda () (symbol-function 3)))
        (%sfnt-condition (lambda () (symbol-function "CAR"))))
  ((:undefined-function nil) (:undefined-function t) (:undefined-function nil)
   :type-error :type-error))
