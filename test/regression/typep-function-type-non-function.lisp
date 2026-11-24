;;; Values come from SBCL 2.6.8, with the type given at run time. A function type that says
;;; nothing about the signature -- (function), (function *), (function * *), (function (&rest t) *),
;;; (function * t) -- is T for a function and NIL for anything else. One that constrains the
;;; arguments or the values cannot be tested and stays an error, for every object.

(defun %tftype (obj spec) (handler-case (typep obj spec) (error () :error)))

(deftest typep-function-type-non-function.unconstrained
  (let ((specs '((function) (function *) (function * *) (function (&rest t) *) (function * t) (function (&rest t) t))))
    (list (mapcar (lambda (s) (%tftype 5 s)) specs)
          (mapcar (lambda (s) (%tftype nil s)) specs)
          (mapcar (lambda (s) (%tftype #'car s)) specs)))
  ((nil nil nil nil nil nil) (nil nil nil nil nil nil) (t t t t t t)))

(deftest typep-function-type-non-function.constrained
  (let ((specs '((function (integer) integer) (function () t) (function (&optional) *) (function (*) *))))
    (list (mapcar (lambda (s) (%tftype 5 s)) specs)
          (mapcar (lambda (s) (%tftype #'car s)) specs)))
  ((:error :error :error :error) (:error :error :error :error)))
