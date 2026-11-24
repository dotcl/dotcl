;;; Values come from SBCL 2.6.8. FUNCALL, APPLY and the mapping functions look only at the
;;; function cell of the symbol they are given: QQ::CAR in a package that does not use
;;; COMMON-LISP is not CL:CAR, however it is spelled.

(defpackage :fsn-qq (:use))
(defpackage :fsn-own (:use :common-lisp))

(defun %fsn (thunk) (handler-case (funcall thunk) (undefined-function () :undefined-function)))

(deftest funcall-symbol-no-name-lookup.undefined
  (let ((car (intern "CAR" :fsn-qq)) (plus (intern "+" :fsn-qq)) (kw (intern "CAR" :keyword))
        (gen (make-symbol "CAR")))
    (list (fboundp car)
          (%fsn (lambda () (funcall car '(1 2))))
          (%fsn (lambda () (apply car '((1 2)))))
          (%fsn (lambda () (mapcar car '((1)))))
          (%fsn (lambda () (funcall plus 1 2)))
          (%fsn (lambda () (funcall kw '(1 2))))
          (%fsn (lambda () (funcall gen '(1 2))))
          (%fsn (lambda () (symbol-function car)))
          (%fsn (lambda () (fdefinition car)))))
  (nil :undefined-function :undefined-function :undefined-function :undefined-function
   :undefined-function :undefined-function :undefined-function :undefined-function))

(deftest funcall-symbol-no-name-lookup.defined
  (let ((own (intern "CAR" :fsn-own)) (f (intern "FSN-FN" :fsn-qq)))
    (setf (symbol-function f) (lambda (x) (list :own x)))
    (list (eq own 'car)
          (funcall 'car '(1 2))
          (funcall 'cl:car '(1 2))
          (apply 'cl:+ '(1 2))
          (funcall f 5)
          (mapcar #'car '((1) (2)))
          (funcall (find-symbol "LIST" :common-lisp) 1 2)))
  (t 1 1 3 (:own 5) (1 2) (1 2)))
