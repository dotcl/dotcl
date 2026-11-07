;;; (:include NAME) of a structure that does not exist is an error; including a
;;; defined structure still works.

(defstruct dsiu-base a)

(deftest defstruct-include-undefined.error
  (handler-case (progn (eval '(defstruct (dsiu-child (:include dsiu-no-such-struct)) b)) :no-error)
    (error () :error))
  :error)

(deftest defstruct-include-undefined.defined-parent-works
  (progn
    (eval '(defstruct (dsiu-ok (:include dsiu-base)) b))
    (let ((o (make-dsiu-ok :a 1 :b 2)))
      (list (dsiu-base-a o) (dsiu-ok-b o) (typep o 'dsiu-base))))
  (1 2 t))
