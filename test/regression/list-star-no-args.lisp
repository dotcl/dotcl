;;; (list*) with no arguments is a PROGRAM-ERROR (wrong number of arguments), as
;;; for any other function called with too few arguments; it was a SIMPLE-ERROR.

(deftest list-star-no-args.eval
  (handler-case (eval '(list*)) (program-error () :program-error) (error () :other-error))
  :program-error)

(deftest list-star-no-args.compiled
  (handler-case (funcall (compile nil '(lambda () (list*))))
    (program-error () :program-error) (error () :other-error))
  :program-error)

(deftest list-star-no-args.one-arg-unchanged
  (list (list* 1) (list* 1 2) (list* 1 2 '(3)))
  (1 (1 . 2) (1 2 3)))
