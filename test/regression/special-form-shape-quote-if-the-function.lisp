;;; QUOTE, IF, THE and FUNCTION with the wrong number of arguments signal
;;; PROGRAM-ERROR; every valid shape still works.

(defmacro %sfs-pe (form)
  `(handler-case (progn (eval ',form) :no-error)
     (program-error () :program-error)
     (error () :other-error)))

(deftest special-form-shape.invalid
  (values (%sfs-pe (if)) (%sfs-pe (if t)) (%sfs-pe (if t 1 2 3))
          (%sfs-pe (quote)) (%sfs-pe (quote a b)) (%sfs-pe (quote . a))
          (%sfs-pe (the)) (%sfs-pe (the t)) (%sfs-pe (the t 1 2))
          (%sfs-pe (function)) (%sfs-pe (function car cdr)))
  :program-error :program-error :program-error
  :program-error :program-error :program-error
  :program-error :program-error :program-error
  :program-error :program-error)

(deftest-emitting-only special-form-shape.valid
  (values (eval '(if nil 1)) (eval '(if nil 1 2)) (eval '(if t 1 2))
          (eval '(quote a)) (eval '(the t 1)) (eval '(the (values integer) 1))
          (eval '(funcall (function car) '(7)))
          (eval '(funcall (function (lambda (&optional (x 3)) x))))
          (funcall (compile nil '(lambda () (if nil 1)))))
  nil 2 1 a 1 1 7 3 nil)
