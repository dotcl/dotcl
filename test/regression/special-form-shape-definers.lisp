;;; DESTRUCTURING-BIND, DEFMACRO, DEFSETF, DEFINE-SYMBOL-MACRO,
;;; DEFINE-COMPILER-MACRO, DEFINE-MODIFY-MACRO and EVAL-WHEN with a bad shape
;;; signal PROGRAM-ERROR; the valid minimal shapes still work.

(defmacro %sfdef-pe (form)
  `(handler-case (progn (eval ',form) :no-error)
     (program-error () :program-error)
     (error () :other-error)))

(deftest special-form-shape-definers.invalid
  (values (%sfdef-pe (destructuring-bind)) (%sfdef-pe (destructuring-bind (a)))
          (%sfdef-pe (defsetf)) (%sfdef-pe (defsetf %sfdef-s1))
          (%sfdef-pe (defmacro)) (%sfdef-pe (defmacro %sfdef-m1)) (%sfdef-pe (defmacro 1 ()))
          (%sfdef-pe (define-symbol-macro)) (%sfdef-pe (define-symbol-macro %sfdef-sm1))
          (%sfdef-pe (define-compiler-macro)) (%sfdef-pe (define-compiler-macro %sfdef-c1))
          (%sfdef-pe (define-modify-macro)) (%sfdef-pe (define-modify-macro %sfdef-mm1 ()))
          (%sfdef-pe (eval-when x nil)) (%sfdef-pe (eval-when (foo) 1)) (%sfdef-pe (eval-when (:execute . 1) 1)))
  :program-error :program-error :program-error :program-error
  :program-error :program-error :program-error
  :program-error :program-error :program-error :program-error
  :program-error :program-error :program-error :program-error :program-error)

(deftest special-form-shape-definers.valid
  (values (eval '(destructuring-bind (a) '(1)))
          (eval '(destructuring-bind (a &optional b) '(1) (list a b)))
          (eval '(progn (defmacro %sfdef-m2 (x)) (macroexpand-1 '(%sfdef-m2 1))))
          (eval '(progn (define-symbol-macro %sfdef-sm2 7) %sfdef-sm2))
          (eval '(progn (define-modify-macro %sfdef-mm2 (&optional (d 1)) +) (let ((v 1)) (%sfdef-mm2 v 2) v)))
          (eval '(eval-when (:execute) 1)) (eval '(eval-when (compile load eval) 2))
          (eval '(eval-when (:compile-toplevel :load-toplevel :execute) 3)) (eval '(eval-when ())))
  nil (1 nil) nil 7 3 1 2 3 nil)
