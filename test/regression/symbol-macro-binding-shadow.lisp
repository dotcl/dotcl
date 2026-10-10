;;; A variable bound by any binding form hides a symbol macro of the same name
;;; (CLHS 3.4.2), also from the &ENVIRONMENT a macro receives inside the
;;; binding. Expected values are SBCL's.

(defmacro sm-shadow-probe (name &environment env)
  `',(macroexpand-1 name env))

(defclass sm-shadow-obj () ((sm-shadow-x :initform 5 :accessor sm-shadow-obj-x)))

;;; Each binding form at top level, so nothing compiles it as part of an
;;; enclosing function first.
(defparameter *sm-shadow-results*
  (symbol-macrolet ((sm-shadow-x :lexical))
    (list
     (handler-case (error "e") (error (sm-shadow-x) (sm-shadow-probe sm-shadow-x)))
     (handler-case 1 (:no-error (sm-shadow-x) (sm-shadow-probe sm-shadow-x)))
     (restart-case (invoke-restart 'r 1)
       (r (sm-shadow-x) (sm-shadow-probe sm-shadow-x)))
     (restart-case (invoke-restart 'r 1 2)
       (r (a &optional sm-shadow-x) (list a (sm-shadow-probe sm-shadow-x))))
     (restart-case (invoke-restart 'r 1)
       (r (a &aux (sm-shadow-x a)) (sm-shadow-probe sm-shadow-x)))
     (multiple-value-bind (sm-shadow-x) 1 (sm-shadow-probe sm-shadow-x))
     (destructuring-bind (sm-shadow-x) '(1) (sm-shadow-probe sm-shadow-x))
     (do ((sm-shadow-x 0 (1+ sm-shadow-x))) (t (sm-shadow-probe sm-shadow-x)))
     (dolist (sm-shadow-x '(1)) (return (sm-shadow-probe sm-shadow-x)))
     (dotimes (sm-shadow-x 1) (return (sm-shadow-probe sm-shadow-x)))
     (let ((sm-shadow-x 1)) (sm-shadow-probe sm-shadow-x))
     ((lambda (sm-shadow-x) (sm-shadow-probe sm-shadow-x)) 1)
     (with-slots (sm-shadow-x) (make-instance 'sm-shadow-obj)
       (car (sm-shadow-probe sm-shadow-x))))))

(deftest symbol-macro-hidden-by-each-binding-form
  *sm-shadow-results*
  (sm-shadow-x sm-shadow-x sm-shadow-x (1 sm-shadow-x) sm-shadow-x sm-shadow-x
   sm-shadow-x sm-shadow-x sm-shadow-x sm-shadow-x sm-shadow-x sm-shadow-x
   slot-value))

;;; The variable itself still holds the condition.
(deftest symbol-macro-hidden-by-handler-case-var-value
  (symbol-macrolet ((sm-shadow-x :lexical))
    (handler-case (error "e")
      (error (sm-shadow-x) (list (typep sm-shadow-x 'error) (sm-shadow-probe sm-shadow-x)))))
  (t sm-shadow-x))

(deftest-emitting-only symbol-macro-hidden-by-handler-case-var-compiled
  (funcall (compile nil '(lambda ()
                          (symbol-macrolet ((sm-shadow-x :lexical))
                            (handler-case (error "e")
                              (error (sm-shadow-x) (sm-shadow-probe sm-shadow-x)))))))
  sm-shadow-x)

;;; A DEFUN parameter, and a LET inside a DEFUN, hide a symbol macro around the
;;; DEFUN. The scan for a RETURN-FROM of the function's block expands the body
;;; before the parameters are bound.
(symbol-macrolet ((sm-shadow-x :lexical))
  (defun sm-shadow-defun-param (sm-shadow-x) (list sm-shadow-x (sm-shadow-probe sm-shadow-x)))
  (defun sm-shadow-defun-optional (&optional (sm-shadow-x 2)) (sm-shadow-probe sm-shadow-x))
  (defun sm-shadow-defun-let () (let ((sm-shadow-x 1)) (sm-shadow-probe sm-shadow-x)))
  (defun sm-shadow-defun-free () (sm-shadow-probe sm-shadow-x))
  (defun sm-shadow-defun-handler (y)
    (handler-case (error "e") (error (sm-shadow-x) (list y (sm-shadow-probe sm-shadow-x))))))

(deftest symbol-macro-hidden-by-defun-parameter
  (list (sm-shadow-defun-param 1) (sm-shadow-defun-optional) (sm-shadow-defun-let)
        (sm-shadow-defun-free) (sm-shadow-defun-handler 1))
  ((1 sm-shadow-x) sm-shadow-x sm-shadow-x :lexical (1 sm-shadow-x)))
