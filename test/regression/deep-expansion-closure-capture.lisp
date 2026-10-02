;;; Regression: the compiler's mutation / capture analysis followed nested
;;; macro expansions only 50 levels deep, then walked the unexpanded form. A
;;; closure that appears only in a deeper expansion was not seen, so an outer
;;; variable it assigns was captured by value instead of boxed, and the
;;; assignment was lost. Code walkers that wrap every subform in their own macro
;;; (cl-environments, used by generic-cl's DO-SEQUENCES!) reach that depth in
;;; ordinary code.

(defmacro deca-wrap (n form)
  (if (zerop n) form `(deca-wrap ,(1- n) ,form)))

(defmacro deca-call-it (&body body)
  `(let ((f (lambda () ,@body))) (funcall f)))

;;; The closure is 60 expansions deep: INCF inside it must reach the outer I.
(deftest-emitting-only deep-expansion-closure-assigns-outer
  (funcall (compile nil '(lambda ()
                          (let ((i 0))
                            (deca-wrap 60 (deca-call-it (incf i)))
                            (deca-wrap 60 (deca-call-it (incf i)))
                            i))))
  2)

;;; The shape generic-cl produced: a macro expands to a MULTIPLE-VALUE-CALL of
;;; a lambda whose &OPTIONAL defaults are not plain NIL (a real closure, not the
;;; binding shape), deep in expansions, assigning an outer variable in a loop.
(defmacro deca-mvb ((a b) values-form &body body)
  `(multiple-value-call
       #'(lambda (&optional (,a (progn nil)) (,b (progn nil)) &rest r)
           (declare (ignore r))
           ,@body)
     ,values-form))

(deftest-emitting-only deep-expansion-mv-call-closure-in-loop
  (funcall (compile nil '(lambda ()
                          (let ((n 0))
                            (dolist (x '(1 2 3))
                              (deca-wrap 55
                                (deca-mvb (a b) (values x x)
                                  (setq n (+ n a b)))))
                            n))))
  12)

;;; Still correct well inside the compiler's own depth guard.
(deftest-emitting-only deep-expansion-closure-assigns-outer-400
  (funcall (compile nil '(lambda ()
                          (let ((i 0))
                            (deca-wrap 400 (deca-call-it (incf i)))
                            i))))
  1)
