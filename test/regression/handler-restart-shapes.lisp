;;; Malformed clauses of HANDLER-CASE, HANDLER-BIND, RESTART-CASE, RESTART-BIND and
;;; WITH-SIMPLE-RESTART signal an error (a PROGRAM-ERROR, or a TYPE-ERROR from the
;;; analysis pass when the clause is not even a list); the valid shapes still work.
(deftest-emitting-only handler-restart-shapes.errors
  (flet ((bad (form) (handler-case (progn (funcall (compile nil form)) :no-error) (error () :program-error))))
    (list (bad '(lambda () (handler-case 1 5)))
          (bad '(lambda () (handler-case 1 (error))))
          (bad '(lambda () (handler-case 1 (error c c))))
          (bad '(lambda () (handler-case 1 (error (c d) c))))
          (bad '(lambda () (handler-case 1 (1 (c) c))))
          (bad '(lambda () (handler-bind (5) 1)))
          (bad '(lambda () (handler-bind ((error)) 1)))
          (bad '(lambda () (handler-bind ((error #'identity 1)) 1)))
          (bad '(lambda () (restart-case 1 5)))
          (bad '(lambda () (restart-case 1 (r))))
          (bad '(lambda () (restart-case 1 (r 1))))
          (bad '(lambda () (restart-bind (5) 1)))
          (bad '(lambda () (restart-bind ((r)) 1)))
          (bad '(lambda () (with-simple-restart r 1)))
          (bad '(lambda () (with-simple-restart (r) 1)))))
  (:program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error))

(deftest-emitting-only handler-restart-shapes.valid
  (list (funcall (compile nil '(lambda () (handler-case (error "x") (error (c) (declare (ignore c)) 1)))))
        (funcall (compile nil '(lambda () (handler-case 5 (error () 1) (:no-error (v) (* v 2))))))
        (funcall (compile nil '(lambda () (handler-case (error "x") ((or error warning) () 3)))))
        (funcall (compile nil '(lambda () (handler-bind ((error #'identity) ((or warning error) (lambda (c) c))) 7))))
        (funcall (compile nil '(lambda () (restart-case (invoke-restart 'r 4) (r (v) :report "report" v)))))
        (funcall (compile nil '(lambda () (restart-bind ((r (lambda () 9) :report-function (lambda (s) s))) (invoke-restart 'r)))))
        (funcall (compile nil '(lambda () (with-simple-restart (r "restart ~a" 1) (invoke-restart 'r))))))
  (1 10 3 7 4 9 nil))
