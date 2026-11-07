;;; DO and DO* without a variable list or an end clause are malformed
;;; syntax and must signal PROGRAM-ERROR; they used to expand into a loop
;;; that never ends.
(deftest do-missing-end-clause
  (values (handler-case (eval '(do)) (program-error () :program-error))
        (handler-case (eval '(do ())) (program-error () :program-error))
        (handler-case (eval '(do*)) (program-error () :program-error))
        (handler-case (eval '(do* ())) (program-error () :program-error))
        (do () (t :ok))
        (do* () (t :ok)))
  :program-error :program-error :program-error :program-error :ok :ok)
