;;; A LET / LET* binding is VAR or (VAR [INIT-FORM]): a longer one is a PROGRAM-ERROR, not an init
;;; form that is silently dropped. Well-formed bindings are unchanged.

(defun %lmb-outcome (form)
  (handler-case (list :value (funcall (compile nil `(lambda () ,form))))
    (program-error () :program-error)))

(deftest-emitting-only let-malformed-binding.rejected
  (list (%lmb-outcome '(let ((a 1 2)) a))
        (%lmb-outcome '(let* ((a 1 2)) a))
        (%lmb-outcome '(let ((a 1) (b 2 3)) (list a b)))
        (%lmb-outcome '(let* ((a 1) (b a 3)) (list a b))))
  (:program-error :program-error :program-error :program-error))

(deftest-emitting-only let-malformed-binding.valid-forms-unchanged
  (list (%lmb-outcome '(let ((a 1) b (c)) (list a b c)))
        (%lmb-outcome '(let* ((a 1) (b (+ a 1)) c) (list a b c)))
        (%lmb-outcome '(let () 5)))
  ((:value (1 nil nil)) (:value (1 2 nil)) (:value 5)))
