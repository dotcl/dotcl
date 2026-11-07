;;; A dotted key list in CASE / ECASE / CCASE, and a clause that is not a type specifier in
;;; TYPECASE / ETYPECASE / CTYPECASE, are PROGRAM-ERRORs; the valid shapes still work.
(deftest-emitting-only case-clause-shapes.errors
  (flet ((bad (form) (handler-case (progn (funcall (compile nil form)) :no-error) (program-error () :program-error))))
    (list (bad '(lambda () (case 1 ((1 . 2) 'a))))
          (bad '(lambda () (ecase 1 ((1 . 2) 'a))))
          (bad '(lambda () (let ((k 1)) (ccase k ((1 . 2) 'a)))))
          (bad '(lambda () (typecase 1 (1 'a))))
          (bad '(lambda () (typecase 1 ("s" 'a))))
          (bad '(lambda () (etypecase 1 ((1) 'a))))
          (bad '(lambda () (let ((k 1)) (ctypecase k (1.5 'a)))))))
  (:program-error :program-error :program-error :program-error :program-error :program-error :program-error))

(deftest-emitting-only case-clause-shapes.valid
  (list (funcall (compile nil '(lambda () (case 2 ((1 2) 'a) (t 'b)))))
        (funcall (compile nil '(lambda () (typecase 2 ((integer 0 5) 'a) (t 'b)))))
        (funcall (compile nil '(lambda () (typecase 2 ((or string symbol) 'a) (otherwise 'b)))))
        (funcall (compile nil '(lambda () (etypecase "s" (string 'a) ((member 1 2) 'b)))))
        (funcall (compile nil '(lambda () (case 1 ((nil) 'a) (otherwise 'c))))))
  (a a b a c))
