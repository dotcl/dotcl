;;; A place that is a standard accessor called with the wrong number of arguments, a place
;;; headed by a list, and SETQ of a constant are PROGRAM-ERRORs; valid places still work.
(deftest-emitting-only place-shapes.errors
  (flet ((bad (form) (handler-case (progn (funcall (compile nil form)) :no-error) (error () :program-error))))
    (list (bad '(lambda () (let ((l (list 1 2))) (setf (car l l) 5))))
          (bad '(lambda () (let ((l (list 1 2))) (setf (car) 5))))
          (bad '(lambda () (let ((l (list 1 2))) (incf (aref)))))
          (bad '(lambda () (let ((l (list 1 2))) (push 1 (gethash 1)))))
          (bad '(lambda () (let ((l (list 1 2))) (pop (symbol-value)))))
          (bad '(lambda () (let ((l (list 1 2))) (rotatef (car) (car l)))))
          (bad '(lambda () (let ((l (list 1 2))) (shiftf (car l l) (car l) 5))))
          (bad '(lambda () (let ((l (list 1 2))) (psetf (get 'x) 5))))
          (bad '(lambda () (let ((l (list 1 2))) (setf ((car l)) 5))))
          (bad '(lambda () (setq :k 5)))))
  (:program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error))

(deftest-emitting-only place-shapes.valid
  (list (funcall (compile nil '(lambda () (let ((l (list 1 2))) (setf (car l) 5 (cadr l) 6) l))))
        (funcall (compile nil '(lambda () (let ((l (list 1 2)) (h (make-hash-table))) (setf (gethash 1 h) 3) (incf (gethash 1 h 0)) (gethash 1 h)))))
        (funcall (compile nil '(lambda () (let ((v (vector 1 2))) (incf (aref v 1)) (rotatef (aref v 0) (aref v 1)) (coerce v (quote list))))))
        (funcall (compile nil '(lambda () (let ((l (list 1 2))) (push 0 (cdr l)) (pop (cdr l)) l))))
        (funcall (compile nil '(lambda () (let ((x 1) (y 2)) (psetf x y y x) (list x y))))))
  ((5 6) 4 (3 1) (1 2) (2 1)))
