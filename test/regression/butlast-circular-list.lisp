;;; BUTLAST and NBUTLAST of a circular list signal a type error; they used to loop
;;; (and consume memory) without end.

(defun %bcl-circle (n)
  (let ((l (loop for i from 1 to n collect i))) (setf (cdr (last l)) l) l))

(deftest butlast-circular.signals
  (list (handler-case (butlast (%bcl-circle 3) 3) (type-error () :type-error))
        (handler-case (butlast (%bcl-circle 3)) (type-error () :type-error))
        (handler-case (nbutlast (%bcl-circle 4) 2) (type-error () :type-error)))
  (:type-error :type-error :type-error))

(deftest-emitting-only butlast-circular.compiled-call
  (handler-case (funcall (compile nil '(lambda (l) (butlast l))) (%bcl-circle 5)) (type-error () :type-error))
  :type-error)

(deftest butlast-circular.ordinary-lists-unchanged
  (list (butlast '(1 2 3 4) 2) (butlast '(1 2 3 . 4) 1) (butlast '(1)) (butlast nil) (butlast '(1 2) 0)
        (nbutlast (list 1 2 3) 1))
  ((1 2) (1 2) nil nil (1 2) (1 2)))
