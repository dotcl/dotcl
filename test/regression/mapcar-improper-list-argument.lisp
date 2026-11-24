;;; MAPCAR and the other mapping functions signal a type error when a list argument
;;; ends in an atom other than NIL (or is one) before the walk is over; the lists are
;;; looked at in order, and a list that ran out first ends the walk without a look at
;;; the ones after it.

(defmacro %mia-error-p (form) `(handler-case (progn ,form nil) (type-error () t)))

(deftest mapcar-improper.dotted-tail-reached
  (list (%mia-error-p (mapcar #'list '(a b c) '(1 2 3) '(u v . w)))
        (%mia-error-p (mapcar #'list '(a b c) '(1 2 . 3)))
        (%mia-error-p (mapcar #'list '(a b . c) '(1 2)))
        (%mia-error-p (mapcar #'list '(a b) '(1 2) '(4 5 6) '(7 8 9) '(7 8 . 9)))
        (%mia-error-p (mapcar #'list '(a b c) '(1 2 3) '(4 5 6) '(7 8 . 9)))
        (%mia-error-p (mapcar #'list '(1 2) 5)))
  (t t t nil t t))

(deftest mapcar-improper.shorter-list-ends-first
  (list (mapcar #'list '(a b) '(1 2 . 3)) (mapcar #'list '(a) '(1 . 3))
        (mapcar #'list '(a b) '(1 2) '(4 5 6)))
  (((a 1) (b 2)) ((a 1)) ((a 1 4) (b 2 5))))

(deftest-emitting-only mapcar-improper.compiled
  (let ((f (compile nil '(lambda (x y) (mapcar #'list x y)))))
    (list (%mia-error-p (funcall f '(a b c) '(1 2 . 3))) (funcall f '(a b) '(1 2 3))))
  (t ((a 1) (b 2))))
