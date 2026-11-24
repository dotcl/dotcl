;;; MAP looks at its sequences in order: a list that ends in an atom other than NIL
;;; is a type error when the walk reaches it, even if a later sequence is shorter,
;;; and a sequence that ran out first ends the walk without a look at the ones after.

(defmacro %mapia-error-p (form) `(handler-case (progn ,form nil) (type-error () t)))

(deftest map-improper.dotted-first-list-with-shorter-later-one
  (let ((d (list* 'a 'b 'c)) (n 5))
    (list (%mapia-error-p (map 'list #'list d '(1 2)))
          (%mapia-error-p (map 'vector #'list d '(1 2)))
          (%mapia-error-p (map nil #'list d '(1 2)))
          (%mapia-error-p (map 'list #'list d '(1 2 3) '(1 2)))
          (%mapia-error-p (map 'list #'list '(1 2) n))
          (%mapia-error-p (map 'list #'list n '(1)))))
  (t t t t t t))

(deftest map-improper.shorter-list-ends-first
  (list (map 'list #'list '(a b) '(1 2 3) '(4 5))
        (map 'list #'list nil nil)
        (map 'list #'list '(1 2) #(1 2 3))
        (map 'list #'list '(a b) '(1 2 3) '(4 5 6)))
  (((a 1 4) (b 2 5)) nil ((1 1) (2 2))
   ((a 1 4) (b 2 5))))

(deftest map-improper.other-mapping-functions-neighbourhood
  (list (mapc #'list '(a b) '(1 2 3))
        (mapcan #'list '(a b) '(1 2 3))
        (maplist #'list '(a b) '(1 2 3))
        (mapcon #'list '(a b))
        (mapcar #'list nil '(1))
        (list (%mapia-error-p (mapc #'list (list* 'a 'b 'c)))
              (%mapia-error-p (mapcan #'list 5))
              (%mapia-error-p (maplist #'list (list* 'a 'b 'c)))
              (%mapia-error-p (mapl #'list 5))
              (%mapia-error-p (mapcon #'list (list* 'a 'b 'c)))))
  ((a b) (a 1 b 2) (((a b) (1 2 3)) ((b) (2 3))) ((a b) (b)) nil (t t t t t)))
