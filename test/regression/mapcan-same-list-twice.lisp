;;; MAPCAN whose function returns the list it already collected closes it into a
;;; circle instead of looping while looking for the last cons.

(deftest mapcan-same-list.circular-result
  (let* ((p (list 'a 'b))
         (r (mapcan (lambda (x) (declare (ignore x)) p) '(1 2))))
    (list (eq r p) (eq (cddr r) r)))
  (t t))

(deftest mapcan-same-list.distinct-pieces
  (mapcan (lambda (x) (list x x)) '(1 2 3))
  (1 1 2 2 3 3))

(deftest mapcan-same-list.nil-and-last-atom
  (mapcan (lambda (x) (if (oddp x) (list x) nil)) '(1 2 3 4 5))
  (1 3 5))

(deftest mapcan-same-list.non-list-first-then-piece
  (handler-case (mapcan (lambda (x) (if (= x 1) 5 (list x))) '(1 2))
    (type-error () :type-error))
  :type-error)
