;;; REPLACE between a list and a vector, and SEARCH over lists, walk each list once. The answers are
;;; the standard's; the sizes make the old walk-from-the-head-per-element form (quadratic) take
;;; minutes, so a regression there shows as the whole run not finishing.

(defun %lsl-list (n) (loop for i below n collect i))

(deftest list-sequence-linear.replace-list-vector
  (let* ((n 100000) (l (%lsl-list n)) (v (make-array n)))
    (replace v l)
    (let ((back (make-list n :initial-element 0)))
      (replace back v)
      (list (equal back l) (aref v 0) (aref v (1- n)) (= (length back) n))))
  (t 0 99999 t))

(deftest list-sequence-linear.replace-ranges
  (let ((l (list 0 1 2 3 4 5 6 7 8 9)) (v (vector :a :b :c :d)))
    (list (replace (copy-list l) v :start1 3 :start2 1 :end2 3)
          (let ((w (make-array 6 :initial-element 0))) (replace w l :start1 2 :start2 5) (coerce w 'list))
          (replace (copy-list l) '(a b c) :start1 8)
          (replace (copy-list l) nil)
          (replace (list 1 2) (vector) :start1 1)))
  ((0 1 2 :b :c 5 6 7 8 9) (0 0 5 6 7 8) (0 1 2 3 4 5 6 7 a b) (0 1 2 3 4 5 6 7 8 9) (1 2)))

(deftest list-sequence-linear.replace-same-list-overlap
  (let ((a (list 1 2 3 4 5)) (b (list 1 2 3 4 5)) (c (vector 1 2 3 4 5)))
    (replace a a :start1 1 :start2 0)
    (replace b b :start1 0 :start2 2)
    (replace c c :start1 1 :start2 0)
    (list a b (coerce c 'list) (replace (list 1 2 3) (list 1 2 3))))
  ((1 1 2 3 4) (3 4 5 4 5) (1 1 2 3 4) (1 2 3)))

(deftest list-sequence-linear.search-in-lists
  (let ((l (%lsl-list 100000)))
    (list (search '(99998 99999) l) (search '(5 6 7) l) (search '(7 5) l) (search nil l)
          (search '(-1 -2) l) (search '(1 2) l :from-end t) (search '(50 51) l :start2 60)
          (search '(3 4) l :test #'=) (search '(4 5) l :key #'1-)
          (search (list 3 4) (coerce l 'vector)) (search (vector 3 4) l) (search '(5) l :end2 5)))
  (99998 5 nil 0 nil 1 nil 3 4 3 3 nil))
