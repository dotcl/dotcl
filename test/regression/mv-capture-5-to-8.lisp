;;; A function returning 5 to 8 values, read by MULTIPLE-VALUE-BIND, MULTIPLE-VALUE-LIST and
;;; MULTIPLE-VALUE-CALL, gives every value in order, whatever the caller binds.

(defun %m58-5 (x) (values x (1+ x) (+ x 2) (+ x 3) (+ x 4)))
(defun %m58-6 (x) (values x (1+ x) (+ x 2) (+ x 3) (+ x 4) (+ x 5)))
(defun %m58-7 (x) (values x (1+ x) (+ x 2) (+ x 3) (+ x 4) (+ x 5) (+ x 6)))
(defun %m58-8 (x) (values x (1+ x) (+ x 2) (+ x 3) (+ x 4) (+ x 5) (+ x 6) (+ x 7)))
(defun %m58-9 (x) (values x (1+ x) (+ x 2) (+ x 3) (+ x 4) (+ x 5) (+ x 6) (+ x 7) (+ x 8)))
(declaim (notinline %m58-5 %m58-6 %m58-7 %m58-8 %m58-9))

(deftest mv-capture-5-to-8.bind-all
  (list (multiple-value-bind (a b c d e) (%m58-5 10) (list a b c d e))
        (multiple-value-bind (a b c d e f) (%m58-6 10) (list a b c d e f))
        (multiple-value-bind (a b c d e f g) (%m58-7 10) (list a b c d e f g))
        (multiple-value-bind (a b c d e f g h) (%m58-8 10) (list a b c d e f g h))
        (multiple-value-bind (a b c d e f g h i) (%m58-9 10) (list a b c d e f g h i)))
  ((10 11 12 13 14) (10 11 12 13 14 15) (10 11 12 13 14 15 16) (10 11 12 13 14 15 16 17)
   (10 11 12 13 14 15 16 17 18)))

(deftest mv-capture-5-to-8.bind-fewer-and-more
  (list (multiple-value-bind (a b) (%m58-8 1) (list a b))
        (multiple-value-bind (a b c d e f g h i j) (%m58-8 1) (list a b c d e f g h i j))
        (multiple-value-bind (a b c d e f) (%m58-5 1) (list a b c d e f))
        (multiple-value-bind (h) (%m58-8 3) h)
        (nth-value 7 (%m58-8 3)) (nth-value 8 (%m58-8 3)))
  ((1 2) (1 2 3 4 5 6 7 8 nil nil) (1 2 3 4 5 nil) 3 10 nil))

(deftest mv-capture-5-to-8.list-and-call
  (list (multiple-value-list (%m58-6 0)) (multiple-value-call #'list (%m58-5 5) (%m58-7 100))
        (multiple-value-list (values-list (multiple-value-list (%m58-8 0))))
        (length (multiple-value-list (%m58-9 0))))
  ((0 1 2 3 4 5) (5 6 7 8 9 100 101 102 103 104 105 106) (0 1 2 3 4 5 6 7) 9))

(deftest mv-capture-5-to-8.single-value-use-and-loop
  (let ((s 0))
    (dotimes (i 200)
      (multiple-value-bind (a b c d e f g h) (%m58-8 i) (incf s (+ a b c d e f g h)))
      (incf s (%m58-5 i)))
    (list s (+ 1 (%m58-6 1)) (list (%m58-7 2))))
  (#.(loop for i below 200 sum (+ (* 8 i) 28 i)) 2 (2)))
