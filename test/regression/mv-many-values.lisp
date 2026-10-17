;;; Functions that return five or more values from their tail, called with each
;;; value mode. Expected values are SBCL's.

(defun %mmv-5 (x) (values x 2 3 4 5))
(defun %mmv-6 (x) (values x 2 3 4 5 6))
(defun %mmv-7 (x) (values x 2 3 4 5 6 7))
(defun %mmv-8 (x) (values x 2 3 4 5 6 7 8))
(defun %mmv-9 (x) (values x 2 3 4 5 6 7 8 9))
(defun %mmv-12 (x) (values x 2 3 4 5 6 7 8 9 10 11 12))
(defun %mmv-pick (n x) (case n (5 (%mmv-5 x)) (8 (%mmv-8 x)) (t (%mmv-12 x))))

(deftest mv-many-values.list
  (list (multiple-value-list (%mmv-5 1)) (multiple-value-list (%mmv-6 1))
        (multiple-value-list (%mmv-7 1)) (multiple-value-list (%mmv-8 1))
        (multiple-value-list (%mmv-9 1)) (multiple-value-list (%mmv-12 1))
        (multiple-value-list (%mmv-pick 8 :a)))
  ((1 2 3 4 5) (1 2 3 4 5 6) (1 2 3 4 5 6 7) (1 2 3 4 5 6 7 8) (1 2 3 4 5 6 7 8 9)
   (1 2 3 4 5 6 7 8 9 10 11 12) (:a 2 3 4 5 6 7 8)))

(deftest mv-many-values.bind-and-nth
  (list (multiple-value-bind (a b c d e f) (%mmv-5 1) (list a b c d e f))
        (multiple-value-bind (a b c d e f g h i) (%mmv-8 1) (list a b c d e f g h i))
        (multiple-value-bind (a b) (%mmv-12 1) (list a b))
        (multiple-value-bind (a b c d e f g h i j k l m) (%mmv-12 1) (list a b c d e f g h i j k l m))
        (multiple-value-bind (a b c d e f g) (%mmv-pick 12 :z) (list a b c d e f g))
        (nth-value 4 (%mmv-5 1)) (nth-value 6 (%mmv-7 1)) (nth-value 8 (%mmv-9 1))
        (nth-value 11 (%mmv-12 1)) (nth-value 12 (%mmv-12 1)))
  ((1 2 3 4 5 nil) (1 2 3 4 5 6 7 8 nil) (1 2) (1 2 3 4 5 6 7 8 9 10 11 12 nil)
   (:z 2 3 4 5 6 7) 5 7 9 12 nil))

(deftest mv-many-values.single-and-call
  (list (%mmv-5 :a) (%mmv-8 :b) (%mmv-12 :c) (let ((x (%mmv-9 :d))) x)
        (multiple-value-call #'+ (%mmv-6 1) (%mmv-9 1))
        (multiple-value-list (funcall #'%mmv-7 1))
        (multiple-value-list (apply #'%mmv-12 '(1)))
        (progn (%mmv-8 1) (multiple-value-list (values))))
  (:a :b :c :d 66 (1 2 3 4 5 6 7) (1 2 3 4 5 6 7 8 9 10 11 12) ()))

(defun %mmv-loop (n)
  (declare (fixnum n))
  (let ((r nil) (o (list 1)))
    (dotimes (i n r)
      (multiple-value-bind (a b c d e) (%mmv-5 o) (setq r (and a b c d e)))
      (multiple-value-bind (a b c d e f g h) (%mmv-8 o) (setq r (and a b c d e f g h)))
      (setq r (nth-value 5 (%mmv-6 o)))
      (setq r (%mmv-pick 8 o))
      (%mmv-7 o))))

(deftest-compiled-only mv-many-values.allocates-nothing
  (< (bytes-per-op #'%mmv-loop) 1)
  t)
