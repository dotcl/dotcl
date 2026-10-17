;;; A generic function of one or two arguments hands the caller's value mode
;;; (only the primary value used, or the values captured by
;;; MULTIPLE-VALUE-BIND) on to the method it runs from its loose cache, as a
;;; direct call does. The method's tail VALUES then make no MvReturn.

(defgeneric %gvm-4 (a b))
(defmethod %gvm-4 ((a integer) b) (values a b (+ a 1) (+ a 2)))
(defmethod %gvm-4 ((a symbol) b) (values))
(defmethod %gvm-4 ((a string) b) (%gvm-tail a b))
(defmethod %gvm-4 ((a float) b) (values a b))
(defun %gvm-tail (a b) (values b a))

(defgeneric %gvm-1 (a))
(defmethod %gvm-1 ((a integer)) (values a (* a 2) (* a 3)))
(defmethod %gvm-1 ((a cons)) (%gvm-1 (car a)))
(defmethod %gvm-1 ((a t)) (values :t a))

(defgeneric %gvm-k (a b &key c))
(defmethod %gvm-k ((a integer) b &key (c 0)) (values a b c (+ a c)))

(defun %gvm-pass (a b) (%gvm-4 a b))

(deftest gf-value-mode.values
  (list (multiple-value-list (%gvm-4 1 2))
        (multiple-value-list (%gvm-4 'x 2))
        (multiple-value-list (%gvm-4 "s" 2))
        (multiple-value-list (%gvm-4 1.5 2))
        (multiple-value-bind (a b c d) (%gvm-4 1 2) (list a b c d))
        (multiple-value-bind (a b c d) (%gvm-4 'x 2) (list a b c d))
        (multiple-value-bind (a b) (%gvm-pass "s" 2) (list a b))
        (let ((x (%gvm-4 1 2))) x)
        (let ((x (%gvm-4 'x 2))) x)
        (nth-value 3 (%gvm-4 10 0))
        (multiple-value-list (funcall #'%gvm-4 1 2))
        (multiple-value-list (apply #'%gvm-4 '(1 2)))
        (multiple-value-list (%gvm-1 4))
        (multiple-value-list (%gvm-1 '(4)))
        (multiple-value-list (%gvm-1 :z))
        (multiple-value-bind (a b c) (%gvm-1 4) (list a b c))
        (multiple-value-list (%gvm-k 1 2))
        (multiple-value-list (%gvm-k 1 2 :c 3))
        (multiple-value-bind (a b c d) (%gvm-k 1 2) (list a b c d)))
  ((1 2 2 3) () (2 "s") (1.5 2) (1 2 2 3) (nil nil nil nil) (2 "s") 1 nil 12
   (1 2 2 3) (1 2 2 3) (4 8 12) (4 8 12) (:t :z) (4 8 12) (1 2 0 1) (1 2 3 4)
   (1 2 0 1)))

(defun %gvm-bytes (fn n)
  (dotimes (i 100) (funcall fn))
  (min-allocated-bytes (lambda () (dotimes (i n) (funcall fn)))))

(deftest-emitting-only gf-value-mode.no-mv-object
  (list (< (%gvm-bytes (lambda () (%gvm-4 1 2)) 10000) 4000)
        (< (%gvm-bytes (lambda () (multiple-value-bind (a b c d) (%gvm-4 1 2)
                                    (declare (ignore a b c d))
                                    nil))
                       10000)
           4000)
        (< (%gvm-bytes (lambda () (%gvm-1 4)) 10000) 4000)
        (< (%gvm-bytes (lambda () (%gvm-k 1 2)) 10000) 4000))
  (t t t t))

;; Three and four arguments: the plain primary chain gets the mode too.
(defgeneric %gvm-3 (a b c))
(defmethod %gvm-3 ((a integer) b c) (values a b c (+ a 1)))
(defmethod %gvm-3 ((a symbol) b c) (values))
(defmethod %gvm-3 ((a string) b c) (%gvm-tail b c))
(defgeneric %gvm-4a (a b c d))
(defmethod %gvm-4a ((a integer) b c d) (values d c b a))
(defmethod %gvm-4a ((a cons) b c d) (if (car a) (call-next-method) (values :cons b)))
(defmethod %gvm-4a ((a t) b c d) (values :t a))
(defgeneric %gvm-3a (a b c))
(defmethod %gvm-3a ((a integer) b c) (values a b c))
(defmethod %gvm-3a :after ((a integer) b c) (values :after :after))

(deftest gf-value-mode.three-four
  (list (multiple-value-list (%gvm-3 1 2 3))
        (multiple-value-bind (a b c d) (%gvm-3 1 2 3) (list a b c d))
        (multiple-value-list (%gvm-3 'x 2 3))
        (multiple-value-bind (a b) (%gvm-3 'x 2 3) (list a b))
        (multiple-value-bind (a b) (%gvm-3 "s" 2 3) (list a b))
        (let ((x (%gvm-3 1 2 3))) x)
        (nth-value 3 (%gvm-3 10 0 0))
        (multiple-value-list (%gvm-4a 1 2 3 4))
        (multiple-value-bind (a b c d) (%gvm-4a 1 2 3 4) (list a b c d))
        (multiple-value-list (%gvm-4a '(nil) 2 3 4))
        (multiple-value-list (%gvm-4a '(t) 2 3 4))
        (let ((x (%gvm-4a 1 2 3 4))) x)
        (multiple-value-list (%gvm-3a 1 2 3))
        (multiple-value-bind (a b c) (%gvm-3a 1 2 3) (list a b c)))
  ((1 2 3 2) (1 2 3 2) () (nil nil) (3 2) 1 11 (4 3 2 1) (4 3 2 1) (:cons 2)
   (:t (t)) 4 (1 2 3) (1 2 3)))

(deftest-emitting-only gf-value-mode.three-four-no-mv-object
  (list (< (%gvm-bytes (lambda () (%gvm-3 1 2 3)) 10000) 4000)
        (< (%gvm-bytes (lambda () (multiple-value-bind (a b c d) (%gvm-3 1 2 3)
                                    (declare (ignore a b c d))
                                    nil))
                       10000)
           4000)
        (< (%gvm-bytes (lambda () (multiple-value-bind (a b c d) (%gvm-4a 1 2 3 4)
                                    (declare (ignore a b c d))
                                    nil))
                       10000)
           4000))
  (t t t))

;; A named call of a generic function in single-value position passes the
;; primary-value mode, as one of a function that takes it does.
(defun %gvm-single-loop (n)
  (declare (fixnum n))
  (let ((r nil))
    (dotimes (i n r)
      (setq r (%gvm-3 1 2 3))
      (setq r (%gvm-1 4))
      (setq r (%gvm-4 1 2)))))

(deftest gf-value-mode.single-position
  (list (%gvm-single-loop 3) (let ((x (%gvm-4 'x 2))) x) (let ((x (%gvm-3a 5 2 3))) x))
  (1 nil 5))

(deftest-compiled-only gf-value-mode.single-position-no-mv-object
  (< (bytes-per-op #'%gvm-single-loop) 1)
  t)
