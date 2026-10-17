;;; APPLY hands the caller's value mode to the function it calls. Expected
;;; values are SBCL's.

(defun %mam-v (&rest xs) (values-list (cons :v xs)))
(defun %mam-2 (a b) (values a b (list a b)))
(defun %mam-4 (a b c d) (values d c b a))
(defun %mam-tail (f &rest xs) (apply f xs))
(defgeneric %mam-gf (a b))
(defmethod %mam-gf ((a integer) b) (values b a))

(deftest mv-apply-mode.values
  (list (multiple-value-list (apply #'%mam-2 '(1 2)))
        (multiple-value-list (apply #'%mam-2 1 '(2)))
        (multiple-value-list (apply #'%mam-4 1 2 '(3 4)))
        (multiple-value-list (apply #'%mam-4 '(1 2 3 4)))
        (multiple-value-list (apply #'%mam-v '()))
        (multiple-value-list (apply #'%mam-v 1 2 3 '(4 5 6)))
        (multiple-value-bind (a b c) (apply #'%mam-2 '(1 2)) (list a b c))
        (multiple-value-bind (a b) (apply #'%mam-4 1 '(2 3 4)) (list a b))
        (multiple-value-bind (a b) (apply '%mam-gf 1 2 nil) (list a b))
        (nth-value 1 (apply #'%mam-4 1 2 '(3 4)))
        (apply #'%mam-2 '(1 2)) (apply #'%mam-4 1 2 '(3 4)) (apply #'%mam-v '(1))
        (multiple-value-list (%mam-tail #'%mam-2 5 6))
        (multiple-value-bind (a b) (%mam-tail #'%mam-4 1 2 3 4) (list a b))
        (%mam-tail #'%mam-4 1 2 3 4))
  ((1 2 (1 2)) (1 2 (1 2)) (4 3 2 1) (4 3 2 1) (:v) (:v 1 2 3 4 5 6) (1 2 (1 2))
   (4 3) (2 1) 3 1 4 :v (5 6 (5 6)) (4 3) 4))

(deftest mv-apply-mode.errors
  (list (handler-case (multiple-value-bind (a b) (apply #'%mam-2 '(1)) (list a b))
          (program-error () :program-error))
        (handler-case (multiple-value-bind (a b) (apply 'no-such-function-mam '(1)) (list a b))
          (undefined-function () :undefined))
        (handler-case (multiple-value-bind (a b) (apply #'%mam-2 1 '(2 . 3)) (list a b))
          (error () :error)))
  (:program-error :undefined :error))

(defun %mam-loop (n)
  (declare (fixnum n))
  (let ((r nil) (l (list 1 2 3 4)))
    (dotimes (i n r)
      (multiple-value-bind (a b) (apply #'%mam-4 l) (setq r (and a b)))
      (multiple-value-bind (a b) (apply #'%mam-4 1 (cdr l)) (setq r (and a b)))
      (setq r (apply #'%mam-4 l)))))

(deftest-compiled-only mv-apply-mode.allocates-nothing
  (< (bytes-per-op #'%mam-loop) 1)
  t)
