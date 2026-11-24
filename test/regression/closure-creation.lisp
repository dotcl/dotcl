;;; Closures made by code compiled in memory, with every shape of lambda list:
;;; the values they return, their identity, and what a creation costs.

(defun %cc-make-fixed (e) (lambda (a b) (list a b e)))
(defun %cc-make-opt (e) (lambda (a &optional (b 7 b-p)) (list a b b-p e)))
(defun %cc-make-key (e) (lambda (&key (x 1) (y 2 y-p)) (list x y y-p e)))
(defun %cc-make-rest (e) (lambda (a &rest r) (list* a e r)))
(defun %cc-make-rest-key (e) (lambda (&rest r &key k &allow-other-keys) (list e k r)))
(defun %cc-make-counter () (let ((n 0)) (lambda () (incf n))))
(defun %cc-make-pair ()
  (let ((n 0))
    (list (lambda () (incf n)) (lambda () n) (lambda (v) (setq n v)))))
(defun %cc-fact (n) (labels ((f (k acc) (if (< k 2) acc (f (1- k) (* acc k))))) (f n 1)))
(defun %cc-sum-rest (&rest xs) (apply #'+ xs))

(deftest closure-creation.values
  (list (funcall (%cc-make-fixed 9) 1 2)
        (funcall (%cc-make-opt 9) 1)
        (funcall (%cc-make-opt 9) 1 2)
        (funcall (%cc-make-key 9))
        (funcall (%cc-make-key 9) :y 5)
        (funcall (%cc-make-rest 9) 1)
        (funcall (%cc-make-rest 9) 1 2 3)
        (apply (%cc-make-rest 9) 1 '(2 3 4))
        (funcall (%cc-make-rest-key 9) :k 3 :z 4)
        (apply (%cc-make-fixed 9) '(5 6)))
  ((1 2 9) (1 7 nil 9) (1 2 t 9) (1 2 nil 9) (1 5 t 9) (1 9) (1 9 2 3) (1 9 2 3 4)
   (9 3 (:k 3 :z 4)) (5 6 9)))

(deftest closure-creation.captured-variable-is-shared
  (let* ((c (%cc-make-counter)) (p (%cc-make-pair)))
    (list (funcall c) (funcall c) (funcall c)
          (funcall (first p)) (funcall (first p)) (funcall (second p))
          (funcall (third p) 100) (funcall (second p)) (funcall (first p))))
  (1 2 3 1 2 2 100 100 101))

(deftest closure-creation.each-creation-is-a-new-object
  (let ((a (%cc-make-counter)) (b (%cc-make-counter)) (r1 (%cc-make-rest 1)) (r2 (%cc-make-rest 1)))
    (funcall a) (funcall a)
    (list (eq a b) (funcall a) (funcall b) (eq r1 r2) (functionp r1)
          (eq r1 (identity r1)) (eq r1 (coerce r1 'function))))
  (nil 3 1 nil t t t))

(deftest closure-creation.arity-errors
  (list (handler-case (funcall (%cc-make-fixed 1) 1) (error () :error))
        (handler-case (funcall (%cc-make-fixed 1) 1 2 3) (error () :error))
        (handler-case (funcall (%cc-make-rest 1)) (error () :error))
        (handler-case (funcall (%cc-make-key 1) :nope 1) (error () :error)))
  (:error :error :error :error))

(deftest closure-creation.local-functions
  (list (%cc-fact 10)
        (%cc-sum-rest) (%cc-sum-rest 1 2 3)
        (funcall #'%cc-sum-rest 4 5)
        (flet ((g (x &rest more) (list x (length more)))) (list (g 1) (g 1 2 3)))
        (labels ((ev (n) (if (= n 0) t (od (1- n)))) (od (n) (if (= n 0) nil (ev (1- n))))) (ev 10))
        (mapcar (lambda (x) (* x x)) '(1 2 3))
        (let ((k 3)) (mapcar (lambda (x) (+ x k)) '(1 2 3))))
  (3628800 0 6 9 ((1 0) (1 2)) t (1 4 9) (4 5 6)))

;; Creating a closure allocates the closure object and its environment and nothing else:
;; before, a closure with a lambda list of more than required parameters also built a
;; delegate and its display class (about 100 bytes).
(defvar *cc-sink* nil)
(defun %cc-alloc-fixed (e) (lambda (a) (cons a e)))
(defun %cc-alloc-rest (e) (lambda (&rest r) (cons e r)))

(deftest-compiled-only closure-creation.allocation
  (list (< (min-allocated-bytes (lambda () (dotimes (i 1000) (setq *cc-sink* (%cc-alloc-fixed i)))) 5)
           (* 1000 200))
        (< (min-allocated-bytes (lambda () (dotimes (i 1000) (setq *cc-sink* (%cc-alloc-rest i)))) 5)
           (* 1000 200)))
  (t t))
