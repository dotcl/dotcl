;;; A call in tail position to a closure (a function with its own environment) takes the same
;;; tail-call path as one to a plain function without asking TailPrepCore each time. Proper tail
;;; calls, values, units kept alive and errors are as before.

(defun %tcc-count-down (n)
  (let ((f nil))
    (setq f (lambda (k) (if (zerop k) :done (funcall f (1- k)))))
    (funcall f n)))

(defun %tcc-mk (next) (declare (function next)) (lambda (x) (funcall next x)))
(defun %tcc-mk-id () (lambda (x) x))
(defun %tcc-chain (n) (let ((c (%tcc-mk-id))) (dotimes (i n c) (setq c (%tcc-mk c)))))
(defun %tcc-mv (next) (declare (function next)) (lambda (x) (funcall next x)))
(defun %tcc-mv-end () (lambda (x) (values x (1+ x) (+ x 2))))
(defun %tcc-mutual ()
  (let (ev od)
    (setq ev (lambda (n) (if (zerop n) t (funcall od (1- n)))) od (lambda (n) (if (zerop n) nil (funcall ev (1- n)))))
    (list (funcall ev 1000000) (funcall od 1000001))))

(deftest tail-call-closure-fast.proper-tail-calls
  (list (%tcc-count-down 3000000) (funcall (%tcc-chain 5000) 7) (%tcc-mutual))
  (:done 7 (t t)))

(deftest tail-call-closure-fast.values-pass-through
  (let ((c (%tcc-mv (%tcc-mv (%tcc-mv (%tcc-mv-end))))))
    (list (multiple-value-list (funcall c 5)) (multiple-value-list (multiple-value-call #'values (funcall c 1)))
          (funcall c 10) (multiple-value-bind (a b c2) (funcall c 100) (list a b c2))
          (nth-value 2 (funcall c 0))))
  ((5 6 7) (1 2 3) 10 (100 101 102) 2))

(deftest tail-call-closure-fast.other-units-and-eval
  (let* ((inner (eval '(let ((k 10)) (lambda (x) (+ x k)))))
         (outer (%tcc-mk inner)) (outer2 (%tcc-mk outer))
         (viaeval (eval `(lambda (y) (funcall ,outer2 y)))))
    (list (funcall outer 1) (funcall outer2 2) (funcall viaeval 3)
          (loop repeat 20000 sum (funcall viaeval 1))))
  (11 12 13 220000))

(deftest tail-call-closure-fast.errors-and-arity
  (let ((c (%tcc-chain 3)))
    (list (handler-case (funcall c) (error () :arity-error))
          (handler-case (funcall c 1 2) (error () :arity-error))
          (handler-case (funcall (%tcc-mk (lambda (x) (error "boom ~A" x))) 5) (error (e) (princ-to-string e)))
          (funcall c :ok)))
  (:arity-error :arity-error "boom 5" :ok))

(defvar *tcc-special* 1)
(deftest tail-call-closure-fast.specials-and-unwinding
  (let* ((c (%tcc-mk (lambda (x) (+ x *tcc-special*)))))
    (list (funcall c 1) (let ((*tcc-special* 100)) (funcall c 1))
          (catch 'k (funcall (%tcc-mk (lambda (x) (throw 'k (* x 2)))) 21))
          (block b (funcall (%tcc-mk (lambda (x) (return-from b (list :out x)))) 4))
          (funcall c 1)))
  (2 101 42 (:out 4) 2))
