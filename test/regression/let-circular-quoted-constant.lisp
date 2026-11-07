;;; A circular constant quoted inside a LET body (also inside a quoted form, as a
;;; macro such as a test framework's IS leaves it) compiles and keeps its shape.

(defstruct lcq-point x y)

(deftest-emitting-only let-circular-quoted-constant.direct
  (let* ((f (compile nil '(lambda () (let ((r 1)) (list r '#1=(#1#))))))
         (v (funcall f)))
    (list (first v) (eq (second v) (first (second v)))))
  (1 t))

(deftest-emitting-only let-circular-quoted-constant.nested
  (let* ((f (compile nil '(lambda () (let ((r 1)) (list r '(list nil '#1=(#1#)))))))
         (v (funcall f)))
    (list (first v) (car (second v)) (eq (second (second (second v)))
                                          (first (second (second (second v)))))))
  (1 list t))

;; Quoted data that looks like an accessor call on the variable is not a use of it.
(deftest-emitting-only let-circular-quoted-constant.quoted-accessor-call
  (let ((f (compile nil '(lambda (p) (let ((s p)) (list (lcq-point-x s) '(lcq-point-y s)))))))
    (funcall f (make-lcq-point :x 3 :y 4)))
  (3 (lcq-point-y s)))
