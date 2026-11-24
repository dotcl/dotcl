;;; A lambda list whose default form is a circular literal is defined and called.

(defun cdll-f (x &optional (z '#1=(a b . #1#)) &key (w '(#2=(q) #2#)))
  (list x (car z) (cadr z) (caddr z) (eq (car w) (cadr w))))

(defparameter *cdll-g*
  (lambda (&optional (z '#3=(1 . #3#))) (list (car z) (eq z (cdr z)))))

(deftest circular-default-lambda-list.defun
  (list (cdll-f 1) (cdll-f 2 '(x y z) :w nil))
  ((1 a b a t) (2 x y z t)))

(deftest circular-default-lambda-list.lambda
  (funcall *cdll-g*)
  (1 t))

(deftest circular-default-lambda-list.nested-lambda
  (list (funcall (lambda () (funcall (lambda (&optional (z '#4=(a . #4#))) (eq z (cdr z))))))
        (flet ((g (&optional (z '#5=(1 . #5#))) (car z))) (g)))
  (t 1))
