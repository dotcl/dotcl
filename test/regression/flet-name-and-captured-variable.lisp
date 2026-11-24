;;; A local function name is not a variable: inside a lambda, a variable of the same name
;;; from outside is still captured, whether FLET, LABELS or MACROLET names that function.

(defun %fnv-flet (f) (funcall (lambda () (flet ((f (x) x)) (list f (f 3))))))
(defun %fnv-labels (f)
  (funcall (lambda () (labels ((f (x) (if (> x 0) (f (1- x)) 0))) (list f (f 3))))))
(defun %fnv-other-name (f) (funcall (lambda () (flet ((g (x) x)) (list f (g 3))))))
(defun %fnv-flet-outside (f)
  (flet ((f (x) (* x 2))) (funcall (lambda () (list f (f 3))))))
(defun %fnv-call-with-var (f) (funcall (lambda () (flet ((f (x) (+ x 0))) (f f)))))
(defun %fnv-function-ref (f)
  (funcall (lambda () (flet ((f (x) (+ x 1))) (list f (mapcar #'f (list 1 2)))))))
(defun %fnv-nested (f)
  (funcall (lambda () (funcall (lambda () (flet ((f (x) (- x))) (list f (f 4))))))))
(defun %fnv-let-inside (f)
  (funcall (lambda () (flet ((f (x) x)) (let ((f 10)) (funcall (lambda () (list f (f 1)))))))))
(defun %fnv-macrolet (f)
  (funcall (lambda () (macrolet ((f (x) `(list ,x))) (list f (f 3))))))
(defun %fnv-assigned (f)
  (let ((g (lambda () (flet ((f (x) x)) (setq f (+ f 1)) (list f (f 3))))))
    (list (funcall g) f)))
(defun %fnv-two-functions (f g)
  (funcall (lambda () (flet ((f (x) (+ x 1)) (g (x) (* x 2))) (list f g (f 1) (g 2))))))

(deftest flet-name-and-captured-variable.flet
  (%fnv-flet 7) (7 3))
(deftest flet-name-and-captured-variable.labels
  (%fnv-labels 7) (7 0))
(deftest flet-name-and-captured-variable.different-name
  (%fnv-other-name 7) (7 3))
(deftest flet-name-and-captured-variable.flet-outside-lambda
  (%fnv-flet-outside 7) (7 6))
(deftest flet-name-and-captured-variable.function-called-with-variable
  (%fnv-call-with-var 7) 7)
(deftest flet-name-and-captured-variable.function-reference
  (%fnv-function-ref 7) (7 (2 3)))
(deftest flet-name-and-captured-variable.nested-lambdas
  (%fnv-nested 7) (7 -4))
(deftest flet-name-and-captured-variable.inner-let
  (%fnv-let-inside 7) (10 1))
(deftest flet-name-and-captured-variable.macrolet
  (%fnv-macrolet 7) (7 (3)))
(deftest flet-name-and-captured-variable.assigned
  (%fnv-assigned 7) ((8 3) 8))
(deftest flet-name-and-captured-variable.two-functions
  (%fnv-two-functions 7 8) (7 8 2 4))
