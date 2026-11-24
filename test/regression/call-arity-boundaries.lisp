;;; Calls at the arity boundaries: 4, 5, 8 and 9 required parameters, &optional and &rest mixed in,
;;; and the error for the wrong number of arguments.

(defun %cab-f4 (a0 a1 a2 a3) (list a0 a1 a2 a3))
(defun %cab-f5 (a0 a1 a2 a3 a4) (list a0 a1 a2 a3 a4))
(defun %cab-f8 (a0 a1 a2 a3 a4 a5 a6 a7) (list a0 a1 a2 a3 a4 a5 a6 a7))
(defun %cab-f9 (a0 a1 a2 a3 a4 a5 a6 a7 a8) (list a0 a1 a2 a3 a4 a5 a6 a7 a8))

(defun %cab-opt5 (a b c d &optional (e 5) f) (list a b c d e f))
(defun %cab-opt8 (a b c d e f g &optional (h 8)) (list a b c d e f g h))
(defun %cab-rest5 (a b c d e &rest r) (list a b c d e r))
(defun %cab-rest9 (a b c d e f g h i &rest r) (list a b c d e f g h i r))

(defun %cab-call-direct ()
  (list (%cab-f4 1 2 3 4) (%cab-f5 1 2 3 4 5) (%cab-f8 1 2 3 4 5 6 7 8) (%cab-f9 1 2 3 4 5 6 7 8 9)))

(defun %cab-call-funcall ()
  (list (funcall #'%cab-f4 1 2 3 4) (funcall #'%cab-f5 1 2 3 4 5)
        (funcall #'%cab-f8 1 2 3 4 5 6 7 8) (funcall #'%cab-f9 1 2 3 4 5 6 7 8 9)
        (apply #'%cab-f9 1 2 3 4 5 6 7 8 '(9))))

(deftest call-arity.direct
  (%cab-call-direct)
  ((1 2 3 4) (1 2 3 4 5) (1 2 3 4 5 6 7 8) (1 2 3 4 5 6 7 8 9)))

(deftest call-arity.funcall
  (%cab-call-funcall)
  ((1 2 3 4) (1 2 3 4 5) (1 2 3 4 5 6 7 8) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)))

(deftest call-arity.optional-and-rest
  (list (%cab-opt5 1 2 3 4) (%cab-opt5 1 2 3 4 50) (%cab-opt5 1 2 3 4 50 60)
        (%cab-opt8 1 2 3 4 5 6 7) (funcall #'%cab-opt8 1 2 3 4 5 6 7 80)
        (%cab-rest5 1 2 3 4 5) (%cab-rest5 1 2 3 4 5 6 7)
        (%cab-rest9 1 2 3 4 5 6 7 8 9) (funcall #'%cab-rest9 1 2 3 4 5 6 7 8 9 10))
  ((1 2 3 4 5 nil) (1 2 3 4 50 nil) (1 2 3 4 50 60)
   (1 2 3 4 5 6 7 8) (1 2 3 4 5 6 7 80)
   (1 2 3 4 5 nil) (1 2 3 4 5 (6 7))
   (1 2 3 4 5 6 7 8 9 nil) (1 2 3 4 5 6 7 8 9 (10))))

(defmacro %cab-errors (&rest forms)
  `(list ,@(mapcar (lambda (f) `(handler-case ,f (program-error () :program-error) (error () :other-error))) forms)))

(deftest call-arity.wrong-count-errors
  (%cab-errors (funcall #'%cab-f4 1 2 3) (funcall #'%cab-f4 1 2 3 4 5)
               (funcall #'%cab-f5 1 2 3 4) (funcall #'%cab-f5 1 2 3 4 5 6)
               (funcall #'%cab-f8 1 2 3 4 5 6 7) (funcall #'%cab-f8 1 2 3 4 5 6 7 8 9)
               (funcall #'%cab-f9 1 2 3 4 5 6 7 8) (funcall #'%cab-f9 1 2 3 4 5 6 7 8 9 10)
               (funcall #'%cab-opt5 1 2 3) (funcall #'%cab-opt5 1 2 3 4 5 6 7)
               (funcall #'%cab-rest5 1 2 3 4))
  (:program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error))
