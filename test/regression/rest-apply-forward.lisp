;;; (APPLY F ... REST) with REST the &REST variable of the enclosing lambda takes the
;;; arguments from the lambda's own argument array. Results, argument order, multiple
;;; values, the list when it is also read, and a REST that is assigned are as before.

(defun %raf-compose (f g) (lambda (&rest args) (funcall f (apply g args))))
(defun %raf-wrap (g) (lambda (&rest args) (apply g args)))
(defun %raf-spread (g) (lambda (a &optional (b 2) &rest args) (apply g a b 99 args)))
(defun %raf-both (g) (lambda (&rest args) (list (length args) (apply g args) args)))
(defun %raf-setq (g) (lambda (&rest args) (setq args (cons 0 args)) (apply g args)))
(defun %raf-nested (g) (lambda (&rest args) (funcall (lambda () (apply g args)))))
(defun %raf-mv (g) (lambda (&rest args) (apply g args)))
(defun %raf-key (g) (lambda (x &rest args &key k &allow-other-keys) (list x k (apply g args))))

(deftest rest-apply-forward.values
  (list (funcall (%raf-compose #'1+ #'+) 1 2 3)
        (funcall (%raf-compose #'list #'list))
        (funcall (%raf-wrap #'list))
        (funcall (%raf-wrap #'list) 1)
        (funcall (%raf-wrap #'list) 1 2 3 4 5 6 7)
        (funcall (%raf-spread #'list) 1)
        (funcall (%raf-spread #'list) 1 5 7 8)
        (funcall (%raf-both #'+) 1 2 3)
        (funcall (%raf-setq #'list) 5 6)
        (funcall (%raf-nested #'list) 8 9)
        (multiple-value-list (funcall (%raf-mv #'floor) 7 2))
        (funcall (%raf-key #'list) 1 :k 2 :z 3))
  (7 (nil) nil (1) (1 2 3 4 5 6 7)
   (1 2 99) (1 5 99 7 8)
   (3 6 (1 2 3))
   (0 5 6) (8 9) (3 1)
   (1 2 (:k 2 :z 3))))

(deftest rest-apply-forward.errors
  (list (handler-case (funcall (%raf-wrap #'car) 1 2) (error () :error))
        (handler-case (funcall (%raf-wrap 5) 1) (error () :error))
        (funcall (%raf-wrap #'car) '(1)))
  (:error :error 1))

;;; The same diagnostics as the general APPLY where the forwarding does not apply.
(defun %raf-plain-list (g) (lambda (args) (apply g args)))
(defun %raf-last-not-list (g) (lambda (&rest args) (declare (ignore args)) (apply g 5)))
(defun %raf-rest-first (g) (lambda (a &rest args) (apply g a args)))

(deftest rest-apply-forward.other-shapes
  (list (funcall (%raf-plain-list #'list) '(1 2))
        (handler-case (funcall (%raf-plain-list #'list) 7) (error () :error))
        (handler-case (funcall (%raf-last-not-list #'list)) (error () :error))
        (handler-case (funcall (%raf-wrap 'no-such-function-xyz) 1) (error () :error))
        (funcall (%raf-wrap 'list) 1 2)
        (funcall (%raf-wrap (lambda (&rest r) (reverse r))) 1 2 3)
        (funcall (%raf-rest-first #'list) 1)
        (funcall (%raf-rest-first #'list) 1 2 3))
  ((1 2) :error :error :error (1 2) (3 2 1) (1) (1 2 3)))
