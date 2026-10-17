;;; A boxed binding (assigned and captured) that shadows an outer variable of
;;; the same name: its init form, or a parameter default, still reads the outer
;;; variable, which need not be boxed itself.

(defun %bbis-let* (subs)
  (let* ((subs (cons 's subs)))
    (funcall (lambda () (setf subs (list subs))))))

(defun %bbis-mvb (subs)
  (multiple-value-bind (p subs) (values 1 (cons 's subs))
    (setf subs (list subs))
    (list p (funcall (lambda () subs)))))

(defun %bbis-let*-later (x)
  (let* ((y (list x)) (x (cons y x)) (z (lambda () x)))
    (setf x (list x))
    (list y (funcall z))))

(defun %bbis-optional (x)
  (funcall (lambda (&optional (x (cons 'o x)))
             (setf x (list x))
             (funcall (lambda () x)))))

(defun %bbis-labels-optional (x)
  (labels ((g (&optional (x (cons 'o x)))
             (setf x (list x))
             (funcall (lambda () x))))
    (g)))

(defun %bbis-key (x)
  (funcall (lambda (&key (x (cons 'k x)))
             (setf x (list x))
             (funcall (lambda () x)))))

(defun %bbis-outer-boxed (x)
  (setf x (list x))
  (let ((g (lambda () x)))
    (funcall (lambda (&optional (x (cons 'o x)))
               (setf x (list x))
               (list (funcall g) (funcall (lambda () x)))))))

(deftest boxed-binding-init-scope.values
  (list (%bbis-let* '(s0)) (%bbis-mvb '(s0)) (%bbis-let*-later '(s0))
        (%bbis-optional '(s0)) (%bbis-labels-optional '(s0)) (%bbis-key '(s0))
        (%bbis-outer-boxed 's0))
  (((s s0)) (1 ((s s0))) (((s0)) ((((s0)) s0))) ((o s0)) ((o s0)) ((k s0))
   ((s0) ((o s0)))))
