;;; Two variables whose symbols have the same name in different packages are different variables. The
;;; compiler once took the boxed outer one for the inner one (it kept the set of boxed variables by
;;; name), so the outer variable was stored to as if it were not boxed and read back as an unset slot.
;;; An environment-taking macro call (which makes every variable in scope boxed) in the same scope
;;; showed it. The expected values are SBCL's.

(defpackage :%snopv-p2 (:use))
(defpackage :%snopv-p3 (:use :cl) (:shadow #:let))
(defmacro %snopv-p3::let (&whole call &rest stuff &environment env)
  (declare (ignorable call env))
  (cons 'cl:let stuff))

(defmacro %snopv-run (form) `(funcall (lambda () ,form)))

(deftest same-name-other-package-variable.macro-after
  (%snopv-run
   (cl:let* (key)
     (multiple-value-bind (more? %snopv-p2::key) (values t 1) (when more? (setq key %snopv-p2::key)))
     (%snopv-p3::let () (list (integerp key) key))))
  (t 1))

(deftest same-name-other-package-variable.macro-in-values-form
  (%snopv-run
   (cl:let* (key)
     (multiple-value-bind (more? %snopv-p2::key) (values t (%snopv-p3::let ((x 1)) x))
       (when more? (setq key %snopv-p2::key)))
     (list key)))
  (1))

(deftest same-name-other-package-variable.let-and-closure
  (%snopv-run
   (cl:let* (key (n 0))
     (cl:let ((%snopv-p2::key 10))
       (setq key (+ %snopv-p2::key 1)))
     (flet ((bump () (incf n) (setq key (+ key n))))
       (bump) (bump))
     (%snopv-p3::let () (list key n))))
  (14 2))

(deftest same-name-other-package-variable.hash-table-iterator
  (%snopv-run
   (cl:let* (key val items (lc (list nil)) (lst lc))
     (with-hash-table-iterator (nx (%snopv-p3::let ((x (make-hash-table))) (setf (gethash 'color x) 'brown) x))
       (tagbody ll
         (multiple-value-bind (more? %snopv-p2::key %snopv-p2::value) (nx)
           (unless more? (go end))
           (setq key %snopv-p2::key val %snopv-p2::value))
         (setq items (cons key val))
         (setq lc (setf (cdr lc) (cons items nil)))
         (go ll)
        end))
     (cdr lst)))
  ((color . brown)))
