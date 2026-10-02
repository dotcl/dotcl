;;; Regression: a &key default form that referred to the &rest variable failed
;;; to compile ("Undeclared local: R_2"): the &rest variable was bound after the
;;; &key variables, while CLHS 3.4.1 binds it first. cl-rte's TRAVERSE-PATTERN
;;; has (&rest functions &key (client (lambda (p) (apply ... functions))) ...).

(defun rvk-plain (&rest r &key (k (list :rest r)) z)
  (list k z))

(defun rvk-closure (x &rest fns &key (client (lambda (y) (list y (length fns)))) (g client))
  (funcall g x))

(deftest rest-visible-in-key-default.plain
  (list (rvk-plain) (rvk-plain :z 1) (rvk-plain :k 7))
  (((:rest nil) nil) ((:rest (:z 1)) 1) (7 nil)))

(deftest rest-visible-in-key-default.closure
  (list (rvk-closure 5) (rvk-closure 5 :g #'identity))
  ((5 0) 5))

(deftest rest-visible-in-key-default.after-optional
  (funcall (lambda (&optional (o 1) &rest r &key (k (list o r)) &allow-other-keys) k)
           2 :a 3)
  (2 (:a 3)))
