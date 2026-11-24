;;; (APPLY #'CALL-NEXT-METHOD required... rest) in a method body is not turned into (CALL-NEXT-METHOD)
;;; when the body calls a macro of its own: the macro can change or read a parameter by name without
;;; the name showing in the body. Expected values are SBCL's.

(defclass dacmh-k1 () ())
(defclass dacmh-k3 (dacmh-k1) ())
(defgeneric dacmh-g (a b &rest r))
(defmethod dacmh-g (a b &rest r) (list b r))

(defmacro dacmh-bump-b () `(setq b 50))
(defmacro dacmh-bump-r () `(setq r '(7)))
(defmacro dacmh-with-extra (&body body) `(progn (setq r (append r '(extra))) ,@body))

(defgeneric dacmh-hb (a b &rest r))
(defmethod dacmh-hb (a b &rest r) (list b r))
(defmethod dacmh-hb :around ((a dacmh-k3) b &rest r) (dacmh-bump-b) (apply #'call-next-method a b r))

(defgeneric dacmh-hr (a b &rest r))
(defmethod dacmh-hr (a b &rest r) (list b r))
(defmethod dacmh-hr :around ((a dacmh-k3) b &rest r) (dacmh-bump-r) (apply #'call-next-method a b r))

(defgeneric dacmh-an (a b &rest r))
(defmethod dacmh-an (a b &rest r) (list b r))
(defmethod dacmh-an :around ((a dacmh-k3) b &rest r) (dacmh-with-extra (apply #'call-next-method a b r)))

(deftest defmethod-apply-cnm-macro-hidden.changes-seen
  (let ((o (make-instance 'dacmh-k3)))
    (list (dacmh-hb o 1 2 3) (dacmh-hr o 1 2 3) (dacmh-an o 1 2 3)))
  ((50 (2 3)) (1 (7)) (1 (2 3 extra))))

(deftest defmethod-apply-cnm-macro-hidden.plain-still-works
  (progn
    (defmethod dacmh-g :around ((a dacmh-k3) b &rest r) (apply #'call-next-method a b r))
    (dacmh-g (make-instance 'dacmh-k3) 1 2 3))
  (1 (2 3)))
