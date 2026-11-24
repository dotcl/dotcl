;;; An initarg named only in :DEFAULT-INITARGS is not a valid initarg of its own: no slot takes it and no
;;; INITIALIZE-INSTANCE / SHARED-INITIALIZE method has it as a keyword (CLHS 7.1.2), so passing it to
;;; MAKE-INSTANCE is an error. It was accepted.

(defclass dia-plain () () (:default-initargs :dia-x 1))
(defclass dia-slot () ((x :initarg :dia-x)) (:default-initargs :dia-x 1))
(defclass dia-method () () (:default-initargs :dia-foo 1))
(defmethod initialize-instance :after ((o dia-method) &key dia-foo) (declare (ignore dia-foo)) nil)
(defclass dia-aok () () (:default-initargs :allow-other-keys t))

(defun %dia-error-p (thunk) (handler-case (progn (funcall thunk) nil) (error () t)))

(deftest default-initargs-validity.not-a-valid-initarg
  (list (%dia-error-p (lambda () (make-instance 'dia-plain :dia-x 2)))
        (%dia-error-p (lambda () (make-instance 'dia-slot :dia-x 2)))
        (%dia-error-p (lambda () (make-instance 'dia-method :dia-foo 2)))
        (%dia-error-p (lambda () (make-instance 'dia-aok :anything 2))))
  (t nil nil nil))

(deftest default-initargs-validity.defaults-still-apply
  (list (slot-value (make-instance 'dia-slot) 'x) (slot-value (make-instance 'dia-slot :dia-x 5) 'x))
  (1 5))
