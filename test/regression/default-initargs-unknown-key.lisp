;;; A :DEFAULT-INITARGS key that names no slot initarg (and no keyword of an
;;; INITIALIZE-INSTANCE / SHARED-INITIALIZE method) is added to the call and so is an
;;; invalid initarg, whether or not the call supplied other initargs.

(defclass diuk-a () () (:default-initargs :x 1))
(defclass diuk-b () ((y :initarg :y)) (:default-initargs :x 1))
(defclass diuk-ok () ((x :initarg :x :reader diuk-x)) (:default-initargs :x 7))
(defclass diuk-c () ())
(defmethod initialize-instance :after ((o diuk-c) &key z) z)
(defclass diuk-d (diuk-c) () (:default-initargs :z 5))

(deftest default-initargs-unknown-key.errors
  (flet ((try (thunk) (handler-case (progn (funcall thunk) :ok) (error () :error))))
    (list (try (lambda () (make-instance 'diuk-a)))
          (try (lambda () (make-instance 'diuk-b)))
          (try (lambda () (make-instance 'diuk-b :y 1)))
          (try (lambda () (make-instance 'diuk-a :x 2)))))
  (:error :error :error :error))

(deftest default-initargs-unknown-key.valid-ones-still-work
  (list (diuk-x (make-instance 'diuk-ok))
        (diuk-x (make-instance 'diuk-ok :x 1))
        (typep (make-instance 'diuk-d) 'diuk-d))
  (7 1 t))
