;;; MAKE-INSTANCE of a structure class. A DEFSTRUCT slot takes the keyword of
;;; its name as initarg, as it does in SBCL, and slots not given one get their
;;; DEFSTRUCT default. jingoh builds its issue records this way.

(defstruct mis-point (x 10) y)
(defstruct (mis-point3 (:include mis-point)) (z (list :z)))

(deftest make-instance-struct-initargs
  (let ((p (make-instance 'mis-point :x 1 :y 2)))
    (list (mis-point-p p) (mis-point-x p) (mis-point-y p)))
  (t 1 2))

(deftest make-instance-struct-defaults
  (let ((p (make-instance 'mis-point :y 5)))
    (list (mis-point-x p) (mis-point-y p)))
  (10 5))

(deftest make-instance-struct-included-slots
  (let ((p (make-instance (find-class 'mis-point3) :x 1 :z 3)))
    (list (mis-point3-p p) (mis-point-x p) (mis-point-y p) (mis-point3-z p)))
  (t 1 nil 3))

(deftest make-instance-struct-equalp-constructor
  (equalp (make-instance 'mis-point :x 1 :y 2) (make-mis-point :x 1 :y 2))
  t)

(deftest make-instance-struct-unknown-initarg
  (handler-case (progn (make-instance 'mis-point :w 1) :no-error)
    (error () :error))
  :error)

(deftest make-instance-struct-allow-other-keys
  (mis-point-x (make-instance 'mis-point :w 1 :x 3 :allow-other-keys t))
  3)
