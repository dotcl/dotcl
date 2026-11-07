;;; The keyword constructor of a (:TYPE VECTOR) or (:TYPE LIST) structure, called with constant
;;; keywords, builds the vector or list at the call site instead of going through the &key call. The
;;; result, the order the argument forms are evaluated in, and the slot type checks are those of the
;;; constructor itself (reached here through FUNCALL, which never takes the call-site path).

(defstruct (tsk-v (:type vector) :named) (x 0 :type fixnum) (y 10) (z 20))
(defstruct (tsk-vu (:type vector)) a (b 5))
(defstruct (tsk-vo (:type vector) :named (:initial-offset 2)) (m 1) (n 2))
(defstruct (tsk-l (:type list) :named) (x 0) (y 1))
(defstruct (tsk-lc (:type list) :named) (x (list 1)) (y 2))     ; a non-constant default
(defstruct (tsk-vp (:type vector) :named) (a 0) (b 1))
(defstruct (tsk-vc (:include tsk-vp) (:type vector) :named) (c 2))

(defvar *tsk-order* nil)
(defun tsk-note (x) (push x *tsk-order*) x)

(defun %tsk-same (direct via-funcall)
  (and (equalp direct via-funcall) (equal (type-of direct) (type-of via-funcall))))

(deftest typed-struct-keyword-ctor.same-as-the-constructor
  (list (%tsk-same (make-tsk-v :x 1 :y 2 :z 3) (funcall #'make-tsk-v :x 1 :y 2 :z 3))
        (%tsk-same (make-tsk-v :z 3 :x 1) (funcall #'make-tsk-v :z 3 :x 1))
        (%tsk-same (make-tsk-v) (funcall #'make-tsk-v))
        (%tsk-same (make-tsk-vu :a 1) (funcall #'make-tsk-vu :a 1))
        (%tsk-same (make-tsk-vo :n 9) (funcall #'make-tsk-vo :n 9))
        (%tsk-same (make-tsk-l :y 5) (funcall #'make-tsk-l :y 5))
        (%tsk-same (make-tsk-lc :y 5) (funcall #'make-tsk-lc :y 5))
        (%tsk-same (make-tsk-vc :a 1 :c 3) (funcall #'make-tsk-vc :a 1 :c 3))
        (coerce (make-tsk-v :x 1 :y 2 :z 3) 'list)
        (make-tsk-l :x 'a))
  (t t t t t t t t (tsk-v 1 2 3) (tsk-l a 1)))

(deftest typed-struct-keyword-ctor.argument-order
  (progn
    (setq *tsk-order* nil)
    (make-tsk-v :z (tsk-note :z) :x (tsk-note 0) :y (tsk-note :y))
    (list (reverse *tsk-order*)))
  ((:z 0 :y)))

(deftest typed-struct-keyword-ctor.slot-type
  (flet ((err (thunk) (handler-case (progn (funcall thunk) :no-error) (error () :error))))
    (list (err (lambda () (make-tsk-v :x "no")))
          (err (lambda () (funcall #'make-tsk-v :x "no")))
          (err (lambda () (make-tsk-v :x 1)))))
  (:error :error :no-error))

(deftest typed-struct-keyword-ctor.not-constant-keys
  ;; Keys that are not literal keywords, and unknown keys, take the ordinary call.
  (let ((k :x))
    (flet ((err (thunk) (handler-case (progn (funcall thunk) :no-error) (error () :error))))
      (list (coerce (make-tsk-v k 7) 'list)
            (err (lambda () (make-tsk-v :nosuch 1)))
            (coerce (apply #'make-tsk-v '(:y 4)) 'list))))
  ((tsk-v 7 10 20) :error (tsk-v 0 4 20)))
