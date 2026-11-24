;;; (APPLY #'CALL-NEXT-METHOD required... rest) in a method whose parameters occur nowhere else passes on
;;; the arguments the method was called with, as (CALL-NEXT-METHOD) does; a body that changes, reads or
;;; hands on those parameters keeps the meaning it has. Expected values are SBCL's.

(defvar *dacm-log* nil)

(defclass dacm-a () ((x :initarg :x :initform :x0 :accessor dacm-x)
                     (y :initarg :y :initform :y0 :accessor dacm-y)))
(defclass dacm-b (dacm-a) ())
(defclass dacm-c (dacm-b) ())

;; the rewritten shape: required + &rest, nothing else touches them
(defmethod initialize-instance :around ((o dacm-a) &rest initargs)
  (push :a *dacm-log*)
  (apply #'call-next-method o initargs))
;; with &key
(defmethod initialize-instance :around ((o dacm-b) &rest initargs &key &allow-other-keys)
  (push :b *dacm-log*)
  (apply #'call-next-method o initargs))
;; not rewritten: the rest list is changed
(defmethod initialize-instance :around ((o dacm-c) &rest initargs &key &allow-other-keys)
  (push :c *dacm-log*)
  (setq initargs (list* :y :y-from-c initargs))
  (apply #'call-next-method o initargs))

(defun dacm-run (class &rest args)
  (setq *dacm-log* nil)
  (let ((o (apply #'make-instance class args)))
    (list (reverse *dacm-log*) (dacm-x o) (dacm-y o))))
(defun dacm-3 (class &rest args) (let (r) (dotimes (i 3) (setq r (apply #'dacm-run class args))) r))

(deftest defmethod-apply-call-next-method.initialize-instance
  (list (dacm-3 'dacm-a :x 1) (dacm-3 'dacm-b :x 2 :y 3) (dacm-3 'dacm-c :x 4))
  (((:a) 1 :y0) ((:b :a) 2 3) ((:c :b :a) 4 :y-from-c)))

;; ordinary generic functions: the same, with a next method and without one
(defgeneric dacm-f (a b &rest more))
(defmethod dacm-f ((a integer) b &rest more) (list :base a b more))
(defmethod dacm-f ((a (eql 1)) b &rest more) (list :one (apply #'call-next-method a b more)))
(defmethod dacm-f ((a (eql 2)) b &rest more) (list :two (apply #'call-next-method a b more) more))
(defmethod dacm-f ((a (eql 3)) b &rest more) (list :three (apply #'call-next-method a (1+ b) more)))
(defmethod dacm-f ((a (eql 4)) b &rest more) (list :four (apply #'call-next-method a b (cdr more))))
(defmethod dacm-f ((a (eql 5)) b &rest more)
  (flet ((go-on () (apply #'call-next-method a b more))) (list :five (go-on))))
(defgeneric dacm-g (a &rest more))
(defmethod dacm-g ((a integer) &rest more) (apply #'call-next-method a more))

(deftest defmethod-apply-call-next-method.generic-function
  (let (r)
    (dotimes (i 3)
      (setq r (list (dacm-f 1 10 :p :q) (dacm-f 2 10 :p) (dacm-f 3 10 :p) (dacm-f 4 10 :p :q) (dacm-f 5 10 :r)
                    (handler-case (dacm-g 1 :z) (error () :no-next-method)))))
    r)
  ((:one (:base 1 10 (:p :q))) (:two (:base 2 10 (:p)) (:p)) (:three (:base 3 11 (:p)))
   (:four (:base 4 10 (:q))) (:five (:base 5 10 (:r))) :no-next-method))

;; two required parameters in the wrong order is a change of arguments, not the same ones
(defgeneric dacm-h (a b &rest more))
(defmethod dacm-h ((a integer) (b integer) &rest more) (list :base a b more))
(defmethod dacm-h ((a (eql 1)) (b integer) &rest more) (list :swap (apply #'call-next-method a b more)))
(defmethod dacm-h ((a (eql 2)) (b integer) &rest more) (list :same (apply #'call-next-method a b more)))
(deftest defmethod-apply-call-next-method.order
  (list (dacm-h 1 7 :m) (dacm-h 2 7 :m))
  ((:swap (:base 1 7 (:m))) (:same (:base 2 7 (:m)))))
