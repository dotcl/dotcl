;;; FUNCALL of every kind of function object and designator, and closures that read plain, mutated
;;; (boxed) and shared captured variables: the call path takes a plain function as it is and sends every
;;; other object (a generic function, a funcallable instance, a symbol, a non-function) through the
;;; ordinary coercion.

(defgeneric fde-gf (x))
(defmethod fde-gf ((x integer)) (list :int x))
(defmethod fde-gf ((x string)) (list :str x))

(defclass fde-fi () ((tag :initarg :tag :accessor fde-tag)) (:metaclass dotcl-mop:funcallable-standard-class))
(defun fde-defun (a) (* a 2))

(defun fde-mk (a b)
  (let ((c 0))
    (list (lambda (x) (+ x a b))              ; plain captured variables
          (lambda (x) (incf c x))             ; a mutated (boxed) variable
          (lambda () c))))                    ; the same boxed variable, shared

(defun fde-call (f x) (declare (function f)) (funcall f x))

(deftest funcall-designators.kinds
  (let ((fi (make-instance 'fde-fi :tag 7)))
    (dotcl-mop:set-funcallable-instance-function fi (lambda (x) (list :fi (fde-tag fi) x)))
    (list (funcall (lambda (x) (+ x 1)) 1)
          (funcall #'fde-defun 4)
          (funcall 'fde-defun 5)
          (funcall #'fde-gf 3)
          (funcall (function fde-gf) "s")
          (funcall fi 9)
          (fde-call #'fde-gf 4)
          (fde-call fi 2)
          (funcall (symbol-function 'fde-gf) 1)))
  (2 8 10 (:int 3) (:str "s") (:fi 7 9) (:int 4) (:fi 7 2) (:int 1)))

(deftest funcall-designators.non-function-is-a-type-error
  (list (handler-case (funcall 42 1) (type-error () :type-error) (error () :other))
        (handler-case (funcall nil 1) (error () :error))
        (handler-case (funcall "x" 1) (type-error () :type-error) (error () :other)))
  (:type-error :error :type-error))

(deftest funcall-designators.env-slots
  (destructuring-bind (add bump get) (fde-mk 10 20)
    (list (funcall add 1) (funcall bump 5) (funcall bump 6) (funcall get)))
  (31 5 11 11))
