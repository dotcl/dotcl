;;; One-argument generic functions keep following method and class changes once the
;;; class of the first warm call is the one tried before the hash probe: the calls
;;; below warm a class first, then other classes, then change the methods or classes.

(defun %glf-warm (fn args)
  (dotimes (k 60) (dolist (a args) (funcall fn a))))

(defgeneric glf-num (x))
(defmethod glf-num ((x integer)) :integer)
(defmethod glf-num ((x t)) :t)

(deftest gf-dispatch-loose-front.fixnum-method-added
  (let ((args (list 3 'a (expt 2 70) "s")) (out '()))
    (%glf-warm #'glf-num (list 3))
    (%glf-warm #'glf-num args)
    (push (mapcar #'glf-num args) out)
    (defmethod glf-num ((x fixnum)) :fixnum)
    (%glf-warm #'glf-num args)
    (push (mapcar #'glf-num args) out)
    (defmethod glf-num ((x fixnum)) :fixnum2)
    (push (mapcar #'glf-num args) out)
    (remove-method #'glf-num (find-method #'glf-num nil (list (find-class 'fixnum))))
      (push (mapcar #'glf-num args) out)
    (push (mapcar (lambda (a) (apply #'glf-num (list a))) args) out)
    (nreverse out))
  ((:integer :t :integer :t)
   (:fixnum :t :integer :t)
   (:fixnum2 :t :integer :t)
   (:integer :t :integer :t)
   (:integer :t :integer :t)))

(defgeneric glf-fib (x))
(defmethod glf-fib ((x (eql 0))) 1)
(defmethod glf-fib ((x (eql 1))) 1)
(defmethod glf-fib (x) (+ (glf-fib (- x 1)) (glf-fib (- x 2))))

(deftest gf-dispatch-loose-front.eql-methods-change
  (let ((out '()))
    (push (glf-fib 15) out)
    (let ((m (defmethod glf-fib ((x (eql 2))) 10)))
      (push (glf-fib 15) out)
      (remove-method #'glf-fib m))
    (push (glf-fib 15) out)
    (defmethod glf-fib ((x (eql 1))) 2)
    (push (glf-fib 10) out)
    (defmethod glf-fib ((x (eql 1))) 1)
    (nreverse out))
  (987 4003 987 144))

(defclass glf-p () ())
(defclass glf-q () ())
(defclass glf-r (glf-p) ())
(defgeneric glf-who (x))
(defmethod glf-who ((x glf-p)) :p)
(defmethod glf-who ((x glf-q)) :q)
(defmethod glf-who ((x t)) :other)

(deftest gf-dispatch-loose-front.change-class
  (let ((x (make-instance 'glf-p)) (out '()))
    (%glf-warm #'glf-who (list x))
    (push (glf-who x) out)
    (change-class x 'glf-q)
    (push (glf-who x) out)
    (change-class x 'standard-object)
    (push (glf-who x) out)
    (change-class x 'glf-p)
    (push (glf-who x) out)
    (nreverse out))
  (:p :q :other :p))

(deftest gf-dispatch-loose-front.class-redefinition
  (let ((r (make-instance 'glf-r)) (out '()))
    (%glf-warm #'glf-who (list r 1))
    (push (glf-who r) out)
    (defclass glf-r (glf-q) ())
    (%glf-warm #'glf-who (list r))
    (push (glf-who r) out)
    (defclass glf-r (glf-p) ())
    (push (glf-who r) out)
    (defmethod glf-who ((x glf-r)) :r)
    (push (glf-who r) out)
    (nreverse out))
  (:p :q :p :r))

(defstruct glf-s)
(defgeneric glf-opt (x &optional y))
(defmethod glf-opt ((x glf-s) &optional y) (list :s y))
(defmethod glf-opt ((x t) &optional y) (list :t y))

(deftest gf-dispatch-loose-front.optional-and-arity
  (let ((s (make-glf-s)))
    (%glf-warm #'glf-opt (list s 1))
    (list (glf-opt s) (glf-opt s 2) (glf-opt 1) (glf-opt 1 2)
          (handler-case (funcall #'glf-opt) (program-error () :too-few))
          (handler-case (funcall #'glf-opt s 1 2) (program-error () :too-many))
          (progn
            (dolist (m (copy-list (dotcl-mop:generic-function-methods #'glf-opt)))
              (remove-method #'glf-opt m))
            (defgeneric glf-opt (x y))
            (defmethod glf-opt ((x glf-s) y) (list :s2 y))
            (handler-case (funcall #'glf-opt s) (program-error () :too-few)))
          (glf-opt s 3)))
  ((:s nil) (:s 2) (:t nil) (:t 2) :too-few :too-many :too-few (:s2 3)))

(defgeneric glf-mv (x))
(defmethod glf-mv ((x fixnum)) (values x (1+ x)))
(defmethod glf-mv ((x t)) (values :t :u))

(deftest gf-dispatch-loose-front.values
  (progn
    (%glf-warm #'glf-mv (list 1 'a))
    (list (multiple-value-list (glf-mv 5))
          (let ((v (glf-mv 5))) v)
          (multiple-value-bind (a b) (glf-mv 'a) (list a b))
          (multiple-value-list (funcall #'glf-mv 7))))
  ((5 6) 5 (:t :u) (7 8)))

;; The value mode a caller hands a generic function's method is not left behind
;; when the stack check in the generic function's entry signals.
(defgeneric glf-no-bottom (n))
(defmethod glf-no-bottom ((n integer)) (1+ (glf-no-bottom (1+ n))))
(defun glf-two () (values 1 2))

(deftest-emitting-only gf-dispatch-loose-front.mode-after-overflow
  (call-on-small-stack
   (lambda ()
     (list (handler-case (glf-no-bottom 0) (storage-condition () :overflow))
           (multiple-value-list (apply #'glf-two nil))
           (multiple-value-list (funcall 'glf-two))
           (multiple-value-list (glf-mv 3)))))
  (:overflow (1 2) (1 2) (3 4)))
