;;; DEFGENERIC and DEFMETHOD with a malformed name, lambda list or option signal a
;;; PROGRAM-ERROR; the valid shapes still work.

(defmacro %dgs-pe (form)
  `(handler-case (progn (eval ',form) :no-error)
     (program-error () :program-error)
     (error () :other-error)))

(deftest defgeneric-shape.invalid
  (list (%dgs-pe (defgeneric %dgs-a1))
        (%dgs-pe (defgeneric 1 ()))
        (%dgs-pe (defgeneric %dgs-a2 (x x)))
        (%dgs-pe (defgeneric %dgs-a3 (&optional (x 1))))
        (%dgs-pe (defgeneric %dgs-a4 (&key (x 1))))
        (%dgs-pe (defgeneric %dgs-a5 (&aux x)))
        (%dgs-pe (defgeneric %dgs-a6 (&bogus)))
        (%dgs-pe (defgeneric %dgs-a7 (&key x &optional y)))
        (%dgs-pe (defgeneric %dgs-a8 (x) 1))
        (%dgs-pe (defgeneric %dgs-a9 (x) (:documentation)))
        (%dgs-pe (defgeneric %dgs-a10 (x) (:documentation 1)))
        (%dgs-pe (defgeneric %dgs-a11 (x) (:generic-function-class)))
        (%dgs-pe (defgeneric %dgs-a12 (x) (:method)))
        (%dgs-pe (defgeneric %dgs-a13 (x) (:method-combination standard) (:method-combination standard))))
  (:program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error))

(deftest defgeneric-shape.valid
  (list (progn (eval '(defgeneric %dgs-b1 (x &optional y &key z) (:documentation "d"))) t)
        (progn (eval '(defgeneric %dgs-b2 (x &optional (y) &key ((:k z)) (w) &allow-other-keys))) t)
        (progn (eval '(defgeneric %dgs-b3 () (:method () 1) (:method :around () (call-next-method)))) (%dgs-b3))
        (progn (eval '(defgeneric (setf %dgs-b4) (v x))) t))
  (t t 1 t))

(deftest defmethod-shape.invalid
  (list (%dgs-pe (defmethod %dgs-c1))
        (%dgs-pe (defmethod 1 (x) 1))
        (%dgs-pe (defmethod %dgs-c2 ((x)) 1))
        (%dgs-pe (defmethod %dgs-c3 ((x integer y)) 1))
        (%dgs-pe (defmethod %dgs-c4 ((x (eql))) 1))
        (%dgs-pe (defmethod %dgs-c5 ((x (eql 1 2))) 1))
        (%dgs-pe (defmethod %dgs-c6 (x x) 1))
        (%dgs-pe (defmethod %dgs-c7 (&bogus) 1))
        (%dgs-pe (defmethod %dgs-c8 (&key (x 1 2 3)) 1))
        (%dgs-pe (defmethod %dgs-c9 (&optional ((x integer))) 1))
        (%dgs-pe (defmethod %dgs-c10 (&rest) 1))
        (%dgs-pe (defmethod %dgs-c11 (&key x &optional y) 1)))
  (:program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error :program-error))

(deftest defmethod-shape.valid
  (list (progn (eval '(defmethod %dgs-d1 ((x integer) &optional (y 1 yp) &key ((:k kk) 2) &aux (x x)) (list x y yp kk)))
               (%dgs-d1 5))
        (progn (eval '(defmethod %dgs-d2 ((x (eql 3)) y) (list x y)))
               (eval '(defmethod %dgs-d2 :around ((x (eql 3)) y) (call-next-method)))
               (%dgs-d2 3 4))
        (progn (eval '(defmethod %dgs-d3 (x) x)) (%dgs-d3 9))
        (progn (eval '(defmethod %dgs-d4 ())) (%dgs-d4)))
  ((5 1 nil 2) (3 4) 9 nil))
