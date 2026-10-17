
(defclass %gnmf-a () ())
(defclass %gnmf-b (%gnmf-a) ())
(defclass %gnmf-c (%gnmf-b) ())

;; An auxiliary method has no next method, also when it runs inside an
;; :around method's CALL-NEXT-METHOD (whose own state has a next method).
(defvar *%gnmf-aux-log* nil)
(defgeneric %gnmf-aux2 (x y))
(defmethod %gnmf-aux2 ((x %gnmf-a) y) (list :a y))
(defmethod %gnmf-aux2 :before ((x %gnmf-a) y) (push (list :before y (next-method-p)) *%gnmf-aux-log*))
(defmethod %gnmf-aux2 :after ((x %gnmf-a) y) (push (list :after y (next-method-p)) *%gnmf-aux-log*))
(defmethod %gnmf-aux2 :around ((x %gnmf-c) y) (cons :around (call-next-method)))
(defgeneric %gnmf-aux-cnm (x))
(defmethod %gnmf-aux-cnm ((x %gnmf-a)) :a)
(defmethod %gnmf-aux-cnm :before ((x %gnmf-a)) (call-next-method))
(defmethod %gnmf-aux-cnm :around ((x %gnmf-c)) (call-next-method))

(deftest gf-next-method-frame.auxiliary-methods
  (loop repeat 3
        collect (let ((*%gnmf-aux-log* nil))
                  (list (%gnmf-aux2 (make-instance '%gnmf-b) 1)
                        (%gnmf-aux2 (make-instance '%gnmf-c) 2)
                        (funcall #'%gnmf-aux2 (make-instance '%gnmf-c) 3)
                        (reverse *%gnmf-aux-log*)
                        (handler-case (%gnmf-aux-cnm (make-instance '%gnmf-c))
                          (error () :error)))))
  (#1=((:a 1) (:around :a 2) (:around :a 3)
       ((:before 1 nil) (:after 1 nil) (:before 2 nil) (:after 2 nil) (:before 3 nil) (:after 3 nil))
       :error)
   #1# #1#))
