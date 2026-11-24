;;; A DEFMETHOD that fails because a specializer names a class that is not defined
;;; leaves no generic function behind; the same DEFMETHOD works once the class
;;; exists, and a failed one on an existing generic function leaves its methods alone.

(deftest-emitting-only defmethod-failure-leaves-no-gf.undefined-class
  (progn
    (ignore-errors (eval '(defmethod dfg-g ((x dfg-undefined)) 1)))
    (list (fboundp 'dfg-g) (and (fboundp 'dfg-g) (length (generic-function-methods #'dfg-g)))))
  (nil nil))

(deftest-emitting-only defmethod-failure-leaves-no-gf.second-specializer
  (progn
    (ignore-errors (eval '(defmethod dfg-h ((x integer) (y dfg-undefined)) 1)))
    (fboundp 'dfg-h))
  nil)

(deftest-emitting-only defmethod-failure-leaves-no-gf.eql-and-undefined
  (progn
    (ignore-errors (eval '(defmethod dfg-i ((x (eql 1)) (y dfg-undefined)) 1)))
    (fboundp 'dfg-i))
  nil)

(deftest-emitting-only defmethod-failure-leaves-no-gf.setf-name
  (progn
    (ignore-errors (eval '(defmethod (setf dfg-j) (v (x dfg-undefined)) v)))
    (fboundp '(setf dfg-j)))
  nil)

(deftest-emitting-only defmethod-failure-leaves-no-gf.works-after-class
  (progn
    (ignore-errors (eval '(defmethod dfg-k ((x dfg-later)) :later)))
    (eval '(defclass dfg-later () ()))
    (eval '(defmethod dfg-k ((x dfg-later)) :later))
    (list (length (generic-function-methods #'dfg-k)) (dfg-k (make-instance 'dfg-later))))
  (1 :later))

(deftest-emitting-only defmethod-failure-leaves-no-gf.existing-gf-unchanged
  (progn
    (eval '(defmethod dfg-m ((x integer)) :int))
    (ignore-errors (eval '(defmethod dfg-m ((x dfg-undefined)) :no)))
    (list (length (generic-function-methods #'dfg-m)) (dfg-m 1)))
  (1 :int))

(deftest-emitting-only defmethod-failure-leaves-no-gf.ordinary-still-creates
  (progn
    (eval '(defmethod dfg-n ((x integer)) :int))
    (list (fboundp 'dfg-n) (dfg-n 5)))
  (t :int))
