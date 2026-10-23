;;; A function NOTINLINE in its own body, or proclaimed NOTINLINE, calls itself
;;; through its global definition (CLHS 3.2.2.3): a wrapper installed with
;;; (SETF FDEFINITION) sees every recursive call, tail calls included.

(defvar *nsc-calls* 0)

(defun nsc-fact (n)
  (declare (notinline nsc-fact))
  (if (<= n 1) 1 (* n (nsc-fact (1- n)))))

(defun nsc-count (n acc)
  (declare (notinline nsc-count))
  (if (= n 0) acc (nsc-count (1- n) (1+ acc))))

(defun nsc-fixnum (n)
  (declare (fixnum n) (notinline nsc-fixnum))
  (if (<= n 1) 1 (the fixnum (* n (the fixnum (nsc-fixnum (1- n)))))))

(defun nsc-double (x n)
  (declare (double-float x) (fixnum n) (notinline nsc-double))
  (if (= n 0) x (the double-float (+ 1d0 (the double-float (nsc-double x (1- n)))))))

(declaim (notinline nsc-global))
(defun nsc-global (n)
  (if (<= n 1) 1 (* n (nsc-global (1- n)))))

(defun nsc-counted (name thunk)
  (let ((orig (fdefinition name)))
    (setf (fdefinition name) (lambda (&rest args) (incf *nsc-calls*) (apply orig args)))
    (unwind-protect
         (let ((*nsc-calls* 0))
           (list (funcall thunk) *nsc-calls*))
      (setf (fdefinition name) orig))))

(deftest notinline-self-call.non-tail
  (nsc-counted 'nsc-fact (lambda () (nsc-fact 5)))
  (120 5))

(deftest notinline-self-call.tail
  (nsc-counted 'nsc-count (lambda () (nsc-count 5 0)))
  (5 6))

(deftest notinline-self-call.fixnum
  (nsc-counted 'nsc-fixnum (lambda () (nsc-fixnum 5)))
  (120 5))

(deftest notinline-self-call.double
  (nsc-counted 'nsc-double (lambda () (nsc-double 1d0 3)))
  (4d0 4))

(deftest notinline-self-call.proclaimed
  (nsc-counted 'nsc-global (lambda () (nsc-global 5)))
  (120 5))
