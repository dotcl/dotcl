;;; MAKE-METHOD-LAMBDA returns two values, the method lambda and its initargs (AMOP), also
;;; from the default method.

(defgeneric %mmlv-gf (x))

(deftest make-method-lambda-values.default-method
  (let* ((m (make-instance 'standard-method :lambda-list '(x)
                           :specializers (list (find-class t))
                           :function (lambda (a b) (declare (ignore a b)) 1)))
         (vals (multiple-value-list
                (dotcl-mop:make-method-lambda #'%mmlv-gf m '(lambda (x) x) nil))))
    (list (length vals) (consp (first vals)) (eq (car (first vals)) 'lambda) (second vals)))
  (2 t t nil))
