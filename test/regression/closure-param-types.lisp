;;; A closure that captures variables compiles its body with the type
;;; declarations of its own parameters and with the known types of the values
;;; it captures, as a function body does; an outer declaration of a name the
;;; closure binds as a parameter does not reach the parameter.

(setf dotcl:*save-sil* t)

(defun %cpt-shadow (y z)
  (declare (fixnum y))
  (list y (lambda (y) (list z (logand y 255)))))

(defun %cpt-params (w)
  (lambda (x n)
    (declare (single-float x) (fixnum n))
    (list w (* x 2f0) (logand n 255))))

(defun %cpt-captured (l)
  (destructuring-bind (h w) l
    (declare (type (unsigned-byte 32) h w))
    (lambda (x)
      (declare (single-float x))
      (- (* x w) (* 0.5f0 h)))))

(defun %cpt-captured-fixnum (k)
  (declare (fixnum k))
  (lambda (x) (declare (double-float x)) (* x (float k 1d0))))

(deftest closure-param-types.shadow
  (let ((f (second (%cpt-shadow 1 :z))))
    (list (funcall f (expt 2 70)) (funcall f 300)
          (handler-case (funcall f 1.5) (type-error () :type-error))))
  ((:z 0) (:z 44) :type-error))

(deftest closure-param-types.values
  (list (funcall (%cpt-params :w) 1.5f0 513)
        (handler-case (funcall (%cpt-params :w) 1.5d0 1) (type-error () :type-error))
        (funcall (%cpt-captured '(2 10)) 0.5f0)
        (funcall (%cpt-captured-fixnum 3) 0.5d0))
  ((:w 3.0 1) :type-error 4.0 1.5d0))

(deftest-compiled-only closure-param-types.raw
  (let ((p (%flat-text (dotcl:function-sil #'%cpt-params)))
        (c (%flat-text (dotcl:function-sil #'%cpt-captured)))
        (k (%flat-text (dotcl:function-sil #'%cpt-captured-fixnum))))
    (list (search "Runtime.Multiply" p) (search "Logand" p)
          (search "Runtime.Multiply" c) (search "Runtime.Subtract" c)
          (search "Runtime.Multiply" k) (search "FLOAT" k)))
  (nil nil nil nil nil nil))

(setf dotcl:*save-sil* nil)
