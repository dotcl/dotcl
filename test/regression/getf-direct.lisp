;;; (GETF plist indicator [default]) is a direct call. Same values, same errors, same
;;; evaluation order as the call through the function object.

(defun %gd-get (pl k) (getf pl k))
(defun %gd-get3 (pl k d) (getf pl k d))

(deftest getf-direct.values
  (let ((pl (list :a 1 :b nil :c "s" 'sym :sym 5 :five)))
    (list (%gd-get pl :a) (%gd-get pl :b) (%gd-get pl :c) (%gd-get pl 'sym) (%gd-get pl 5) (%gd-get pl :zz)
          (%gd-get3 pl :b :dflt) (%gd-get3 pl :zz :dflt) (%gd-get3 nil :a 7) (%gd-get nil :a)
          (funcall #'getf pl :a) (funcall #'getf pl :zz 9) (apply #'getf (list pl :c))
          (let ((k 5)) (%gd-get pl k)) (%gd-get pl 5.0)))
  (1 nil "s" :sym :five nil nil :dflt 7 nil 1 9 "s" :five nil))

(deftest getf-direct.errors
  (list (handler-case (%gd-get '(:a) :a) (type-error () :type-error))
        (handler-case (%gd-get '(:a 1 :b) :zz) (type-error () :type-error))
        (handler-case (%gd-get 5 :a) (type-error () :type-error))
        (handler-case (%gd-get3 '(:a 1 . 2) :zz 0) (type-error () :type-error))
        (handler-case (eval '(getf '(:a 1))) (program-error () :program-error))
        (handler-case (eval '(getf '(:a 1) :a 1 2)) (program-error () :program-error)))
  (:type-error :type-error :type-error :type-error :program-error :program-error))

(deftest getf-direct.order-and-setf
  (let ((log nil) (pl (list :a 1)))
    (flet ((n (x) (push x log) x))
      (list (getf (n pl) (n :a) (n :d)) (mapcar (lambda (x) (if (consp x) :plist x)) (reverse log))
            (progn (setf (getf pl :b) 2) (getf pl :b))
            (progn (incf (getf pl :a) 10) (getf pl :a))
            (getf pl :c (progn (push :late log) 0)) (car log))))
  (1 (:plist :a :d) 2 11 0 :late))
